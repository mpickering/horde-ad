{-# LANGUAGE CPP, DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes,
             StandaloneDeriving, UnboxedTuples #-}
-- | The second component of our rendition of dual numbers:
-- delta expressions, with their semantics.
-- Neel Krishnaswami calls them \"sparse vector expressions\",
-- and indeed even in the simplest case of an objective function
-- defined on scalars only, the codomain of the function that computes
-- gradients from such delta expressions is a set of vectors, because
-- the gradient of an @R^n@ to @R@ function is an @R^n@ vector.
--
-- The \'sparsity\' is less obvious when the domain of the function consists
-- of multiple vectors, matrices and tensors and when the expressions themselves
-- contain vectors, matrices and tensors. However, a single tiny delta
-- expression (e.g., a sum of two inputs) may denote a vector of matrices.
-- Even a delta expression containing a big matrix usually denotes something
-- much bigger: a whole vector of such matrices and more.
--
-- The algebraic structure here is an extension of vector space.
-- The crucial extra constructor of an input replaces the one-hot
-- access to parameters with something cheaper and more uniform.
-- A lot of the remaining additional structure is for introducing
-- and reducing dimensions (ranks).
--
-- This is an internal low-level API, while the module @DualClass@
-- is an intermediate mid-level API that generates 'NodeId' identifiers
-- and provides a generalization to other kinds of second components
-- of dual numbers, e.g., the same as primal component for fast computation
-- of forward derivatives (because @derivativeFromDelta@ below,
-- computing derivatives from delta-expressions, is slow once
-- the expressions grow large enough to affect cache misses).
module HordeAd.Internal.Delta
  ( -- * Abstract syntax trees of the delta expressions
    Delta0 (..), Delta1 (..)
  , -- * Delta expression identifiers
    NodeId(..), InputId, toInputId
  , -- * Evaluation of the delta expressions
    DeltaDt (..), Domain0, Domain1, Domains(..), nullDomains
  , gradientFromDelta, derivativeFromDelta
  ) where

import Prelude

import           Control.DeepSeq (NFData)
import           Control.Exception (assert)
import           Control.Monad (liftM2)
import           Control.Monad.ST.Strict (ST, runST)
import qualified Data.EnumMap.Strict as EM
import           Data.List.Index (ifoldl')
import           Data.Primitive (Prim)
import           Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.Generics (Generic)
import           Numeric.LinearAlgebra (Numeric, Vector, (<.>))
import qualified Numeric.LinearAlgebra as LA
import           Text.Show.Functions ()

-- * Abstract syntax trees of the delta expressions

-- | For each choice of the underlying scalar type @r@,
-- we have several primitive differentiable types based on the scalar:
-- the scalar type @r@ itself, @Vector r@ and (in the non-simplified
-- version of delta expressions) @Matrix r@ and tensors.
-- Many operations span the ranks and so span the datatypes, which makes
-- the datatypes mutually recursive. Later on in this module,
-- algorithms are implemented for computing gradients and for computing
-- derivatives of objective functions from which such delta expressions
-- are obtained via our dual number method.
--
-- The first pair of grammars given below are of delta-expressions
-- at tensor rank 0, that is, at the scalar level. The first few operations
-- have analogues at the level of vectors, matrices and arbitrary tensors,
-- but the other operations are specific to the rank.
--
-- The `NodeId` identifier that appears in a @Let0 n d@ expression
-- is the unique identity stamp of subterm @d@, that is, there is
-- no different term @e@ such that @Let0 n e@ appears in any delta
-- expression term in memory during the same run of an executable.
-- The subterm identity is used to avoid evaluating shared
-- subterms repeatedly in gradient and derivative computations.
-- The identifier also represents data dependencies among terms
-- for the purpose of gradient and derivative computation. Computation for
-- a term may depend only on data obtained from terms with lower value
-- of their node identifiers. Such data dependency determination
-- agrees with the subterm relation, but is faster than traversing
-- the term tree in order to determine the relation of terms.
-- There is one exception to the subterm data dependency rule:
-- any term containing a function (e.g., a @Build1@ node)
-- may depend on terms generated by applying the function,
-- regardless of their node identifiers (which in our implementation
-- are going to be larger than their ancestors').
--
-- When computing gradients, node identifiers are also used to index,
-- directly or indirectly, the data accumulated for each node,
-- in the form of cotangents, that is partial derivatives
-- of the objective function with respect to the position(s)
-- of the node in the whole objective function dual number term
-- (or, more precisely, with respect to the single node in the term DAG,
-- in which subterms with the same node identifier are collapsed).
-- Only the @Input@ nodes of all ranks have a separate data storage.
-- The per-rank `InputId` identifiers in the @Input@ term constructors
-- are indexes into contiguous vectors of contangents of exclusively @Input@
-- subterms of the whole term. The value at that index is the partial
-- derivative of the objective function (represented by the whole term,
-- or more precisely by (the data flow graph of) its particular
-- evaluation from which the delta expression originates)
-- with respect to the input parameter component at that index
-- in the objective function domain. The collection of all such
-- vectors of partial derivatives across all ranks is the gradient.
data Delta0 r =
    Zero0
  | Input0 (InputId r)
  | Scale0 r (Delta0 r)
  | Add0 (Delta0 r) (Delta0 r)
  | Let0 NodeId (Delta0 r)

  | SumElements10 (Delta1 r) Int  -- ^ see Note [SumElements10]
  | Index10 (Delta1 r) Int Int  -- ^ second integer is the length of the vector

  | Dot0 (Vector r) (Delta1 r)  -- ^ Dot0 v vd == SumElements10 (Scale1 v vd) n

deriving instance (Show r, Numeric r) => Show (Delta0 r)

-- | This is the grammar of delta-expressions at tensor rank 1, that is,
-- at vector level.
data Delta1 r =
    Zero1
  | Input1 (InputId (Vector r))
  | Scale1 (Vector r) (Delta1 r)
  | Add1 (Delta1 r) (Delta1 r)
  | Let1 NodeId (Delta1 r)

  | FromList1 [Delta0 r]
  | FromVector1 (Data.Vector.Vector (Delta0 r))  -- ^ "unboxing" conversion
  | Konst1 (Delta0 r) Int  -- ^ length; needed only for forward derivative
  | Append1 (Delta1 r) Int (Delta1 r)
      -- ^ second argument is the length of the first argument
  | Slice1 Int Int (Delta1 r) Int  -- ^ last integer is the length of argument

    -- unsorted and undocumented yet
  | Reverse1 (Delta1 r)
  | Build1 Int (Int -> Delta0 r)
      -- ^ the first argument is length; needed only for forward derivative

deriving instance (Show r, Numeric r) => Show (Delta1 r)

-- * Delta expression identifiers

newtype NodeId = NodeId {fromNodeId :: Int}
  deriving newtype (Enum, Prim)
    -- The Prim instance conversions take lots of time when old-time profiling,
    -- but are completely optimized away in normal builds.
    -- No Eq instance to limit hacks outside this module.

instance Show NodeId where
  show (NodeId n) = show n  -- to keep debug printouts readable

newtype InputId a = InputId Int
  deriving (Show, Enum)
    -- No Eq instance to limit hacks outside this module.

-- | Wrap non-negative (only!) integers in the `InputId` newtype.
toInputId :: Int -> InputId a
toInputId i = assert (i >= 0) $ InputId i


-- * Evaluation of the delta expressions

-- | Helper definitions to shorten type signatures. @Domains@, among other
-- roles, is the internal representation of domains of objective functions.
type Domain0 r = Vector r

type Domain1 r = Data.Vector.Vector (Vector r)

data Domains r = Domains
  { domains0 :: Domain0 r
  , domains1 :: Domain1 r
  }
  deriving (Show, Generic, NFData)

nullDomains :: Numeric r => Domains r -> Bool
nullDomains Domains{..} =
  V.null domains0 && V.null domains1

-- | The main input of the differentiation functions:
-- the delta expression to be differentiated and the dt perturbation
-- (small change) of the objective function codomain, for which we compute
-- the gradient.
data DeltaDt r =
    DeltaDt0 r (Delta0 r)
  | DeltaDt1 (Vector r) (Delta1 r)

-- | The state of evaluation. It consists of several maps.
-- The maps indexed by input identifiers and node identifiers
-- eventually store cotangents for their respective nodes.
-- The cotangents are built gradually during the evaluation,
-- by summing cotangent contributions.
--
-- Data invariant:
-- 1. keys dMap0 `intersect` keys dMap1 == mempty
-- 2. keys nMap == keys dMap0 `union` keys dMap1
-- 3. key `member` dMap0 == nMap!key is DeltaBinding0
-- 4. key `member` dMap1 == nMap!key is DeltaBinding1
data EvalState r = EvalState
  { iMap0 :: EM.EnumMap (InputId r) r
      -- ^ eventually, cotangents of objective function inputs of rank 0
      -- (finally copied to the vector representing the rank 0 portion
      -- of the gradient of the objective function);
      -- the identifiers need to be contiguous and start at 0
  , iMap1 :: EM.EnumMap (InputId (Vector r)) (Vector r)
      -- ^ eventually, cotangents of objective function inputs of rank 1;
      -- (eventually copied to the vector representing the rank 1 portion
      -- of the gradient of the objective function);
      -- the identifiers need to be contiguous and start at 0
  , dMap0 :: EM.EnumMap NodeId r
      -- ^ eventually, cotangents of non-input subterms of rank 0 indexed
      -- by their node identifiers
  , dMap1 :: EM.EnumMap NodeId (Vector r)
      -- ^ eventually, cotangents of non-input subterms of rank 1 indexed
      -- by their node identifiers
  , nMap  :: EM.EnumMap NodeId (DeltaBinding r)
      -- ^ nodes left to be evaluated
  }

-- | Nodes left to be evaluated.
-- We can't evaluate them at once, because their other shared copies
-- may still not be processed, so we'd not take advantage of the sharing
-- and not take into account the whole summed context when finally evaluating.
data DeltaBinding r =
    DeltaBinding0 (Delta0 r)
  | DeltaBinding1 (Delta1 r)

-- | Delta expressions naturally denote forward derivatives, as encoded
-- in function 'derivativeFromDelta'. However, we are usually more
-- interested in computing gradients, which is what @gradientFromDelta@ does.
-- The two functions are bound by the equation from Lemma 5 from the paper
-- "Provably correct, asymptotically efficient, higher-order reverse-mode
-- automatic differentiation":
--
-- > dt <.> derivativeFromDelta d ds = gradientFromDelta d dt <.> ds
--
-- where @\<.\>@ denotes generalized dot product (multiplying
-- all tensors element-wise and summing the results), @d@ is the top level
-- delta expression from translation of the objective function @f@ to dual
-- numbers, @ds@ belongs to the domain of @f@ and @dt@ to the codomain.
-- In other words, @ds@ is a perturbation (small change) of the arguments
-- of @f@, for which we compute the derivative, and @dt@ is a perturbation
-- of the result of @f@, for which we compute the gradient.
-- We omitted for clarity the @dim@ arguments that are
-- the lengths of vectors of the tensors in the domain of @f@.
--
-- Let's first discuss in detail the semantics of delta-expressions
-- in terms of forward derivatives, since it's more straightforward.
-- Let @r@ be the type of underlying scalars. Let @f@ be a mathematical
-- differentiable function that takes arguments (a collection
-- of fininte maps or vectors) of type @Domains r@ and produces
-- a single result of type @r@. Let a dual number counterpart
-- of @f@ applied to a fixed collection of parameters @P@
-- of type @Domains r@ be represented as a Haskell value @b@.
-- Let @d :: Delta0 r@ be the delta expression that is
-- the second component of @b@, let @ds@ belong to @Domains r@.
-- The semantics of @d@ is a linear function from @Domains r@
-- to @r@ that is the derivative of @f@ at point @P@
-- with respect to the perturbation @ds@. The mathematical formula
-- for the derivative follows straightforwardly the syntactic form
-- of the delta expression @d@ (see 'derivativeFromDelta').
--
-- Let's now describe the semantics of a delta expression @d@
-- as the gradient of @f@ at point @P@ with respect to a @dt@ that belongs
-- to @r@. Here the semantics of @d@ is a collection of finite maps
-- (vectors) @v0@, @v1@, ..., corresponding to @Domains r@.
-- The value of @vi@ at index @k@ is the partial derivative
-- of function @f@ at @P@ with respect to its parameter of type @ai@
-- residing at index @k@.
--
-- Function @gradientFromDelta@ computes the four vectors described above.
-- Requested lengths of the vectors are given in the first few arguments.
-- The delta expression to be evaluated, together with the @dt@ perturbation
-- value (usually set to @1@) is given in the @DeltaDt r@ parameter.
gradientFromDelta
  :: forall r. (Numeric r, Num (Vector r))
  => Int -> Int -> DeltaDt r
  -> Domains r
gradientFromDelta dim0 dim1 deltaDt =
  -- Create finite maps that hold values associated with inputs
  -- and with (possibly shared) term tree nodes.
  -- The former are initialized with dummy values so that it's cheap
  -- to check if any update has already been performed to a cell
  -- (allocating big vectors filled with zeros is too costly,
  -- especially if never used in an iteration, and adding to such vectors
  -- and especially using them as scaling factors is wasteful; additionally,
  -- it may not be easy to deduce the sizes of the vectors).
  let s0 =
        let iMap0 = EM.fromDistinctAscList
                    $ zip [toInputId 0 ..]
                          (replicate dim0 0)
                      -- 0 is the correct value; below is a dummy value
            iMap1 = EM.fromDistinctAscList
                    $ zip [toInputId 0 ..]
                          (replicate dim1 (V.empty :: Vector r))
            dMap0 = EM.empty
            dMap1 = EM.empty
            nMap = EM.empty
        in EvalState {..}

  -- Eval.
  in let EvalState{iMap0, iMap1} = buildFinMaps s0 deltaDt

  -- Extract results.
  in Domains (V.fromList $ EM.elems iMap0) (V.fromList $ EM.elems iMap1)
{-# SPECIALIZE gradientFromDelta
  :: Int -> Int -> DeltaDt Double -> Domains Double #-}

buildFinMaps :: forall r. (Numeric r, Num (Vector r))
             => EvalState r -> DeltaDt r -> EvalState r
buildFinMaps s0 deltaDt =
  -- The first argument is the evaluation state being modified,
  -- the second is the cotangent accumulator that will become an actual
  -- cotangent contribution when complete (see below for an explanation)
  -- and the third argument is the node itself.
  let eval0 :: EvalState r -> r -> Delta0 r -> EvalState r
      eval0 s !r = \case
        Zero0 -> s
        Input0 i -> s {iMap0 = EM.adjust (+ r) i $ iMap0 s}
        Scale0 k d -> eval0 s (k * r) d
        Add0 d e -> eval0 (eval0 s r d) r e
        Let0 n d ->
          -- In this context, by construction, @d@ is the dual component
          -- of a dual number term. Let's say that, at this point, evaluation
          -- considers position (node) p out of possibly multiple positions
          -- at which that dual number resides in the whole term tree
          -- of the dual number representation of the objective function.
          -- (Equivalently, considers edges p, one of many leading to the only
          -- node with identifier @n@ in the DAG representing the term).
          -- If so, the @r@ argument of @eval0@ is the cotangent
          -- contribution for position p, that is, the partial derivative
          -- of the objective function with respect to position p.
          --
          -- If there are indeed multiple such positions (the term is shared)
          -- then, over the course of evaluation, cotangent contributions
          -- of them all are gradually accumulated in the finite
          -- maps and eventually their total sum represents the total
          -- influence of the objective function's subcomputation
          -- (more precisely, subgraph of the data flow graph in question)
          -- corresponding to the shared term @Let0 n d@. This total
          -- influence over the objective function's behaviour is called
          -- in short the cotangent of the node identifier @n@.
          -- In other words, the cotangent of @n@ is the sum,
          -- over all positions (edges) q in the global delta-expression DAG
          -- that are a reference to node @n@, of the partial derivative
          -- of the objective function with respect to the subcomputation
          -- corresponding to @q@ (meaning, subcomputations denoted by
          -- Haskell terms whose dual components are @Let n ...@).
          --
          -- For @Input@ terms, the eventual lists of cotangents end up
          -- in the cells of the gradient vectors that are the final
          -- result of the evaluation.
          assert (case d of
                    Zero0 -> False
                    Input0{} -> False
                    Let0{} -> False  -- wasteful and nonsensical
                    _ -> True)
          $ case EM.lookup n $ nMap s of
              Just (DeltaBinding0 _) ->
                s {dMap0 = EM.adjust (+ r) n $ dMap0 s}
              Nothing ->
                s { nMap = EM.insert n (DeltaBinding0 d) $ nMap s
                  , dMap0 = EM.insert n r $ dMap0 s }
              _ -> error "buildFinMaps: corrupted nMap"

        SumElements10 vd n -> eval1 s (LA.konst r n) vd
        -- The general case is given as the last one below,
        -- but for a few constructors it's faster to inline @eval1@ instead.
        -- BTW, such an optimization doesn't really belong in the simplified
        -- horde-ad and no consistent benefit should be expected here.
        Index10 Zero1 _ _ -> s  -- shortcut
        Index10 (Input1 i) ix k ->
          let f v = if V.null v
                    then LA.konst 0 k V.// [(ix, r)]
                    else v V.// [(ix, v V.! ix + r)]
          in s {iMap1 = EM.adjust f i $ iMap1 s}
        Index10 (Let1 n d) ix k ->
          case EM.lookup n $ nMap s of
            Just (DeltaBinding1 _) ->
              let f v = v V.// [(ix, v V.! ix + r)]
              in s {dMap1 = EM.adjust f n $ dMap1 s}
                -- This would be an asymptotic optimization compared to
                -- the general case, if not for the non-mutable update,
                -- which implies copying the whole @v@ vector,
                -- so it's only several times faster (same allocation,
                -- but not adding to each cell of @v@).
            Nothing ->
              let v = LA.konst 0 k V.// [(ix, r)]
              in s { nMap = EM.insert n (DeltaBinding1 d) $ nMap s
                   , dMap1 = EM.insert n v $ dMap1 s }
            _ -> error "buildFinMaps: corrupted nMap"
        Index10 d ix k -> eval1 s (LA.konst 0 k V.// [(ix, r)]) d

        Dot0 v vd -> eval1 s (LA.scale r v) vd

      addToVector :: Vector r -> Vector r -> Vector r
      addToVector r = \v -> if V.null v then r else v + r
      eval1 :: EvalState r -> Vector r -> Delta1 r -> EvalState r
      eval1 s !r = \case
        Zero1 -> s
        Input1 i -> s {iMap1 = EM.adjust (addToVector r) i $ iMap1 s}
        Scale1 k d -> eval1 s (k * r) d
        Add1 d e -> eval1 (eval1 s r d) r e
        Let1 n d ->
          assert (case d of
                    Zero1 -> False
                    Input1{} -> False
                    Let1{} -> False  -- wasteful and nonsensical
                    _ -> True)
          $ case EM.lookup n $ nMap s of
              Just (DeltaBinding1 _) ->
                s {dMap1 = EM.adjust (+ r) n $ dMap1 s}
              Nothing ->
                s { nMap = EM.insert n (DeltaBinding1 d) $ nMap s
                  , dMap1 = EM.insert n r $ dMap1 s }
              _ -> error "buildFinMaps: corrupted nMap"

        FromList1 lsd -> ifoldl' (\s2 i d -> eval0 s2 (r V.! i) d) s lsd
          -- lsd is a list of scalar delta expressions
        FromVector1 lsd -> V.ifoldl' (\s2 i d -> eval0 s2 (r V.! i) d) s lsd
          -- lsd is a boxed vector of scalar delta expressions
        Konst1 d _n -> V.foldl' (\s2 r2 -> eval0 s2 r2 d) s r

        Append1 d k e -> eval1 (eval1 s (V.take k r) d) (V.drop k r) e
        Slice1 i n d len ->
          eval1 s (LA.konst 0 i V.++ r V.++ LA.konst 0 (len - i - n)) d

        Reverse1 d -> eval1 s (V.reverse r) d
        Build1 _n f -> V.ifoldl' (\s2 i r0 -> eval0 s2 r0 (f i)) s r

      evalFromnMap :: EvalState r -> EvalState r
      evalFromnMap s@EvalState{nMap, dMap0, dMap1} =
        case EM.maxViewWithKey nMap of
          Just ((n, b), nMap2) ->
            let s2 = s {nMap = nMap2}
                s3 = case b of
                  DeltaBinding0 d -> let r = dMap0 EM.! n
                                     in eval0 s2 r d
                  DeltaBinding1 d -> let r = dMap1 EM.! n
                                     in eval1 s2 r d
            in evalFromnMap s3
          Nothing -> s  -- loop ends

      s1 = case deltaDt of
        DeltaDt0 dt deltaTopLevel -> eval0 s0 dt deltaTopLevel
        DeltaDt1 dt deltaTopLevel -> eval1 s0 dt deltaTopLevel
  in evalFromnMap s1

{-# SPECIALIZE buildFinMaps
  :: EvalState Double -> DeltaDt Double -> EvalState Double #-}

-- Unlike @buildFinMaps@, the following is simpler written in ST
-- than with explicit passing of state, because changing the state here
-- is really an irritating side effect, while in @buildFinMaps@ it's building
-- the result. Perhaps this can be simplified completely differently.

-- | Forward derivative computation via forward-evaluation of delta-expressions
-- (which is surprisingly competitive to the direct forward method,
-- until the allocation of deltas gets large enough to affect cache hits).
-- This is the directional derivative, calculated for the point,
-- at which the delta expression was computed (which is the point
-- represented by the parameters of the objective function and used
-- to compute it's dual number result) and along the direction vector(s)
-- given in the last parameter called @ds@.
derivativeFromDelta
  :: forall r. (Numeric r, Num (Vector r))
  => Int -> Int -> Delta0 r -> Domains r -> r
derivativeFromDelta dim0 dim1 deltaTopLevel ds =
  runST $ buildDerivative dim0 dim1 deltaTopLevel ds

-- | This mimics 'buildFinMaps', but in reverse. Perhaps this can be
-- simplified, but the obvious simplest formulation does not honour sharing
-- and evaluates shared subexpressions repeatedly.
buildDerivative
  :: forall s r. (Numeric r, Num (Vector r))
  => Int -> Int -> Delta0 r -> Domains r
  -> ST s r
buildDerivative dim0 dim1 deltaTopLevel
                Domains{..} = do
  dMap0 <- newSTRef EM.empty
  dMap1 <- newSTRef EM.empty
  nMap <- newSTRef EM.empty
  let eval0 :: Delta0 r -> ST s r
      eval0 = \case
        Zero0 -> return 0
        Input0 (InputId i) ->
          if i < dim0
          then return $! domains0 V.! i
          else error "derivativeFromDelta.eval': wrong index for an input"
        Scale0 k d -> (k *) <$> eval0 d
        Add0 d e -> liftM2 (+) (eval0 d) (eval0 e)
        Let0 n d -> do
          -- This is too complex, but uses components already defined
          -- for initializeFinMaps and some of a similar code.
          nm <- readSTRef nMap
          case EM.lookup n nm of
            Just (DeltaBinding0 _) -> do
              dm <- readSTRef dMap0
              return $! dm EM.! n
            Nothing -> do
              r <- eval0 d
              nmNew <- readSTRef nMap
              dm <- readSTRef dMap0
              writeSTRef nMap $! EM.insert n (DeltaBinding0 d) nmNew
              writeSTRef dMap0 $! EM.insert n r dm
              return r
            _ -> error "buildDerivative: corrupted nMap"

        SumElements10 vd _n -> LA.sumElements <$> eval1 vd
        Index10 d ix _k -> flip (V.!) ix <$> eval1 d

        Dot0 vr vd -> (<.>) vr <$> eval1 vd

      eval1 :: Delta1 r -> ST s (Vector r)
      eval1 = \case
        Zero1 -> return 0
        Input1 (InputId i) ->
          if i < dim1
          then return $! domains1 V.! i
          else error "derivativeFromDelta.eval': wrong index for an input"
        Scale1 k d -> (k *) <$> eval1 d
        Add1 d e -> liftM2 (+) (eval1 d) (eval1 e)
        Let1 n d -> do
          nm <- readSTRef nMap
          case EM.lookup n nm of
            Just (DeltaBinding1 _) -> do
              dm <- readSTRef dMap1
              return $! dm EM.! n
            Nothing -> do
              r <- eval1 d
              nmNew <- readSTRef nMap
              dm <- readSTRef dMap1
              writeSTRef nMap $! EM.insert n (DeltaBinding1 d) nmNew
              writeSTRef dMap1 $! EM.insert n r dm
              return r
            _ -> error "buildDerivative: corrupted nMap"

        FromList1 lsd -> do
          l <- mapM eval0 lsd
          return $! V.fromList l
        FromVector1 lsd -> do
          v <- V.mapM eval0 lsd
          return $! V.convert v
        Konst1 d n -> flip LA.konst n <$> eval0 d
        Append1 d _k e -> liftM2 (V.++) (eval1 d) (eval1 e)
        Slice1 i n d _len -> V.slice i n <$> eval1 d

        Reverse1 d -> V.reverse <$> eval1 d
        Build1 n f -> do
          l <- mapM (eval0 . f) [0 .. n - 1]
          return $! V.fromList l

  eval0 deltaTopLevel

{- Note [SumElements10]
~~~~~~~~~~~~~~~~~~~~~~

The second argument of SumElements10 is the length of the vector
to be summed. Given that we sum a delta-expression representing
a vector, we can't call Vector.length on it, so the length needs
to be recorded in the constructor. Alternatively, it could be
recorded in the Delta1 argument to SumElements10. This is what
shaped tensors do at the type level, so for DeltaS the argument
would not be needed.

Sum of vector elements can be implemented using a delta-expression
primitive SumElements10 as well as without this primitive, referring
only to the primitive Index10:

https://github.com/Mikolaj/horde-ad/blob/d069a45773ed849913b5ebd0345153072f304fd9/src/HordeAd.Core.DualNumber.hs#L125-L143

which is confirmed by tests to be equivalent in three different
implementations:

https://github.com/Mikolaj/horde-ad/blob/d069a45773ed849913b5ebd0345153072f304fd9/test/TestSingleGradient.hs#L116-L128

and proved to be prohibitively slow in the two implementations
that don't use the SumElements10 primitive in benchmarks (despite
an ingenious optimization of the common case of Index10 applied to a input):

https://github.com/Mikolaj/horde-ad/blob/d069a45773ed849913b5ebd0345153072f304fd9/bench/BenchProdTools.hs#L178-L193
-}

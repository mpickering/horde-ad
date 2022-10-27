{-# LANGUAGE CPP, DataKinds, GADTs, GeneralizedNewtypeDeriving, KindSignatures,
             RankNTypes, StandaloneDeriving, UnboxedTuples #-}
-- | The second component of dual numbers, @Delta@, with its semantics.
-- Neel Krishnaswami calls it \"sparse vector expressions\",
-- and indeed even in the simplest case of an objective function
-- defined on scalars only, the codomain of the function that computes
-- gradients from such delta expressions is a set of vectors, because
-- the gradient of an @R^n@ to @R@ function is an @R^n@ vector.
--
-- The \'sparsity\' is less obvious when the domain of the function consists
-- of multiple vectors, matrices and tensors and when the expressions themselves
-- contain vectors, matrices and tensors. However, a single tiny delta
-- expression (e.g., a sum of two inputs) may denote a vector of matrices.
-- Even a delta expression containing a big matrix denotes something much
-- bigger: a whole vector of such matrices and more.
--
-- The algebraic structure here is an extension of vector space.
-- The crucial extra constructor of a input replaces the one-hot
-- access to parameters with something cheaper and more uniform.
-- A lot of the remaining additional structure is for introducing
-- and reducing dimensions (ranks).
--
-- This is an internal API now, superseded by "HordeAd.Core.DualClass"
-- that permits other kinds of second component of dual numbers,
-- e.g., the same as primal component, for fast computation
-- of forward derivatives (because @derivativeFromDelta@ below,
-- computing derivatives from delta-expressions, is slow once the expressions
-- grow large enough to affect cache misses).
module HordeAd.Internal.Delta
  ( -- * Abstract syntax trees of the delta expressions
    Delta0 (..), Delta0' (..)
  , Delta1 (..), Delta1' (..)
  , -- * Delta expression identifiers
    NodeId(..), InputId, toInputId, DeltaId
  , -- * Evaluation of the delta expressions
    DeltaDt (..), Domain0, Domain1, Domains
  , gradientFromDelta, derivativeFromDelta
  ) where

import Prelude

import           Control.Exception (assert)
import           Control.Monad (liftM2, unless)
import           Control.Monad.ST.Strict (ST, runST)
import qualified Data.EnumMap.Strict as EM
import           Data.Primitive (Prim)
import           Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           Numeric.LinearAlgebra (Numeric, Vector, (<.>))
import qualified Numeric.LinearAlgebra as HM

-- * Abstract syntax trees of the delta expressions

-- | This is the grammar of delta-expressions at tensor rank 0, that is,
-- at scalar level. The first few operations have analogues
-- at the level of vectors, matrices and arbitrary tensors.
--
-- For each choice of the underlying scalar type @r@,
-- we have several primitive differentiable types based on the scalar:
-- the scalar type @r@ itself, @Vector r@, @Matrix r@ and tensors.
-- Many operations span the ranks and so span the datatypes, which makes
-- the datatypes mutually recursive.
--
-- The identifier that is the first argument of @Delta0@ marks
-- the identity of a subterm as part of the whole global term tree.
--
-- The per-rank identifier that is the second argument of @Delta0@
-- is an index into a contigous vector of gradient or derivative components
-- (partial gradients/derivatives wrt that term's position?) corresponding
-- to subterms of that rank. There is no corresponding argument to the
-- @Zero0@ constructor, because the term not only does not contribute
-- to the derivative (similarly as @Input0@), but we are not even interested
-- in what the (partial) gradient for that subterm position would be.
data Delta0 r =
    Delta0 NodeId (Delta0' r)
  | Zero0
  | Input0 (InputId r)
data Delta0' r =
    Scale0 r (Delta0 r)
  | Add0 (Delta0 r) (Delta0 r)

  | SumElements0 (Delta1 r) Int  -- ^ see Note [SumElements0]
  | Index0 (Delta1 r) Int Int  -- ^ second integer is the length of the vector

  | Dot0 (Vector r) (Delta1 r)  -- ^ Dot0 v vd == SumElements0 (Scale1 v vd) n

deriving instance (Show r, Numeric r) => Show (Delta0 r)
deriving instance (Show r, Numeric r) => Show (Delta0' r)

-- | This is the grammar of delta-expressions at tensor rank 1, that is,
-- at vector level.
data Delta1 r =
    Delta1 NodeId (Delta1' r)
  | Zero1
  | Input1 (InputId (Vector r))
data Delta1' r =
    Scale1 (Vector r) (Delta1 r)
  | Add1 (Delta1 r) (Delta1 r)

  | Seq1 (Data.Vector.Vector (Delta0 r))  -- ^ "unboxing" conversion
  | Konst1 (Delta0 r) Int  -- ^ length; needed only for forward derivative
  | Append1 (Delta1 r) Int (Delta1 r)  -- ^ the length of the first argument
  | Slice1 Int Int (Delta1 r) Int  -- ^ last integer is the length of argument

    -- unsorted and undocumented yet
  | Reverse1 (Delta1 r)

deriving instance (Show r, Numeric r) => Show (Delta1 r)
deriving instance (Show r, Numeric r) => Show (Delta1' r)


-- * Delta expression identifiers

newtype NodeId = NodeId {fromNodeId :: Int}
  deriving (Eq, Enum, Prim)

instance Show NodeId where
  show (NodeId n) = show n  -- to keep debug printouts readable

newtype InputId a = InputId Int
  deriving (Show, Enum)
    -- No Eq instance to limit hacks outside this module.

-- | Wrap non-negative (only!) integers in the `InputId` newtype.
toInputId :: Int -> InputId a
toInputId i = assert (i >= 0) $ InputId i

newtype DeltaId a = DeltaId Int
  deriving (Show, Enum, Prim)
    -- No Eq instance to limit hacks outside this module.
    -- The Prim instance conversions take lots of time when old-time profiling,
    -- but are completely optimized away in normal builds.

-- The key property is that it preserves the phantom type.
succDeltaId :: DeltaId a -> DeltaId a
succDeltaId (DeltaId i) = DeltaId (succ i)


-- * Evaluation of the delta expressions

data DeltaDt r =
    DeltaDt0 r (Delta0 r)
  | DeltaDt1 (Vector r) (Delta1 r)

data DeltaBinding r =
    DeltaBinding0 (DeltaId r) (Delta0' r)
  | DeltaBinding1 (DeltaId (Vector r)) (Delta1' r)

-- | Helper definitions to shorten type signatures. @Domains@, among other
-- roles, is the internal representation of domains of objective functions.
type Domain0 r = Vector r

type Domain1 r = Data.Vector.Vector (Vector r)

type Domains r = (Domain0 r, Domain1 r)

-- | TODO: this single haddock is now outdated, because per-node
-- identities have replaces variables and so exploitation of sharing
-- in order to avoid duplicated computation can't be explained
-- using the common concept of variables and their valuations.
--
-- Delta expressions naturally denote forward derivatives,
-- as encoded in function 'derivativeFromDelta'. However, we are more
-- interested in computing gradients, which is what @gradientFromDelta@ does.
-- The two functions are bound by the equation from Lemma 5 from the paper
-- "Provably correct, asymptotically efficient, higher-order reverse-mode
-- automatic differentiation":
--
-- > dt <.> derivativeFromDelta ct d ds = gradientFromDelta ct d dt <.> ds
--
-- where @\<.\>@ denotes generalized dot product (multiplying
-- all tensors element-wise and summing the results),
-- @ct@ contains bindings of delta inputs and @d@ is the top level
-- delta expression from translation of the objective function @f@ to dual
-- numbers, @ds@ belongs to the domain of @f@ and @dt@ to the codomain.
-- We omitted for clarity the @dim0@, @dim1@, @dim2@ and @dimX@ arguments
-- that are the lengths of vectors of the tensors in the domain of @f@.
--
-- Intuitively, @ds@ is a tiny perturbation of the arguments of @f@,
-- for which we compute the derivative, that is, the induced change
-- in the result of @f@. Similarly, @dt@ is a tiny perturbation of the
-- result of @f@, for which we compute the gradient, that is, the change
-- of arguments of @f@ sufficient to cause the perturbation.
-- Note that the scaling factor @r@ in functions @eval*@ in @gradientFromDelta@
-- locally plays the role of @dt@, just as the argument @parameters@
-- in @eval*@ in @derivativeFromDelta@ corresponds to @ds@.
--
-- Let's first discuss in detail the semantics of delta-expressions
-- in terms of forward derivatives, since it's more straightforward.
-- Let @r@ be the type of underlying scalars. Let @f@ be a mathematical
-- differentiable function that takes a collection of type @C@
-- of arguments and produces a single result of type @r@.
-- Let a dual number counterpart of @f@ applied to a collection
-- of parameters @P@ of type @C@ be represented as a Haskell value @b@.
-- Let @d :: Delta0 r@ be the closed delta expression that is the second
-- component of @b@, let @ds@ belong to @C@. The semantics of @d@ is a linear
-- function from @C@ to @r@ that is the derivative of @f@ at point @P@
-- with respect to the perturbation @ds@. The mathematical formula
-- for the derivative follows straightforwardly the syntactic form
-- of the delta expression @d@ (see 'derivativeFromDelta').
--
-- Let's now describe the semantics of a delta expression @d@
-- as the gradient of @f@ at point @P@ with respect to a @dt@ that belongs
-- to @r@. Here the semantics of @d@ is a collection of four finite maps
-- (vectors) @v0@, @v1@, @v2@, @vX@, corresponding to @C@,
-- each map @vi@ taking indexes of type @DeltaId ai@ to values of type @ai@,
-- where @a0@ is @r@, @a1@ is @Vector r@, @a2@ is @Matrix r@
-- and @aX@ is the type of tensors of @r@.
-- The value of @vi@ at index @DeltaId k@ is the partial derivative
-- of function @f@ at @P@ with respect to its parameter of type @ai@.
-- The position of the @ai@ parameter is represented by @DeltaId k@
-- (in other words, the partial derivative is with respect to an input
-- quantity tagged with @DeltaId k@) and its value comes from @dt@.
--
-- Function @gradientFromDelta@ computes the four vectors described above.
-- Requested lengths of the vectors are given in the first few arguments.
-- The delta expression to be evaluated, together with the @dt@ perturbation
-- value (usually set to @1@) is given in the @DeltaDt r@ parameter.
gradientFromDelta
  :: (Eq r, Numeric r, Num (Vector r))
  => Int -> Int -> DeltaDt r
  -> Domains r
gradientFromDelta dim0 dim1 deltaDt =
  runST $ buildFinMaps dim0 dim1 deltaDt
{-# SPECIALIZE gradientFromDelta
  :: Int -> Int -> DeltaDt Double -> Domains Double #-}

buildFinMaps :: forall s r. (Eq r, Numeric r, Num (Vector r))
             => Int -> Int -> DeltaDt r
             -> ST s ( Vector  r
                     , Data.Vector.Vector (Vector r) )
buildFinMaps dim0 dim1 deltaDt = do
  -- Create finite maps that hold values associated with inputs
  -- and with (possibly shared) term tree nodes.
  -- The former are initialized with dummy values so that it's cheap
  -- to check if any update has already been performed to a cell
  -- (allocating big vectors filled with zeros is too costly,
  -- especially if never used in an iteration, and adding to such vectors
  -- and especially using them as scaling factors is wasteful; additionally,
  -- it may not be easy to deduce the sizes of the vectors).
  iMap0 <- newSTRef $ EM.fromDistinctAscList
                    $ zip [toInputId 0 ..]
                          (replicate dim0 0)
             -- 0 is the correct value; below is a dummy value
  iMap1 <- newSTRef $ EM.fromDistinctAscList
                    $ zip [toInputId 0 ..]
                          (replicate dim1 (V.empty :: Vector r))
  rMap0 <- newSTRef EM.empty
  rMap1 <- newSTRef EM.empty
  dMap <- newSTRef EM.empty
  -- Eval.
  let addToVector :: Vector r -> Vector r -> Vector r
      addToVector r = \v -> if V.null v then r else v + r
      eval0 :: r -> Delta0 r -> ST s ()
      eval0 _ Zero0 = return ()
      eval0 !r (Input0 i) = do
        iv <- readSTRef iMap0
        writeSTRef iMap0 $! EM.adjust (+ r) i iv
      eval0 !r (Delta0 n d) = do
        im <- readSTRef dMap
        case EM.lookup n im of
          Just (DeltaBinding0 did _) -> do
            rm <- readSTRef rMap0
            writeSTRef rMap0 $! EM.adjust (+ r) did rm
          Nothing -> do
            rm <- readSTRef rMap0
            let did = case EM.lookupMax rm of
                  Nothing -> DeltaId 0
                  Just (didOld, _) -> succDeltaId didOld
            writeSTRef dMap $! EM.insert n (DeltaBinding0 did d) im
            writeSTRef rMap0 $! EM.insert did r rm
          _ -> error "buildFinMaps: corrupted dMap"
      eval0' :: r -> Delta0' r -> ST s ()
      eval0' !r = \case
        Scale0 k d -> eval0 (k * r) d
        Add0 d e -> eval0 r e >> eval0 r d

        SumElements0 vd n -> eval1 (HM.konst r n) vd
--        Index0 (Input1 i) ix k | i >= 0 -> do
--          let f v = if V.null v
--                    then HM.konst 0 k V.// [(ix, r)]
--                    else v V.// [(ix, v V.! ix + r)]
--          VM.modify iMap1 f i
            -- This would be an asymptotic optimization compared to
            -- the general case below, if not for the non-mutable update,
            -- which involves copying the whole vector, so it's just
            -- several times faster (same allocation, but not adding vectors).
            -- TODO: does it make sense to extend this beyond @Input1@?
        Index0 d ix k -> eval1 (HM.konst 0 k V.// [(ix, r)]) d

        Dot0 v vd -> eval1 (HM.scale r v) vd

      eval1 :: Vector r -> Delta1 r -> ST s ()
      eval1 _ Zero1 = return ()
      eval1 !r (Input1 i) = do
        iv <- readSTRef iMap1
        writeSTRef iMap1 $! EM.adjust (addToVector r) i iv
      eval1 !r (Delta1 n d) = do
        im <- readSTRef dMap
        case EM.lookup n im of
          Just (DeltaBinding1 did _) -> do
            rm <- readSTRef rMap1
            writeSTRef rMap1 $! EM.adjust (+ r) did rm
          Nothing -> do
            rm <- readSTRef rMap1
            let did = case EM.lookupMax rm of
                  Nothing -> DeltaId 0
                  Just (didOld, _) -> succDeltaId didOld
            writeSTRef dMap $! EM.insert n (DeltaBinding1 did d) im
            writeSTRef rMap1 $! EM.insert did r rm
          _ -> error "buildFinMaps: corrupted dMap"
      eval1' :: Vector r -> Delta1' r -> ST s ()
      eval1' !r = \case
        Scale1 k d -> eval1 (k * r) d
        Add1 d e -> eval1 r e >> eval1 r d

        Seq1 lsd -> V.imapM_ (\i d -> eval0 (r V.! i) d) lsd
        Konst1 d _n -> V.mapM_ (`eval0` d) r
        Append1 d k e -> eval1 (V.drop k r) e >> eval1 (V.take k r) d
        Slice1 i n d len ->
          eval1 (HM.konst 0 i V.++ r V.++ HM.konst 0 (len - i - n)) d

        Reverse1 d -> eval1 (V.reverse r) d

  case deltaDt of
    DeltaDt0 dt deltaTopLevel -> eval0 dt deltaTopLevel
    DeltaDt1 dt deltaTopLevel -> eval1 dt deltaTopLevel

  let evalUnlessZero :: DeltaBinding r -> ST s ()
      evalUnlessZero (DeltaBinding0 did d) = do
        rm <- readSTRef rMap0
        let r = rm EM.! did
        unless (r == 0) $  -- a cheap optimization in case of scalars
          eval0' r d
      evalUnlessZero (DeltaBinding1 did d) = do
        rm <- readSTRef rMap1
        let r = rm EM.! did
        eval1' r d
      evalFromdMap :: ST s ()
      evalFromdMap = do
        im <- readSTRef dMap
        case EM.maxView im of
          Just (b, im2) -> do
            writeSTRef dMap $! im2
            evalUnlessZero b
            evalFromdMap
          Nothing -> return ()  -- loop ends
  evalFromdMap

  -- Extract results.
  iv0 <- readSTRef iMap0
  iv1 <- readSTRef iMap1
  return (V.fromList $ EM.elems iv0, V.fromList $ EM.elems iv1)
{-# SPECIALIZE buildFinMaps
  :: Int -> Int -> DeltaDt Double
  -> ST s ( Vector Double
          , Data.Vector.Vector (Vector Double) ) #-}

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

-- | This mimics 'initializeFinMaps', but in reverse. Perhaps this can be
-- simplified, but the obvious simplest formulation does not honour sharing
-- and evaluates shared subexpressions repeatedly.
buildDerivative
  :: forall s r. (Numeric r, Num (Vector r))
  => Int -> Int -> Delta0 r -> Domains r
  -> ST s r
buildDerivative dim0 dim1 deltaTopLevel
                (params0Init, params1Init) = do
  rMap0 <- newSTRef EM.empty
  rMap1 <- newSTRef EM.empty
  dMap <- newSTRef EM.empty
  let eval0 :: Delta0 r -> ST s r
      eval0 Zero0 = return 0
      eval0 (Input0 (InputId i)) =
        if i < dim0
        then return $! params0Init V.! i
        else error "derivativeFromDelta.eval': wrong index for an input"
      eval0 (Delta0 n d) = do
        -- This is too complex, but uses components already defined
        -- for initializeFinMaps and some of a similar code.
        im <- readSTRef dMap
        case EM.lookup n im of
          Just (DeltaBinding0 did _) -> do
            rm <- readSTRef rMap0
            return $! rm EM.! did
          Nothing -> do
            r <- eval0' d
            imNew <- readSTRef dMap
            rm <- readSTRef rMap0
            let did = case EM.lookupMax rm of
                  Nothing -> DeltaId 0
                  Just (didOld, _) -> succDeltaId didOld
            writeSTRef dMap $! EM.insert n (DeltaBinding0 did d) imNew
            writeSTRef rMap0 $! EM.insert did r rm
            return r
          _ -> error "buildDerivative: corrupted dMap"
      eval0' :: Delta0' r -> ST s r
      eval0' = \case
        Scale0 k d -> (k *) <$> eval0 d
        Add0 d e -> liftM2 (+) (eval0 d) (eval0 e)

        SumElements0 vd _n -> HM.sumElements <$> eval1 vd
        Index0 d ix _k -> flip (V.!) ix <$> eval1 d

        Dot0 vr vd -> (<.>) vr <$> eval1 vd

      eval1 :: Delta1 r -> ST s (Vector r)
      eval1 Zero1 = return 0
      eval1 (Input1 (InputId i)) =
        if i < dim1
        then return $! params1Init V.! i
        else error "derivativeFromDelta.eval': wrong index for an input"
      eval1 (Delta1 n d) = do
        im <- readSTRef dMap
        case EM.lookup n im of
          Just (DeltaBinding1 did _) -> do
            rm <- readSTRef rMap1
            return $! rm EM.! did
          Nothing -> do
            r <- eval1' d
            imNew <- readSTRef dMap
            rm <- readSTRef rMap1
            let did = case EM.lookupMax rm of
                  Nothing -> DeltaId 0
                  Just (didOld, _) -> succDeltaId didOld
            writeSTRef dMap $! EM.insert n (DeltaBinding1 did d) imNew
            writeSTRef rMap1 $! EM.insert did r rm
            return r
          _ -> error "buildDerivative: corrupted dMap"
      eval1' :: Delta1' r -> ST s (Vector r)
      eval1' = \case
        Scale1 k d -> (k *) <$> eval1 d
        Add1 d e -> liftM2 (+) (eval1 d) (eval1 e)

        Seq1 lsd -> do
          v <- V.mapM eval0 lsd
          return $! V.convert v
        Konst1 d n -> flip HM.konst n <$> eval0 d
        Append1 d _k e -> liftM2 (V.++) (eval1 d) (eval1 e)
        Slice1 i n d _len -> V.slice i n <$> eval1 d

        Reverse1 d -> V.reverse <$> eval1 d

  eval0 deltaTopLevel

{- Note [SumElements0]
~~~~~~~~~~~~~~~~~~~~~~

The second argument of SumElements0 is the length of the vector
to be summed. Given that we sum a delta-expression representing
a vector, we can't call Vector.length on it, so the length needs
to be recorded in the constructor. Alternatively, it could be
recorded in the Delta1 argument to SumElements0. This is what
shaped tensors do at the type level, so for DeltaS the argument
would not be needed.

Sum of vector elements can be implemented using a delta-expression
primitive SumElements0 as well as without this primitive, referring
only to the primitive Index0:

https://github.com/Mikolaj/horde-ad/blob/d069a45773ed849913b5ebd0345153072f304fd9/src/HordeAd.Core.DualNumber.hs#L125-L143

which is confirmed by tests to be equivalent in three different
implementations:

https://github.com/Mikolaj/horde-ad/blob/d069a45773ed849913b5ebd0345153072f304fd9/test/TestSingleGradient.hs#L116-L128

and proved to be prohibitively slow in the two implementations
that don't use the SumElements0 primitive in benchmarks (despite
an ingenious optimization of the common case of Index0 applied to a input):

https://github.com/Mikolaj/horde-ad/blob/d069a45773ed849913b5ebd0345153072f304fd9/bench/BenchProdTools.hs#L178-L193
-}

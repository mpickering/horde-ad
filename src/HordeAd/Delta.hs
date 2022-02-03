{-# LANGUAGE FlexibleContexts, GADTs, KindSignatures #-}
-- | The second component of dual numbers, @Delta@, with it's evaluation
-- function. Neel Krishnaswami calls that "sparse vector expressions",
-- and indeed the codomain of the evaluation function is a vector,
-- because the gradient of an @R^n@ to @R@ function is an @R^n@ vector.
--
-- The algebraic structure here is an extension of vector space.
-- The crucial extra constructor for variables is used both to represent
-- sharing in order to avoid exponential blowup and to replace the one-hot
-- functionality with something cheaper and more uniform.
module HordeAd.Delta
  ( Delta (..)
  , DeltaId (..)
  , DeltaBinding (..)
  , DeltaState (..)
  , evalBindings
  ) where

import Prelude

import           Control.Exception (assert)
import           Control.Monad (foldM, unless, void, zipWithM_)
import           Control.Monad.ST.Strict (ST, runST)
import           Data.Kind (Type)
import           Data.STRef
import qualified Data.Strict.IntMap as IM
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Strict.Vector.Autogen.Mutable as Data.Vector.Mutable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable.Mutable
import           Numeric.LinearAlgebra
  (Matrix, Numeric, Vector, asColumn, fromRows, konst, outer, rows, toRows)
import qualified Numeric.LinearAlgebra

data Delta :: Type -> Type where
  Zero :: Delta a
  Scale :: a -> Delta a -> Delta a
  Add :: Delta a -> Delta a -> Delta a
  Var :: DeltaId -> Delta a
  Dot :: Vector r -> Delta (Vector r) -> Delta r
  SumElements :: Delta (Vector r) -> Int -> Delta r
  Konst :: Delta r -> Delta (Vector r)
  Seq :: Data.Vector.Vector (Delta r) -> Delta (Vector r)
  DotL :: Matrix r -> Delta (Matrix r) -> Delta (Vector r)
  DotRowL :: Vector r -> Delta (Matrix r) -> Delta (Vector r)
  KonstL :: Delta (Vector r) -> Delta (Matrix r)
  SeqL :: Data.Vector.Vector (Delta (Vector r)) -> Delta (Matrix r)

newtype DeltaId = DeltaId Int
  deriving (Show, Eq)

data DeltaBinding r =
    DScalar (Delta r)
  | DVector (Delta (Vector r))
  | DMatrix (Delta (Matrix r))

data DeltaState r = DeltaState
  { deltaCounter  :: DeltaId
  , deltaBindings :: [DeltaBinding r]
  }

buildVector :: forall s r. (Eq r, Numeric r, Num (Vector r))
            => Int -> Int -> Int -> DeltaState r -> Delta r
            -> ST s ( Data.Vector.Storable.Mutable.MVector s r
                    , Data.Vector.Mutable.MVector s (Vector r)
                    , Data.Vector.Mutable.MVector s (Matrix r) )
buildVector dim dimV dimL st d0 = do
  let DeltaId storeSize = deltaCounter st
      dimSV = dim + dimV
      dimSVL = dim + dimV + dimL
  -- This is relatively very cheap allocation, so no problem even when most
  -- or all parameters and vars are inside vectors, matrices, etc.
  -- (and vectors and matrices are usually orders of magnitude less numerous
  -- than the sum total of individual parameters):
  store <- VM.replicate storeSize 0  -- correct value
  -- Here, for performance, we partially undo the nice unification
  -- of parameters and delta-variables. Fortunately, this is completely local.
  -- Allocating all these as boxed vectors would be very costly
  -- if most parameters are scalars and so most cells are unused,
  -- so we keep them in a sparse map, except for those that are guaranteed
  -- to be used, because they represent parameters:
  storeV <- VM.replicate dimV (V.empty :: Vector r)  -- dummy value
  storeL <- VM.replicate dimL (fromRows [] :: Matrix r)  -- dummy value
  intMapV <- newSTRef IM.empty
  intMapL <- newSTRef IM.empty
  let addToVector :: Int -> Vector r -> ST s ()
      {-# INLINE addToVector #-}
      addToVector i r =
        let addToStore = do  -- this saves almost nothing on MNIST 500 x500
              let j = i - dim
              v <- VM.read storeV j
              if V.null v
              then VM.write storeV j r
              else do
                vm <- V.unsafeThaw v
                VM.imapM_ (\k x -> VM.write vm k (x + (r V.! k))) vm
                void $ V.unsafeFreeze vm
            addToIntMap = do  -- this saves 3% allocations and runtime
                              -- but a well compiled zip would do better
                              -- and a single big sliced vector even better
              mv <- IM.lookup i <$> readSTRef intMapV
              case mv of
                Nothing -> modifySTRef' intMapV (IM.insert i r)
                Just v -> do
                  vm <- V.unsafeThaw v
                  VM.imapM_ (\k x -> VM.write vm k (x + (r V.! k))) vm
                  void $ V.unsafeFreeze vm
        in if i < dimSV
           then addToStore
           else addToIntMap
      addToMatrix :: Int -> Matrix r -> ST s ()
      {-# INLINE addToMatrix #-}
      addToMatrix i r =
        let addToStore v = if rows v <= 0 then r else v + r
            addToIntMap (Just v) = Just $ v + r
            addToIntMap Nothing = Just r
        in if i < dimSVL
           then VM.modify storeL addToStore (i - dimSV)
           else modifySTRef' intMapL (IM.alter addToIntMap i)
  let eval :: r -> Delta r -> ST s ()
      eval !r = \case
        Zero -> return ()
        Scale k d -> eval (k * r) d
        Add d1 d2 -> eval r d1 >> eval r d2
        Var (DeltaId i) -> VM.modify store (+ r) i
        Dot vr vd -> evalV (Numeric.LinearAlgebra.scale r vr) vd
        SumElements vd n -> evalV (konst r n) vd
        Konst{} -> error "buildVector: Konst can't result in a scalar"
        Seq{} -> error "buildVector: Seq can't result in a scalar"
        DotL{} -> error "buildVector: DotL can't result in a scalar"
        DotRowL{} -> error "buildVector: DotRowL can't result in a scalar"
        KonstL{} -> error "buildVector: KonstL can't result in a scalar"
        SeqL{} -> error "buildVector: SeqL can't result in a scalar"
      evalV :: Vector r -> Delta (Vector r) -> ST s ()
      evalV !r = \case
        Zero -> return ()
        Scale k d -> evalV (k * r) d
        Add d1 d2 -> evalV r d1 >> evalV r d2
        Var (DeltaId i) -> addToVector i r
        Dot{} -> error "buildVector: unboxed vectors of vectors not possible"
        SumElements{} ->
          error "buildVector: unboxed vectors of vectors not possible"
        Konst d -> V.mapM_ (`eval` d) r
        Seq vd -> V.imapM_ (\i d -> eval (r V.! i) d) vd
        DotL mr md -> evalL (asColumn r * mr) md
          -- this @asColumn@ interacted disastrously with @mr = asRow v@
          -- in @(#>!)@, each causing an allocation of a whole new @n^2@ matrix
          -- and then a third with their outer product;
          -- when doing the same computation by hand using @Vector@
          -- instead of @Matrix@, we can avoid even a single matrix allocation;
          -- the cost for the manual computation is many extra delta
          -- expressions which, however, with square enough matrices,
          -- don't dominate
        DotRowL row md -> evalL (r `outer` row) md
          -- this is a way to alleviate the ephemeral matrices problem,
          -- by polluting the API with the detail about the shape
          -- of the passed array (the replicated row shape),
          -- which eliminates two of the three matrix allocations;
          -- we could do even better keeping such matrices unevaluated
          -- and we could sometimes get away with modifying only the vectors
          -- but, e.g., @Scale@ forces allocation of a whole matrix regardless
      evalL :: Matrix r -> Delta (Matrix r) -> ST s ()
      evalL !r = \case
        Zero -> return ()
        Scale k d -> evalL (k * r) d
        Add d1 d2 -> evalL r d1 >> evalL r d2
        Var (DeltaId i) -> addToMatrix i r
        Dot{} -> error "buildVector: unboxed vectors of vectors not possible"
        SumElements{} ->
          error "buildVector: unboxed vectors of vectors not possible"
        KonstL d -> mapM_ (`evalV` d) (toRows r)
        SeqL md -> zipWithM_ evalV (toRows r) (V.toList md)
  eval 1 d0  -- dt is 1 or hardwired in f
  let evalUnlessZero :: DeltaId -> DeltaBinding r -> ST s DeltaId
      evalUnlessZero (DeltaId !i) (DScalar d) = do
        r <- store `VM.read` i
        unless (r == 0) $  -- we init with exactly 0 so the comparison is OK
          eval r d
        return $! DeltaId (pred i)
      evalUnlessZero (DeltaId !i) (DVector d) = do
        if i < dimSV then do
          r <- storeV `VM.read` (i - dim)
          unless (V.null r) $
            evalV r d
        else do
          mr <- IM.lookup i <$> readSTRef intMapV
          maybe (pure ()) (`evalV` d) mr
        return $! DeltaId (pred i)
      evalUnlessZero (DeltaId !i) (DMatrix d) = do
        if i < dimSVL then do
          r <- storeL `VM.read` (i - dimSV)
          unless (rows r <= 0) $
            evalL r d
        else do
          mr <- IM.lookup i <$> readSTRef intMapL
          maybe (pure ()) (`evalL` d) mr
        return $! DeltaId (pred i)
  minusOne <- foldM evalUnlessZero (DeltaId $ pred storeSize) (deltaBindings st)
  let _A = assert (minusOne == DeltaId (-1)) ()
  return (VM.slice 0 dim store, storeV, storeL)

evalBindings :: forall r. (Eq r, Numeric r, Num (Vector r))
             => Int -> Int -> Int -> DeltaState r -> Delta r
             -> ( Vector r
                , Data.Vector.Vector (Vector r)
                , Data.Vector.Vector (Matrix r) )
evalBindings dim dimV dimL st d0 =
  -- We can't just call @V.create@ thrice, because it would run
  -- the @ST@ action thrice.
  runST $ do
    (res, resV, resL) <- buildVector dim dimV dimL st d0
    r <- V.unsafeFreeze res
    rV <- V.unsafeFreeze resV
    rL <- V.unsafeFreeze resL
    return (r, rV, rL)

-- Note: we can't re-use the same three vectors all the time for the parameters,
-- because we need both the old parameters and the new gradient to compute
-- the new parameters. Double-buffering/cycling two sets of vectors
-- would work, but would be complex and both sets would not fit in cache
-- all the time, so it may even be cheaper to allocate them anew
-- than read from distant RAM and overwrite at once. Perhaps library ad does
-- something smart here, so worth a look.
--
-- It seems we can keep reusing the same vector if, for scalars, vectors
-- and matrices, when we update the parameters in the @Var@ cases of @eval@
-- we immediately multiply the increments by @gamma@. That may be what
-- library ad is doing in its @gradWith combine (f input) parameters@ calls.
-- However, for this we need to implement Adam and other gradient descent
-- schemes first, because already our @gdSmart@ gradient descent operation
-- uses both old and new values of parameters. Probably it could use only
-- the new values without much worse results, but other schemes may be
-- less forgiving. OTOH, library ad uses something like our @gdSmart@
-- and Python libraries require "zeroing gradients" so perhaps the popular
-- assumption is that old values of parameters should not be available after
-- gradient is computed?

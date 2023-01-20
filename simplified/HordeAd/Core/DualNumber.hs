{-# LANGUAGE CPP, DataKinds, FlexibleInstances, FunctionalDependencies, GADTs,
             MultiParamTypeClasses, QuantifiedConstraints, RankNTypes,
             TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module HordeAd.Core.DualNumber
  ( module HordeAd.Core.DualNumber
  , ADVal, dD, dDnotShared
  , ADMode(..), ADModeAndNum
  , IntOf, VectorOf
  , IsPrimal (..), IsPrimalAndHasFeatures, IsPrimalAndHasInputs, HasDelta
  , Element, HasPrimal(..)
  , VectorLike(..), ADReady
  , Domain0, Domain1, Domains(..), nullDomains  -- an important re-export
  , -- temporarily re-exported, until these are wrapped in sugar
    Ast(..), AstPrimalPart(..), AstVarName(..), AstVar(..)
  , AstInt(..), AstBool(..)
  , CodeOut(..), CodeIntOut(..), CodeBoolOut(..), RelOut(..)
  ) where

import Prelude

import           Control.Exception (assert)
import qualified Data.Array.DynamicS as OT
import qualified Data.Array.RankedS as OR
import           Data.IORef.Unboxed (Counter, atomicAddCounter_, newCounter)
import           Data.MonoTraversable (MonoFunctor (omap))
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Strict.IntMap as IM
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.TypeLits (KnownNat, Nat, natVal, type (+))
import           Numeric.LinearAlgebra (Numeric, Vector)
import qualified Numeric.LinearAlgebra as LA
import           System.IO.Unsafe (unsafePerformIO)

import HordeAd.Core.Ast
import HordeAd.Core.DualClass
import HordeAd.Internal.Delta
  (Domain0, Domain1, Domains (..), atIndexInTensorR, isTensorDummy, nullDomains)

-- * Auxiliary definitions

type Vec r = OR.Array 1 r

vecToV :: Numeric r => Vec r -> Vector r
vecToV = OR.toVector

vToVec :: Numeric r => Vector r  -> Vec r
vToVec v = OR.fromVector [V.length v] v

-- This is not needed in the simplified version, except for compilation
-- with the common test code.
-- | Sizes of tensor dimensions, of batches, etc., packed for passing
-- between functions as witnesses of type variable values.
data SNat (n :: Nat) where
  MkSNat :: KnownNat n => SNat n

staticNatValue :: forall n i. (KnownNat n, Num i) => SNat n -> i
{-# INLINE staticNatValue #-}
staticNatValue = fromInteger . natVal

staticNatFromProxy :: KnownNat n => Proxy n -> SNat n
staticNatFromProxy Proxy = MkSNat

-- | Add sharing information to the top level of a term, presumably
-- constructed using multiple applications of the `dDnotShared` operation.
-- The resulting term may not have sharing information inside,
-- but is ready to be shared as a whole.
ensureToplevelSharing :: IsPrimal d a => ADVal d a -> ADVal d a
ensureToplevelSharing (D u u') = dD u u'

scaleNotShared :: (Num a, IsPrimal d a) => a -> ADVal d a -> ADVal d a
scaleNotShared a (D u u') = dDnotShared (a * u) (dScale a u')

addNotShared :: (Num a, IsPrimal d a) => ADVal d a -> ADVal d a -> ADVal d a
addNotShared (D u u') (D v v') = dDnotShared (u + v) (dAdd u' v')

multNotShared :: (Num a, IsPrimal d a) => ADVal d a -> ADVal d a -> ADVal d a
multNotShared (D u u') (D v v') =
  dDnotShared (u * v) (dAdd (dScale v u') (dScale u v'))

addParameters :: (Numeric r, Num (Vector r))
              => Domains r -> Domains r -> Domains r
addParameters (Domains a0 a1) (Domains b0 b1) =
  Domains (a0 + b0)
          (V.zipWith (+) a1 b1)

-- Dot product and sum respective ranks and then sum it all.
dotParameters :: Numeric r => Domains r -> Domains r -> r
dotParameters (Domains a0 a1) (Domains b0 b1) =
  a0 LA.<.> b0
  + V.sum (V.zipWith (\v1 u1 ->
      if isTensorDummy v1 || isTensorDummy u1
      then 0
      else OT.toVector v1 LA.<.> OT.toVector u1) a1 b1)


-- * General operations, for any tensor rank

-- These instances are required by the @Real@ instance, which is required
-- by @RealFloat@, which gives @atan2@. No idea what properties
-- @Real@ requires here, so let it crash if it's really needed.
instance Eq (ADVal d a) where

instance Ord (ADVal d a) where

instance (Num a, IsPrimal d a) => Num (ADVal d a) where
  D u u' + D v v' = dD (u + v) (dAdd u' v')
  D u u' - D v v' = dD (u - v) (dAdd u' (dScale (-1) v'))
  D u u' * D v v' = dD (u * v) (dAdd (dScale v u') (dScale u v'))
  negate (D v v') = dD (negate v) (dScale (-1) v')
  abs (D v v') = dD (abs v) (dScale (signum v) v')
  signum (D v _) = dD (signum v) dZero
  fromInteger = constant . fromInteger

instance (Real a, IsPrimal d a) => Real (ADVal d a) where
  toRational = undefined  -- TODO?

instance (Fractional a, IsPrimal d a) => Fractional (ADVal d a) where
  D u u' / D v v' =
    let recipSq = recip (v * v)  -- ensure sharing; also elsewhere
    in dD (u / v) (dAdd (dScale (v * recipSq) u') (dScale (- u * recipSq) v'))
  recip (D v v') =
    let minusRecipSq = - recip (v * v)
    in dD (recip v) (dScale minusRecipSq v')
  fromRational = constant . fromRational

instance (Floating a, IsPrimal d a) => Floating (ADVal d a) where
  pi = constant pi
  exp (D u u') = let expU = exp u
                 in dD expU (dScale expU u')
  log (D u u') = dD (log u) (dScale (recip u) u')
  sqrt (D u u') = let sqrtU = sqrt u
                  in dD sqrtU (dScale (recip (sqrtU + sqrtU)) u')
  D u u' ** D v v' = dD (u ** v) (dAdd (dScale (v * (u ** (v - 1))) u')
                                       (dScale ((u ** v) * log u) v'))
  logBase x y = log y / log x
  sin (D u u') = dD (sin u) (dScale (cos u) u')
  cos (D u u') = dD (cos u) (dScale (- (sin u)) u')
  tan (D u u') = let cosU = cos u
                 in dD (tan u) (dScale (recip (cosU * cosU)) u')
  asin (D u u') = dD (asin u) (dScale (recip (sqrt (1 - u*u))) u')
  acos (D u u') = dD (acos u) (dScale (- recip (sqrt (1 - u*u))) u')
  atan (D u u') = dD (atan u) (dScale (recip (1 + u*u)) u')
  sinh (D u u') = dD (sinh u) (dScale (cosh u) u')
  cosh (D u u') = dD (cosh u) (dScale (sinh u) u')
  tanh (D u u') = let y = tanh u
                  in dD y (dScale (1 - y * y) u')
  asinh (D u u') = dD (asinh u) (dScale (recip (sqrt (1 + u*u))) u')
  acosh (D u u') = dD (acosh u) (dScale (recip (sqrt (u*u - 1))) u')
  atanh (D u u') = dD (atanh u) (dScale (recip (1 - u*u)) u')

instance (RealFrac a, IsPrimal d a) => RealFrac (ADVal d a) where
  properFraction = undefined
    -- TODO: others, but low priority, since these are extremely not continuous

instance (RealFloat a, IsPrimal d a) => RealFloat (ADVal d a) where
  atan2 (D u u') (D v v') =
    let t = 1 / (u * u + v * v)
    in dD (atan2 u v) (dAdd (dScale (- u * t) v') (dScale (v * t) u'))
      -- we can be selective here and omit the other methods,
      -- most of which don't even have a differentiable codomain

instance (Num a, IsPrimal d a) => HasPrimal (ADVal d a) where
  type PrimalOf (ADVal d a) = a
  type DualOf (ADVal d a) = Dual d a
  constant a = dD a dZero
  scale a (D u u') = dD (a * u) (dScale a u')
  primalPart (D u _) = u
  dualPart (D _ u') = u'
  ddD = dD
  fromIntOf = constant . fromInteger . fromIntegral

instance HasPrimal Float where
  type PrimalOf Float = Float
  type DualOf Float = ()
  constant = id
  scale = (*)
  primalPart = id
  dualPart _ = ()
  ddD u _ = u
  fromIntOf = fromInteger . fromIntegral

instance HasPrimal Double where
  type PrimalOf Double = Double
  type DualOf Double = ()
  constant = id
  scale = (*)
  primalPart = id
  dualPart _ = ()
  ddD u _ = u
  fromIntOf = fromInteger . fromIntegral

-- The constraint requires UndecidableInstances.
instance (KnownNat n, Numeric r, Num (Vector r))
         => HasPrimal (OR.Array n r) where
  type PrimalOf (OR.Array n r) = OR.Array n r
  type DualOf (OR.Array n r) = ()
  constant = id
  scale = (*)
  primalPart = id
  dualPart _ = ()
  ddD u _ = u
  fromIntOf = fromInteger . fromIntegral

instance HasPrimal (Ast r a) where
  type PrimalOf (Ast r a) = AstPrimalPart r a
  type DualOf (Ast r a) = ()  -- TODO: data AstDualPart: dScale, dAdd, dkonst1
  constant = AstConstant
  scale = AstScale
  primalPart = AstPrimalPart
  dualPart = error "TODO"
  ddD = error "TODO"
  fromIntOf = AstInt

logistic :: (Floating a, IsPrimal d a) => ADVal d a -> ADVal d a
logistic (D u u') =
  let y = recip (1 + exp (- u))
  in dD y (dScale (y * (1 - y)) u')

-- Optimized and more clearly written @u ** 2@.
square :: (Num a, IsPrimal d a) => ADVal d a -> ADVal d a
square (D u u') = dD (u * u) (dScale (2 * u) u')

squaredDifference :: (Num a, IsPrimal d a)
                  => a -> ADVal d a -> ADVal d a
squaredDifference targ res = square $ res - constant targ

relu, reluLeaky
  :: ( HasPrimal a, MonoFunctor (PrimalOf a), Num (PrimalOf a)
     , Ord (Element (PrimalOf a)), Fractional (Element (PrimalOf a)) )
  => a -> a
relu v =
  let oneIfGtZero = omap (\x -> if x > 0 then 1 else 0) (primalPart v)
  in scale oneIfGtZero v

reluLeaky v =
  let oneIfGtZero = omap (\x -> if x > 0 then 1 else 0.01) (primalPart v)
  in scale oneIfGtZero v

reluAst
  :: ( Fractional a, MonoFunctor (PrimalOf (Ast r a))
     , r ~ Element a, Fractional r )
  => Ast r a -> Ast r a
reluAst v =
  let oneIfGtZero = omap (\(AstPrimalPart x) ->
                            AstPrimalPart $ AstCond (AstRel GtOut [x, 0]) 1 0)
                         (primalPart v)
  in scale oneIfGtZero v


-- * Operations resulting in a scalar

sumElements10 :: ADModeAndNum d r
              => ADVal d (Vec r) -> ADVal d r
sumElements10 = lsumElements10

index10 :: ADModeAndNum d r => ADVal d (Vec r) -> Int -> ADVal d r
index10 = lindex10

minimum0 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d r
minimum0 = lminimum0

maximum0 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d r
maximum0 = lmaximum0

foldl'0 :: ADModeAndNum d r
        => (ADVal d r -> ADVal d r -> ADVal d r)
        -> ADVal d r -> ADVal d (Vec r)
        -> ADVal d r
foldl'0 f uu' (D v v') =
  let k = llength v
      g !acc ix p = f (dD p (dIndex10 v' [ix] [k])) acc
  in V.ifoldl' g uu' (OR.toVector v)

altSumElements10 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d r
altSumElements10 = foldl'0 (+) 0

-- | Dot product.
infixr 8 <.>!
(<.>!) :: ADModeAndNum d r
       => ADVal d (Vec r) -> ADVal d (Vec r) -> ADVal d r
(<.>!) = ldot0

-- | Dot product with a constant vector.
infixr 8 <.>!!
(<.>!!) :: ADModeAndNum d r
        => ADVal d (Vec r) -> Vec r -> ADVal d r
(<.>!!) (D u u') v = dD (ldot0 u v) (dDot10 v u')

sumElementsVectorOfDual
  :: ADModeAndNum d r => Data.Vector.Vector (ADVal d r) -> ADVal d r
sumElementsVectorOfDual = V.foldl' (+) 0

softMax :: ADModeAndNum d r
        => Data.Vector.Vector (ADVal d r)
        -> Data.Vector.Vector (ADVal d r)
softMax us =
  let expUs = V.map exp us  -- used twice below, so named, to enable sharing
      sumExpUs = sumElementsVectorOfDual expUs
  in V.map (\r -> r * recip sumExpUs) expUs

-- In terms of hmatrix: @-(log res <.> targ)@.
lossCrossEntropy :: forall d r. ADModeAndNum d r
                 => Vector r
                 -> Data.Vector.Vector (ADVal d r)
                 -> ADVal d r
lossCrossEntropy targ res =
  let f :: ADVal d r -> Int -> ADVal d r -> ADVal d r
      f !acc i d = acc + scale (targ V.! i) (log d)
  in negate $ V.ifoldl' f 0 res

-- In terms of hmatrix: @-(log res <.> targ)@.
lossCrossEntropyV :: ADModeAndNum d r
                  => Vec r
                  -> ADVal d (Vec r)
                  -> ADVal d r
lossCrossEntropyV targ res = negate $ log res <.>!! targ

-- Note that this is equivalent to a composition of softMax and cross entropy
-- only when @target@ is one-hot. Otherwise, results vary wildly. In our
-- rendering of the MNIST data all labels are one-hot.
lossSoftMaxCrossEntropyV
  :: ADModeAndNum d r
  => Vec r -> ADVal d (Vec r) -> ADVal d r
lossSoftMaxCrossEntropyV target (D u u') =
  -- The following protects from underflows, overflows and exploding gradients
  -- and is required by the QuickCheck test in TestMnistCNN.
  -- See https://github.com/tensorflow/tensorflow/blob/5a566a7701381a5cf7f70fce397759483764e482/tensorflow/core/kernels/sparse_softmax_op.cc#L106
  -- and https://github.com/tensorflow/tensorflow/blob/5a566a7701381a5cf7f70fce397759483764e482/tensorflow/core/kernels/xent_op.h
  let expU = exp (u - lkonst1 (llength u) (lmaximum0 u))
      sumExpU = lsumElements10 expU
      recipSum = recip sumExpU
-- not exposed: softMaxU = LA.scaleRecip sumExpU expU
      softMaxU = lkonst1 (llength expU) recipSum * expU
  in dD (negate $ log softMaxU `ldot0` target)  -- TODO: avoid: log . exp
        (dDot10 (softMaxU - target) u')


-- * Operations resulting in a vector

-- @1@ means rank one, so the dual component represents a vector.
fromList1 :: ADModeAndNum d r
          => [ADVal d r] -> ADVal d (Vec r)
fromList1 = lfromList1

fromVector1 :: ADModeAndNum d r
            => Data.Vector.Vector (ADVal d r) -> ADVal d (Vec r)
fromVector1 = lfromVector1

konst1 :: ADModeAndNum d r => ADVal d r -> Int -> ADVal d (Vec r)
konst1 d n = lkonst1 n d

append1 :: ADModeAndNum d r
        => ADVal d (Vec r) -> ADVal d (Vec r) -> ADVal d (Vec r)
append1 = lappend1

slice1 :: ADModeAndNum d r
       => Int -> Int -> ADVal d (Vec r) -> ADVal d (Vec r)
slice1 = lslice1

reverse1 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d (Vec r)
reverse1 = lreverse1

-- TODO: define Enum instance of (AstInt r) to enable AST for this.
-- No padding; remaining areas ignored.
maxPool1 :: ADModeAndNum d r
         => Int -> Int -> ADVal d (Vec r) -> ADVal d (Vec r)
maxPool1 ksize stride v =
  let slices = [slice1 i ksize v | i <- [0, stride .. llength v - ksize]]
  in fromList1 $ map maximum0 slices

softMaxV :: ADModeAndNum d r
         => ADVal d (Vec r) -> ADVal d (Vec r)
softMaxV d =
  let expU = exp d  -- shared in 2 places, though cse may do this for us
      sumExpU = sumElements10 expU
  in lkonst1 (llength d) (recip sumExpU) * expU

from10 :: ADModeAndNum d r => ADVal d (OR.Array 0 r) -> ADVal d r
from10 (D u u') = dD (OR.unScalar u) (dFrom10 u')

from01 :: ADModeAndNum d r => ADVal d r -> ADVal d (OR.Array 0 r)
from01 (D u u') = dD (OR.scalar u) (dFrom01 u')


-- * Build and map variants

build1POPL :: Int -> (Int -> ADVal d r) -> Data.Vector.Vector (ADVal d r)
build1POPL n f = V.fromList $ map f [0 .. n - 1]

-- Fake rank 1. This is still an array of delta expressions, thinly wrapped,
-- instead of a single delta expression representing an array.
-- We gain a little by storing the primal part in an unboxed vector.
build1Elementwise
  :: ADModeAndNum d r
  => Int -> (Int -> ADVal d r) -> ADVal d (Vec r)
build1Elementwise n f = fromList1 $ map f [0 .. n - 1]
  -- equivalent to @fromVector1 $ build1POPL n f@

build1Closure
  :: ADModeAndNum d r
  => Int -> (Int -> ADVal d r) -> ADVal d (Vec r)
build1Closure n f =
  let g i = let D u _ = f i in u
      h i = let D _ u' = f i in u'
  in dD (lfromList1 $ map g [0 .. n - 1]) (dBuild01 [n] (\l -> h (head l)))

build1
  :: ADModeAndNum d r
  => Int -> (Int -> ADVal d r) -> ADVal d (Vec r)
build1 = build1Closure

map1POPL :: (ADVal d r -> ADVal d r) -> Data.Vector.Vector (ADVal d r)
         -> Data.Vector.Vector (ADVal d r)
map1POPL f vd = V.map f vd

map1Elementwise
  :: ADModeAndNum d r
  => (ADVal d r -> ADVal d r) -> ADVal d (Vec r) -> ADVal d (Vec r)
map1Elementwise f d =
  build1Elementwise (llength d) $ \i -> f (index10 d i)
    -- equivalent to
    -- @fromVector1 . map1POPL f . rank1toVector
    --   where rank1toVector d@(D v _v') = V.generate (llength d) (index10 d)@

map1Closure
  :: ADModeAndNum d r
  => (ADVal d r -> ADVal d r) -> ADVal d (Vec r) -> ADVal d (Vec r)
map1Closure f d = build1Closure (llength d) $ \i -> f (index10 d i)


-- * Instances of VectorLike

instance (Numeric r, IntOf r ~ Int, VectorOf r ~ Vec r)
         => VectorLike (Vec r) r where
  llength = OR.size
  lminIndex = LA.minIndex . OR.toVector
  lmaxIndex = LA.maxIndex . OR.toVector

  lindex10 v ix = (V.! ix) $ OR.toVector v
  lsumElements10 = OR.sumA
  ldot0 u v = OR.toVector u LA.<.> OR.toVector v
  lminimum0 = LA.minElement . OR.toVector
  lmaximum0 = LA.maxElement . OR.toVector

  lfromList1 l = OR.fromList [length l] l
  lfromVector1 v = OR.fromVector [V.length v] $ V.convert v
  lkonst1 n r = OR.constant [n] r
  lappend1 = OR.append
  lslice1 i k = OR.slice [(i, k)]
  lreverse1 = OR.rev [0]
  lbuild1 n f = OR.generate [n] (\l -> f (head l))
  lmap1 = OR.mapA
  lzipWith = OR.zipWithA

instance VectorLike (Ast r (Vec r)) (Ast r r) where
  llength = AstLength
  lminIndex = AstMinIndex
  lmaxIndex = AstMaxIndex

  lindex10 v ix = AstIndex10 v [ix]
  lsumElements10 = AstSum10
  ldot0 = AstDot10
  lminimum0 = AstMinimum10
  lmaximum0 = AstMaximum10

  lfromList1 = AstFromList01
  lfromVector1 = AstFromVector01
  lkonst1 = AstKonst01
  lappend1 = AstAppend1
  lslice1 = AstSlice1
  lreverse1 = AstReverse1
  lbuild1 = astBuild1  -- TODO: this vectorizers depth-first, but is this
  lmap1 = astMap1      -- needed? should we vectorize the whole program instead?
  lzipWith = undefined  -- TODO: express all with build instead?

-- Not that this instance doesn't do vectorization. To enable it,
-- use the Ast instance, vectorize and finally interpret in ADVal.
-- The interpretation step uses this instance, including lbuild1
-- and lmap1, as a fallback for failed vectorization.
instance ADModeAndNum d r
         => VectorLike (ADVal d (Vec r)) (ADVal d r) where
  llength (D u _) = llength u
  lminIndex (D u _) = lminIndex u
  lmaxIndex (D u _) = lmaxIndex u

  lindex10 (D u u') ix = dD (lindex10 u ix) (dIndex10 u' [ix] [llength u])
  lsumElements10 (D u u') =
    dD (lsumElements10 u) (dSum10 [llength u] u')
  ldot0 (D u u') (D v v') = dD (ldot0 u v) (dAdd (dDot10 v u') (dDot10 u v'))
  lminimum0 (D u u') =
    dD (lminimum0 u) (dIndex10 u' [lminIndex u] [llength u])
  lmaximum0 (D u u') =
    dD (lmaximum0 u) (dIndex10 u' [lmaxIndex u] [llength u])

  lfromList1 l = dD (lfromList1 $ map (\(D u _) -> u) l)  -- I hope this fuses
                    (dFromList01 [length l] $ map (\(D _ u') -> u') l)
  lfromVector1 v = dD (lfromVector1 $ V.map (\(D u _) -> u) v)
                        -- I hope it fuses
                      (dFromVector01 [V.length v] $ V.map (\(D _ u') -> u') v)
  lkonst1 n (D u u') = dD (lkonst1 n u) (dKonst01 [n] u')
  lappend1 (D u u') (D v v') = dD (lappend1 u v) (dAppend1 u' (llength u) v')
  lslice1 i n (D u u') = dD (lslice1 i n u) (dSlice1 i n u' (llength u))
  lreverse1 (D u u') = dD (lreverse1 u) (dReverse1 u')
  lbuild1 = build1Elementwise
  lmap1 = map1Elementwise
  lzipWith = undefined

-- * AST-based build and map variants

-- Impure but in the most trivial way (only ever incremented counter).
unsafeAstVarCounter :: Counter
{-# NOINLINE unsafeAstVarCounter #-}
unsafeAstVarCounter = unsafePerformIO (newCounter 0)

unsafeGetFreshAstVar :: IO (AstVarName a)
{-# INLINE unsafeGetFreshAstVar #-}
unsafeGetFreshAstVar = AstVarName <$> atomicAddCounter_ unsafeAstVarCounter 1

astBuild1 :: AstInt r -> (AstInt r -> Ast r r) -> Ast r (OR.Array 1 r)
{-# NOINLINE astBuild1 #-}
astBuild1 n f = unsafePerformIO $ do
  freshAstVar <- unsafeGetFreshAstVar
  return $! build1Vectorize n ( freshAstVar
                              , AstFrom01 (f (AstIntVar freshAstVar)) )

astMap1 :: (Ast r r -> Ast r r) -> Ast r (OR.Array 1 r) -> Ast r (OR.Array 1 r)
{-# NOINLINE astMap1 #-}
astMap1 f e = unsafePerformIO $ do
  freshAstVar <- unsafeGetFreshAstVar
  return $! map1Vectorize (freshAstVar, f (AstVar0 freshAstVar)) e

build1Vectorize
  :: KnownNat n
  => AstInt r -> (AstVarName Int, Ast r (OR.Array n r))
  -> Ast r (OR.Array (n + 1) r)
build1Vectorize n (var, u) =
  if not (intVarInAst var u)
  then AstKonst1 n u
  else case u of
    AstOp codeOut args ->
      AstOp codeOut $ map (\w -> build1Vectorize n (var, w)) args
    AstCond b x y ->
      -- This handles conditionals that depend on var,
      -- so that we produce conditional delta expressions
      -- of size proportional to the exponent of conditional
      -- nesting, instead of proportional to the number of elements
      -- of the tensor.
      --
      -- TODO: actually check that var is in b and simplify if not.
      AstSelect n (var, b)
                (build1Vectorize n (var, x))
                (build1Vectorize n (var, y))
    AstSelect _n2 (_var2, _b) _x _y -> AstBuildPair1 n (var, u)  -- TODO
    AstInt{} -> error "build1Vectorize: can't have free int variables"
    AstConst{} -> error "build1Vectorize: can't have free int variables"
    AstConstant _r -> AstConstant $ AstPrimalPart $ AstBuildPair1 n (var, u)
      -- this is very fast when interpreted in a smart way, but constant
      -- character needs to be exposed for nested cases;
      -- TODO: similarly propagate AstConstant upwards elsewhere
    AstScale (AstPrimalPart r) d ->
      AstScale (AstPrimalPart $ build1Vectorize n (var, r))
               (build1Vectorize n (var, d))

    AstVar1{} -> error "build1Vectorize: can't have free int variables"

    AstIndex1 v i -> buildOfIndex10Vectorize n var v i
      -- @var@ is in @v@ or @i@; TODO: simplify i first
    AstSum1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstFromList1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstFromVector1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstKonst1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstAppend1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstSlice1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstReverse1{} -> AstBuildPair1 n (var, u)  -- TODO
    AstBuildPair1{} -> AstBuildPair1 n (var, u)  -- normal form?
    AstReshape1{} -> AstBuildPair1 n (var, u)  -- TODO

    AstFromList01{} -> AstBuildPair1 n (var, u)  -- TODO
    AstFromVector01{} -> AstBuildPair1 n (var, u)  -- TODO
    AstKonst01{} -> AstBuildPair1 n (var, u)  -- TODO
    AstBuildPair01{} -> AstBuildPair1 n (var, u)  -- TODO
    AstMapPair01{} -> AstBuildPair1 n (var, u)  -- TODO
    AstZipWithPair01{} -> AstBuildPair1 n (var, u)  -- TODO
    AstFrom01 v -> buildOfFrom01Vectorize n (var, v)

    AstOMap1{} -> AstBuildPair1 n (var, u)  -- TODO
    -- All other patterns are redundant due to GADT typing.

-- @var@ is in @v@ or @i@.
buildOfIndex10Vectorize
  :: KnownNat n
  => AstInt r -> AstVarName Int -> Ast r (OR.Array (n + 1) r) -> AstInt r
  -> Ast r (OR.Array (n + 1) r)
buildOfIndex10Vectorize n var v i =
  case v of
    AstOp codeOut args ->
      AstOp codeOut $ map (\w -> buildOfIndex10Vectorize n var w i) args
    AstCond b x y ->
      AstSelect n (var, b) (buildOfIndex10Vectorize n var x i)
                           (buildOfIndex10Vectorize n var y i)
    AstConst _r -> buildOfIndex10VectorizeVarNotInV n var v i
    -- AstFromList1, AstFromVector1: see Note [AstFromList1 is hard]
    AstFromList1 _ l | AstIntConst k <- i -> build1Vectorize n (var, l !! k)
    -- TODO: AstAppend1 v1 v2 -> ... AstCond (i < AstLength v1) (...v1) (...v2)
    AstKonst1 _ r -> build1Vectorize n (var, r)
    AstSlice1 i2 _ u ->
      build1Vectorize n (var, AstIndex1 u (AstIntOp PlusIntOut [i2, i]))
        -- TODO: or should we rewrite in the opposite direction?
    -- TODO: AstReverse1 easy
    -- AstBuildPair1 _ (var2, u2) ->
    --   build1Vectorize n (var, substitute var2 i u2))
           -- TODO: use environments instead
    _ ->
      if intVarInAst var v
      then -- can't do much, probably, since v different in each cell?
        AstBuildPair1 n (var, AstIndex1 v i)
      else
        buildOfIndex10VectorizeVarNotInV n var v i

-- The case where @var@ does not occur in @v@, which implies it's in @i@.
buildOfIndex10VectorizeVarNotInV
  :: AstInt r -> AstVarName Int -> Ast r (OR.Array (n + 1) r) -> AstInt r
  -> Ast r (OR.Array (n + 1) r)
buildOfIndex10VectorizeVarNotInV n var v i = case i of
  AstIntOp PlusIntOut [AstIntVar var2, i2] | var2 == var ->
    AstSlice1 i2 n v
  AstIntVar var2 -> assert (var2 == var) $
    AstSlice1 0 n v  -- simplified further elsewhere, if just identity
  {- TODO: these are impossible (they duplicate the first branch
     of build1Vectorize), unless we start descending recursively:
  AstIntConst _i2 ->
    AstKonst1 n (AstIndex1 v i)  -- v and i the same in each cell, so legit
  AstIntVar _var2 ->
    AstKonst1 n (AstIndex1 v i)  -- v and i the same in each cell, so legit
  -}
  _ -> AstBuildPair1 n (var, AstIndex1 v i)
    -- TODO:
    -- add a new 'gather' operation somehow and, if a more complex index
    -- expression, construct 'gather'

buildOfFrom01Vectorize
  :: AstInt r -> (AstVarName Int, Ast r r) -> Ast r (OR.Array 1 r)
buildOfFrom01Vectorize n (var, u) =
  case u of
    AstOp codeOut args ->
      AstOp codeOut
      $ map (\w -> buildOfFrom01Vectorize n (var, w)) args
    AstCond b x y ->
      AstSelect n (var, b) (buildOfFrom01Vectorize n (var, x))
                           (buildOfFrom01Vectorize n (var, y))
    AstInt{} -> error "buildOfFrom01Vectorize: can't have free int variables"
    AstConst{} -> error "buildOfFrom01Vectorize: can't have free int variables"
    AstConstant _r ->
      AstConstant $ AstPrimalPart $ AstBuildPair1 n (var, AstFrom01 u)
      -- this is very fast when interpreted in a smart way, but constant
      -- character needs to be exposed for nested cases;
      -- TODO: similarly propagate AstConstant upwards elsewhere
    AstScale (AstPrimalPart r) d ->
      AstScale (AstPrimalPart $ buildOfFrom01Vectorize n (var, r))
               (buildOfFrom01Vectorize n (var, d))
    AstVar0{} ->
      error "buildOfFrom01Vectorize: can't have free int variables"
    AstIndex10 _v _is  -> AstBuildPair1 n (var, AstFrom01 u)  -- TODO
    AstSum10 _v -> AstBuildPair1 n (var, AstFrom01 u)  -- TODO
    AstDot10 _u _v -> AstBuildPair1 n (var, AstFrom01 u)  -- TODO
    AstFrom10 v -> build1Vectorize n (var, v)
    AstMinimum10 _v -> AstBuildPair1 n (var, AstFrom01 u)  -- TODO
    AstMaximum10 _v -> AstBuildPair1 n (var, AstFrom01 u)  -- TODO

-- TODO: speed up by keeping free vars in each node.
intVarInAst :: AstVarName Int -> Ast r a -> Bool
intVarInAst var v = case v of
  AstOp _ lv -> or $ map (intVarInAst var) lv
  AstCond _b x y -> intVarInAst var x || intVarInAst var y  -- TODO: check in b
  AstSelect _n (var2, _b) x y ->
    var == var2 || intVarInAst var x || intVarInAst var y
      -- TODO: check in n and b
  AstConst{} -> False
  AstVar0{} -> False  -- not an int variable
  AstVar1{} -> False  -- not an int variable
  AstFromList1 _ l -> or $ map (intVarInAst var) l  -- down from rank 1 to 0
  AstFromVector1 _ vl -> or $ map (intVarInAst var) $ V.toList vl
  _ -> True  -- conservative, TODO

{- Note [AstFromList1 is hard]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is an example where simple but complete vectorization makes things worse.
Any simple rule that would let us fully vectorize

AstBuildPair1 N ("ix", AstIndex10
                         (AstFromList1 [ AstIndex10 v (AstIntVar "ix")
                                       , AstIndex10 v (1 + AstIntVar "ix") ])
                         (AstIntVar "ix" `mod` 2))

would be similar to the POPL implementation of build,
which means constructing a collection of all build elements,
substituting a known integer for "ix" in each and storing them all
(eventually as delta-expressions on tape) until evaluation.

While substituting a known integer for "ix" may simplify @v@
and permit vectorization of builds nested inside @v@, this nevertheless
turns a 2-element expression written by the user into a N-element
monstrosity. With high enough N, evaluating the result takes much more
memory than storing a non-vectorized closure on tape (the closure
would be a serialized Ast expression, so possibly fine on GPUs).

A non-simple rule that would handle this example would need a special
build constructor variant that does not build from individual elements,
but from vectors, distributing their elements in various patterns
(in case of @mod@, concatenating their many "ix"-affected copies).
-}

-- TODO: Shall this be represented and processed as just build?
-- But doing this naively copies @w@ a lot, so we'd need to wait
-- until AST handles sharing properly. Or make @w@ a variable.
map1Vectorize
  :: (AstVarName r, Ast r r) -> Ast r (OR.Array 1 r)
  -> Ast r (OR.Array 1 r)
map1Vectorize (var, u) w = case u of
  AstOp codeOut args ->
    AstOp codeOut $ map (\x -> map1Vectorize (var, x) w) args
  AstInt _i -> AstMapPair01 (var, u) w  -- TODO
  AstCond _b _x1 _x2 -> AstMapPair01 (var, u) w  -- TODO
  AstConst r -> AstKonst01 (AstLength w) (AstConst r)
  AstConstant _r -> AstMapPair01 (var, u) w  -- TODO
  AstScale (AstPrimalPart r) d ->
    AstScale (AstPrimalPart $ map1Vectorize (var, r) w)
             (map1Vectorize (var, d) w)
  AstVar0 var2 | var2 == var -> w  -- identity mapping
  AstVar0 var2 -> AstKonst01 (AstLength w) (AstVar0 var2)
  AstIndex10 _v _i -> AstMapPair01 (var, u) w  -- TODO
    -- both _v and _i can depend on var, e.g., because of conditionals
  AstSum10 _v -> AstMapPair01 (var, u) w  -- TODO
  AstDot10 _u _v -> AstMapPair01 (var, u) w  -- TODO
  AstMinimum10 _v -> AstMapPair01 (var, u) w  -- TODO
  AstMaximum10 _v -> AstMapPair01 (var, u) w  -- TODO
  _ -> undefined
  -- TODO: -- All other patterns are redundant due to GADT typing.

leqAst :: Ast r r -> Ast r r -> AstBool r
leqAst d e = AstRel LeqOut [d, e]

gtAst :: Ast r r -> Ast r r -> AstBool r
gtAst d e = AstRel GtOut [d, e]

gtIntAst :: AstInt r -> AstInt r -> AstBool r
gtIntAst i j = AstRelInt GtOut [i, j]

interpretLambdaD0
  :: (ADModeAndNum d r, IsPrimalAndHasFeatures d a r)
  => IM.IntMap (AstVar (ADVal d r) (ADVal d (Vec r)))
  -> (AstVarName r, Ast r a)
  -> ADVal d r -> ADVal d a
interpretLambdaD0 env (AstVarName var, ast) =
  \d -> interpretAst (IM.insert var (AstVarR0 d) env) ast

interpretLambdaI
  :: (ADModeAndNum d r, IsPrimalAndHasFeatures d a r)
  => IM.IntMap (AstVar (ADVal d r) (ADVal d (Vec r)))
  -> (AstVarName Int, Ast r a)
  -> Int -> ADVal d a
interpretLambdaI env (AstVarName var, ast) =
  \i -> interpretAst (IM.insert var (AstVarI i) env) ast

interpretAst
  :: (ADModeAndNum d r, IsPrimalAndHasFeatures d a r)
  => IM.IntMap (AstVar (ADVal d r) (ADVal d (Vec r)))
  -> Ast r a -> ADVal d a
interpretAst env = \case
  AstOp codeOut args ->
    interpretAstOp (interpretAst env) codeOut args
  AstCond b a1 a2 -> if interpretAstBool env b
                     then interpretAst env a1
                     else interpretAst env a2
  AstSelect n (AstVarName var, b) a1 a2 ->
    let k = interpretAstInt env n
        f [i] = if interpretAstBool (IM.insert var (AstVarI i) env) b
                then 1
                else 0
        f _ = error "AstSelect: unexpected argument"
        bitmap = constant $ OR.generate [k] f
        v1 = interpretAst env a1
        v2 = interpretAst env a2
    in bitmap * v1 + v2 - bitmap * v2
  AstInt i -> fromInteger $ fromIntegral $ interpretAstInt env i
  AstConst a -> constant a
  AstConstant (AstPrimalPart a) ->
    constant $ let D u _ = interpretAst env a in u
  AstScale (AstPrimalPart r) d ->
    scale (let D u _ = interpretAst env r in u) (interpretAst env d)

  AstVar0 (AstVarName var) -> case IM.lookup var env of
    Just (AstVarR0 d) -> d
    Just AstVarR1{} ->
      error $ "interpretAst: type mismatch for var " ++ show var
    Just AstVarI{} -> error $ "interpretAst: type mismatch for var " ++ show var
    Nothing -> error $ "interpretAst: unknown variable var " ++ show var
  AstVar1 (AstVarName var) -> case IM.lookup var env of
    Just AstVarR0{} ->
      error $ "interpretAst: type mismatch for var " ++ show var
    Just (AstVarR1 d) -> d
    Just AstVarI{} -> error $ "interpretAst: type mismatch for var " ++ show var
    Nothing -> error $ "interpretAst: unknown variable var " ++ show var

  AstIndex10 v is ->
    let D u u' = interpretAst env v
        ixs = map (interpretAstInt env) is
    in dD (u `atIndexInTensorR` ixs) (dIndex10 u' ixs (OR.shapeL u))
    -- not general enough: lindex10 (interpretAst env v) (interpretAstInt env i)
  AstSum10 _v -> undefined  -- TODO
    -- not general enough: lsumElements10 $ interpretAst env v
  AstDot10 _u _v -> undefined  -- TODO
    -- not general enough: interpretAst env u `ldot0` interpretAst env v
  AstFrom10 u -> from10 $ interpretAst env u
  AstMinimum10 _v -> undefined  -- TODO
    -- not general enough: lminimum0 $ interpretAst env v
  AstMaximum10 _v -> undefined  -- TODO
    -- not general enough: lmaximum0 $ interpretAst env v

  AstIndex1 _v _i -> undefined  -- TODO
  AstSum1 _v -> undefined  -- TODO
  AstFromList1 _ _l -> undefined  -- TODO
  AstFromVector1 _ _v -> undefined  -- TODO
  AstKonst1 _n _r -> undefined  -- TODO
  AstAppend1 _u _v -> undefined  -- TODO
    -- not general enough: lappend1 (interpretAst env u) (interpretAst env v)
  AstSlice1 _i _n _v -> undefined  -- TODO
    -- not general enough:
{-    let i' = interpretAstInt env i
        n' = interpretAstInt env n
        v'@(D pv _) = interpretAst env v
    in if i' == 0 && n' == llength pv
       then v'  -- perhaps common in code generated from AST
       else lslice1 i' n' v' -}
  AstReverse1 _v -> undefined  -- TODO
    -- not general enough: lreverse1 $ interpretAst env v
  AstBuildPair1 _i (_var, _r) -> undefined  -- TODO
    -- not general enough:
    -- lbuild1 (interpretAstInt env i) (interpretLambdaI env (var, r))
      -- fallback to POPL (memory blowup, but avoids functions on tape)
  AstReshape1{} -> undefined  -- TODO

  AstFromList01 l -> lfromList1 $ map (interpretAst env) l
  AstFromVector01 v -> lfromVector1 $ V.map (interpretAst env) v
  AstKonst01 n r -> lkonst1 (interpretAstInt env n) (interpretAst env r)
  AstBuildPair01 i (var, AstConstant r) ->  -- TODO: interpretAstPrimalPart
    constant
    $ lbuild1 (interpretAstInt env i)
              (\j -> let D v _ = interpretLambdaI env (var, AstConstant r) j
                     in v)
  AstBuildPair01 i (var, r) ->
    lbuild1 (interpretAstInt env i) (interpretLambdaI env (var, r))
      -- fallback to POPL (memory blowup, but avoids functions on tape)
  AstMapPair01 (var, r) e ->
    lmap1 (interpretLambdaD0 env (var, r)) (interpretAst env e)
      -- fallback to POPL (memory blowup, but avoids functions on tape)
  AstZipWithPair01 (_var1, _var2, _r) _e1 _e2 -> undefined
    -- a 2-var interpretLambda would be needed; or express all with build
  AstFrom01 u -> from01 $ interpretAst env u

  AstOMap1 (var, r) e ->  -- this only works on the primal part hence @constant@
    constant
    $ omap (\x -> let D u _ = interpretLambdaD0 env (var, r) (constant x)
                  in u)
           (let D u _ = interpretAst env e
            in u)

interpretAstInt :: ADModeAndNum d r
                => IM.IntMap (AstVar (ADVal d r) (ADVal d (Vec r)))
                -> AstInt r -> Int
interpretAstInt env = \case
  AstIntOp codeIntOut args ->
    interpretAstIntOp (interpretAstInt env) codeIntOut args
  AstIntCond b a1 a2 -> if interpretAstBool env b
                        then interpretAstInt env a1
                        else interpretAstInt env a2
  AstIntConst a -> a
  AstIntVar (AstVarName var) -> case IM.lookup var env of
    Just AstVarR0{} ->
      error $ "interpretAstP: type mismatch for var " ++ show var
    Just AstVarR1{} ->
      error $ "interpretAstP: type mismatch for var " ++ show var
    Just (AstVarI i) -> i
    Nothing -> error $ "interpretAstP: unknown variable var " ++ show var
  AstLength v -> llength $ interpretAst env v
  AstMinIndex v -> lminIndex $ interpretAst env v
  AstMaxIndex v -> lmaxIndex $ interpretAst env v

interpretAstBool :: ADModeAndNum d r
                 => IM.IntMap (AstVar (ADVal d r) (ADVal d (Vec r)))
                 -> AstBool r -> Bool
interpretAstBool env = \case
  AstBoolOp codeBoolOut args ->
    interpretAstBoolOp (interpretAstBool env) codeBoolOut args
  AstBoolConst a -> a
  AstRel relOut args ->
    let f x = let D u _u' = interpretAst env x in u
    in interpretAstRel f relOut args
  AstRelInt relOut args ->
    let f = interpretAstInt env
    in interpretAstRel f relOut args

interpretAstOp :: RealFloat b
               => (Ast r a -> b) -> CodeOut -> [Ast r a] -> b
{-# INLINE interpretAstOp #-}
interpretAstOp f PlusOut [u, v] = f u + f v
interpretAstOp f MinusOut [u, v] = f u - f v
interpretAstOp f TimesOut [u, v] = f u * f v
interpretAstOp f NegateOut [u] = negate $ f u
interpretAstOp f AbsOut [u] = abs $ f u
interpretAstOp f SignumOut [u] = signum $ f u
interpretAstOp f DivideOut [u, v] = f u / f v
interpretAstOp f RecipOut [u] = recip $ f u
interpretAstOp f ExpOut [u] = exp $ f u
interpretAstOp f LogOut [u] = log $ f u
interpretAstOp f SqrtOut [u] = sqrt $ f u
interpretAstOp f PowerOut [u, v] = f u ** f v
interpretAstOp f LogBaseOut [u, v] = logBase (f u) (f v)
interpretAstOp f SinOut [u] = sin $ f u
interpretAstOp f CosOut [u] = cos $ f u
interpretAstOp f TanOut [u] = tan $ f u
interpretAstOp f AsinOut [u] = asin $ f u
interpretAstOp f AcosOut [u] = acos $ f u
interpretAstOp f AtanOut [u] = atan $ f u
interpretAstOp f SinhOut [u] = sinh $ f u
interpretAstOp f CoshOut [u] = cosh $ f u
interpretAstOp f TanhOut [u] = tanh $ f u
interpretAstOp f AsinhOut [u] = asinh $ f u
interpretAstOp f AcoshOut [u] = acosh $ f u
interpretAstOp f AtanhOut [u] = atanh $ f u
interpretAstOp f Atan2Out [u, v] = atan2 (f u) (f v)
interpretAstOp f MaxOut [u, v] = max (f u) (f v)
interpretAstOp f MinOut [u, v] = min (f u) (f v)
interpretAstOp _ codeOut args =
  error $ "interpretAstOp: wrong number of arguments"
          ++ show (codeOut, length args)

interpretAstIntOp :: (AstInt r -> Int) -> CodeIntOut -> [AstInt r] -> Int
{-# INLINE interpretAstIntOp #-}
interpretAstIntOp f PlusIntOut [u, v] = f u + f v
interpretAstIntOp f MinusIntOut [u, v] = f u - f v
interpretAstIntOp f TimesIntOut [u, v] = f u * f v
interpretAstIntOp f NegateIntOut [u] = negate $ f u
interpretAstIntOp f AbsIntOut [u] = abs $ f u
interpretAstIntOp f SignumIntOut [u] = signum $ f u
interpretAstIntOp f MaxIntOut [u, v] = max (f u) (f v)
interpretAstIntOp f MinIntOut [u, v] = min (f u) (f v)
interpretAstIntOp _ codeIntOut args =
  error $ "interpretAstIntOp: wrong number of arguments"
          ++ show (codeIntOut, length args)

interpretAstBoolOp :: (AstBool r -> Bool) -> CodeBoolOut -> [AstBool r]
                   -> Bool
{-# INLINE interpretAstBoolOp #-}
interpretAstBoolOp f NotOut [u] = not $ f u
interpretAstBoolOp f AndOut [u, v] = f u && f v
interpretAstBoolOp f OrOut [u, v] = f u || f v
interpretAstBoolOp f IffOut [u, v] = f u == f v
interpretAstBoolOp _ codeBoolOut args =
  error $ "interpretAstBoolOp: wrong number of arguments"
          ++ show (codeBoolOut, length args)

interpretAstRel :: Ord b => (a -> b) -> RelOut -> [a] -> Bool
{-# INLINE interpretAstRel #-}
interpretAstRel f EqOut [u, v] = f u == f v
interpretAstRel f NeqOut [u, v] = f u /= f v
interpretAstRel f LeqOut [u, v] = f u <= f v
interpretAstRel f GeqOut [u, v] = f u >= f v
interpretAstRel f LsOut [u, v] = f u < f v
interpretAstRel f GtOut [u, v] = f u > f v
interpretAstRel _ codeRelOut args =
  error $ "interpretAstRel: wrong number of arguments"
          ++ show (codeRelOut, length args)

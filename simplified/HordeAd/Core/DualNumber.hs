{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, StandaloneDeriving,
             TypeFamilyDependencies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module HordeAd.Core.DualNumber
  ( ADVal, dD, pattern D
  , ADModeAndNum, HasDelta
  , fromX1, from1X
  , Vec, vecToV, vToVec
  , SNat(..), staticNatValue, staticNatFromProxy
  , ensureToplevelSharing, scaleNotShared, addNotShared, multNotShared
  , addParameters, dotParameters
  , logistic, square, squaredDifference
  , sumElements10, index10, minimum0, maximum0, altSumElements10
  , (<.>!), (<.>!!)
  , softMax, lossCrossEntropy, lossCrossEntropyV, lossSoftMaxCrossEntropyV
  , fromList1, fromVector1, konst1, append1, slice1, reverse1, maxPool1
  , softMaxV, build1POPL, build1Elementwise, build1Closure, build1
  , map1POPL, map1Elementwise
  , -- * Re-exports
    ADMode(..)
  , IsPrimal (..), IsPrimalAndHasFeatures, IsPrimalAndHasInputs
  , Element
  , Domain0, Domain1, Domains(..), nullDomains  -- an important re-export
  , -- temporarily re-exported, until these are wrapped in sugar
    Ast(..), AstPrimalPart1(..)
  , AstVarName(..), AstVar(..)
  , AstInt(..), AstBool(..)
  , OpCode(..), OpCodeInt(..), OpCodeBool(..), OpCodeRel(..)
  ) where

import Prelude

import qualified Data.Array.Convert
import qualified Data.Array.DynamicS as OT
import qualified Data.Array.RankedS as OR
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.TypeLits (KnownNat, Nat, natVal)
import           Numeric.LinearAlgebra (Numeric, Vector)
import qualified Numeric.LinearAlgebra as LA

import HordeAd.Core.Ast
import HordeAd.Core.DualClass
import HordeAd.Internal.Delta
  (Delta0, Domain0, Domain1, Domains (..), nullDomains)
import HordeAd.Core.SizedIndex
import HordeAd.Internal.TensorOps

-- * The main dual number type

-- | Values the objective functions operate on. The first type argument
-- is the automatic differentiation mode and the second is the underlying
-- basic values (scalars, vectors, matrices, tensors and any other
-- supported containers of scalars).
--
-- Here, the datatype is implemented as dual numbers (hence @D@),
-- where the primal component, the basic value, the \"number\"
-- can be any containers of scalars. The primal component has the type
-- given as the second type argument and the dual component (with the type
-- determined by the type faimly @Dual@) is defined elsewhere.
data ADVal (d :: ADMode) a = D a (Dual d a)

deriving instance (Show a, Show (Dual d a)) => Show (ADVal d a)

-- | Smart constructor for 'D' of 'ADVal' that additionally records sharing
-- information, if applicable for the differentiation mode in question.
-- The bare constructor should not be used directly (which is not enforced
-- by the types yet), except when deconstructing via pattern-matching.
dD :: IsPrimal d a => a -> Dual d a -> ADVal d a
dD a dual = D a (recordSharing dual)

-- | This a not so smart constructor for 'D' of 'ADVal' that does not record
-- sharing information. If used in contexts where sharing may occur,
-- it may cause exponential blowup when evaluating the term
-- in backpropagation phase. In contexts without sharing, it saves
-- some evaluation time and memory (in term structure, but even more
-- in the per-node data stored while evaluating).
dDnotShared :: a -> Dual d a -> ADVal d a
dDnotShared = D


-- * Auxiliary definitions

-- | A mega-shorthand for a bundle of connected type constraints.
-- The @Scalar@ in the name means that the second argument is the underlying
-- scalar type of a well behaved (wrt the differentiation mode in the first
-- argument) collection of primal and dual components of dual numbers.
type ADModeAndNum (d :: ADMode) r =
  ( Numeric r
  , Show r
  , HasRanks d r
  , IsPrimalAndHasFeatures d r r
  , IsPrimalR d r
  , RealFloat (Vector r)
  )

-- | Is a scalar and will be used to compute gradients via delta-expressions.
type HasDelta r = ( ADModeAndNum 'ADModeGradient r
                  , HasInputs r
                  , Dual 'ADModeGradient r ~ Delta0 r )

fromX1 :: (ADModeAndNum d r, KnownNat n)
       => ADVal d (OT.Array r) -> ADVal d (OR.Array n r)
fromX1 (D u u') = dDnotShared (Data.Array.Convert.convert u) (dFromX1 u')

from1X :: ADModeAndNum d r
       => ADVal d (OR.Array n r) -> ADVal d (OT.Array r)
from1X (D u u') = dDnotShared (Data.Array.Convert.convert u) (dFrom1X u')

-- Shims to reuse the tests for ordinary vectors.
type Vec r = OR.Array 1 r

vecToV :: Numeric r => Vec r -> Vector r
vecToV = OR.toVector

vToVec :: Numeric r => Vector r  -> Vec r
vToVec v = OR.fromVector [V.length v] v

-- All this is not needed in the simplified version, except for compilation
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


-- * Numeric instances for ADVal

-- These instances are required by the @Real@ instance, which is required
-- by @RealFloat@, which gives @atan2@. No idea what properties
-- @Real@ requires here, so let it crash if it's really needed.
instance Eq (ADVal d a) where

instance Ord (ADVal d a) where

instance (Num a, IsPrimal d a) => Num (ADVal d a) where
  D u u' + D v v' = dD (u + v) (dAdd u' v')
  D u u' - D v v' = dD (u - v) (dAdd u' (dScale (fromInteger (-1)) v'))
    -- without @fromInteger@, this is interpreted as @negate 1@,
    -- causing a crash for ranked tensors (can't guess the rank of @1@
    -- and then no other argument to derive the rank of @negate@);
    -- dynamic tensors dont check at all; shaped have all needed info in types
  D u u' * D v v' = dD (u * v) (dAdd (dScale v u') (dScale u v'))
  negate (D v v') = dD (negate v) (dScale (fromInteger (-1)) v')
  abs (D v v') = dD (abs v) (dScale (signum v) v')
  signum (D v _) = dD (signum v) dZero
  fromInteger = constantADVal . fromInteger

instance (Real a, IsPrimal d a) => Real (ADVal d a) where
  toRational = undefined  -- TODO?

instance (Fractional a, IsPrimal d a) => Fractional (ADVal d a) where
  D u u' / D v v' =
    let recipSq = recip (v * v)  -- ensure sharing; also elsewhere
    in dD (u / v) (dAdd (dScale (v * recipSq) u') (dScale (- u * recipSq) v'))
  recip (D v v') =
    let minusRecipSq = - recip (v * v)
    in dD (recip v) (dScale minusRecipSq v')
  fromRational = constantADVal . fromRational

instance (Floating a, IsPrimal d a) => Floating (ADVal d a) where
  pi = constantADVal pi
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


-- * Legacy operations needed to re-use vector differentiation tests

-- General operations, for any tensor rank

constantADVal :: IsPrimal d a => a -> ADVal d a
constantADVal a = dD a dZero

logistic :: (Floating a, IsPrimal d a) => ADVal d a -> ADVal d a
logistic (D u u') =
  let y = recip (1 + exp (- u))
  in dD y (dScale (y * (1 - y)) u')

-- Optimized and more clearly written @u ** 2@.
square :: (Num a, IsPrimal d a) => ADVal d a -> ADVal d a
square (D u u') = dD (u * u) (dScale (2 * u) u')

squaredDifference :: (Num a, IsPrimal d a)
                  => a -> ADVal d a -> ADVal d a
squaredDifference targ res = square $ res - constantADVal targ


-- Operations resulting in a scalar

unScalarADVal :: ADModeAndNum d r => ADVal d (OR.Array 0 r) -> ADVal d r
unScalarADVal (D u u') = dD (OR.unScalar u) (dUnScalar0 u')

sumElements10 :: ADModeAndNum d r
              => ADVal d (Vec r) -> ADVal d r
sumElements10 (D u u') = dD (tsum0R u) (dSum0 (tshapeR u) u')

index10 :: ADModeAndNum d r => ADVal d (Vec r) -> Int -> ADVal d r
index10 (D u u') ix =
  unScalarADVal $ dD (u `tindexR` ix)
                     (dIndex1 u' ix (head $ OR.shapeL u))

minimum0 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d r
minimum0 (D u u') =
  let ix = tminIndexR u
  in dD (OR.unScalar $ tindexR u ix)
        (dIndex0 u' (singletonIndex ix) (flattenShape (tshapeR u)))

maximum0 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d r
maximum0 (D u u') =
  let ix = tmaxIndexR u
  in dD (OR.unScalar $ tindexR u ix)
        (dIndex0 u' (singletonIndex ix) (flattenShape (tshapeR u)))

foldl'0 :: ADModeAndNum d r
        => (ADVal d r -> ADVal d r -> ADVal d r)
        -> ADVal d r -> ADVal d (Vec r)
        -> ADVal d r
foldl'0 f uu' (D v v') =
  let g !acc ix p =
        f (dD p (dIndex0 v' (singletonIndex ix) (flattenShape (tshapeR v)))) acc
  in V.ifoldl' g uu' (OR.toVector v)

altSumElements10 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d r
altSumElements10 = foldl'0 (+) 0

-- | Dot product.
infixr 8 <.>!
(<.>!) :: ADModeAndNum d r
       => ADVal d (Vec r) -> ADVal d (Vec r) -> ADVal d r
(<.>!) (D u u') (D v v') = dD (tdot0R u v)
                              (dAdd (dDot0 v u') (dDot0 u v'))

-- | Dot product with a constant vector.
infixr 8 <.>!!
(<.>!!) :: ADModeAndNum d r
        => ADVal d (Vec r) -> Vec r -> ADVal d r
(<.>!!) (D u u') v = dD (tdot0R u v) (dDot0 v u')

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
      f !acc i d = acc + scaleADVal (targ V.! i) (log d)
  in negate $ V.ifoldl' f 0 res

scaleADVal :: (Num a, IsPrimal d a) => a -> ADVal d a -> ADVal d a
scaleADVal a (D u u') = dD (a * u) (dScale a u')

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
  let expU = exp (u - OR.constant [tsizeR u] (tminimum0R u))
      sumExpU = tsum0R expU
      recipSum = recip sumExpU
-- not exposed: softMaxU = LA.scaleRecip sumExpU expU
      softMaxU =  OR.constant [tsizeR expU] recipSum * expU
  in dD (negate $ log softMaxU `tdot0R` target)  -- TODO: avoid: log . exp
        (dDot0 (softMaxU - target) u')


-- Operations resulting in a vector (really, a rank 1 OR.Array)

-- @1@ means rank one, so the dual component represents a vector.
fromList1 :: ADModeAndNum d r
          => [ADVal d r] -> ADVal d (Vec r)
fromList1 l =
  dD (tfromList0NR (singletonShape (length l)) $ map (\(D u _) -> u) l)
     (dFromList01 (singletonShape (length l)) $ map (\(D _ u') -> u') l)

fromVector1 :: ADModeAndNum d r
            => Data.Vector.Vector (ADVal d r) -> ADVal d (Vec r)
fromVector1 l =
  dD (tfromVector0NR (singletonShape (V.length l))
      $ V.convert $ V.map (\(D u _) -> u) l)  -- hope it fuses
     (dFromVector01 (singletonShape (V.length l))
      $ V.map (\(D _ u') -> u') l)

konst1 :: ADModeAndNum d r => ADVal d r -> Int -> ADVal d (Vec r)
konst1 (D u u') n =
  dD (tkonst0NR (singletonShape n) u) (dKonst01 (singletonShape n) u')

append1 :: ADModeAndNum d r
        => ADVal d (Vec r) -> ADVal d (Vec r) -> ADVal d (Vec r)
append1 (D u u') (D v v') = dD (tappendR u v)
                               (dAppend1 u' (head $ OR.shapeL u) v')

slice1 :: ADModeAndNum d r
       => Int -> Int -> ADVal d (Vec r) -> ADVal d (Vec r)
slice1 i k (D u u') = dD (tsliceR i k u)
                         (dSlice1 i k u' (head $ OR.shapeL u))

reverse1 :: ADModeAndNum d r => ADVal d (Vec r) -> ADVal d (Vec r)
reverse1 (D u u') = dD (treverseR u) (dReverse1 u')

-- TODO: define Enum instance of (AstInt r) to enable AST for this.
-- No padding; remaining areas ignored.
maxPool1 :: ADModeAndNum d r
         => Int -> Int -> ADVal d (Vec r) -> ADVal d (Vec r)
maxPool1 ksize stride v@(D u _) =
  let slices = [slice1 i ksize v | i <- [0, stride .. tsizeR u - ksize]]
  in fromList1 $ map maximum0 slices

softMaxV :: ADModeAndNum d r
         => ADVal d (Vec r) -> ADVal d (Vec r)
softMaxV d@(D u _) =
  let expU = exp d  -- shared in 2 places, though cse may do this for us
      sumExpU = sumElements10 expU
  in konst1 (recip sumExpU) (tsizeR u) * expU


-- Build and map variants

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
  in dD (OR.fromList [n] $ map g [0 .. n - 1])
        (dBuild01 (singletonShape n) (h . headIndex))

build1
  :: ADModeAndNum d r
  => Int -> (Int -> ADVal d r) -> ADVal d (Vec r)
build1 = build1Closure

map1POPL :: (ADVal d r -> ADVal d r) -> Data.Vector.Vector (ADVal d r)
         -> Data.Vector.Vector (ADVal d r)
map1POPL = V.map

map1Elementwise
  :: ADModeAndNum d r
  => (ADVal d r -> ADVal d r) -> ADVal d (Vec r) -> ADVal d (Vec r)
map1Elementwise f d@(D u _) =
  build1Elementwise (tsizeR u) $ \i -> f (index10 d i)
    -- equivalent to
    -- @fromVector1 . map1POPL f . rank1toVector
    --   where rank1toVector d@(D v _v') = V.generate (llength d) (lindex0 d)@

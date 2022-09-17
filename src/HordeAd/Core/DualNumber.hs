{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, FlexibleInstances, GADTs,
             QuantifiedConstraints, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
#if defined(VERSION_ghc_typelits_natnormalise)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
#endif
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module HordeAd.Core.DualNumber
  ( module HordeAd.Core.DualNumber
  , ADModeAndNum, HasDelta, ADMode(..)
  , Domain0, Domain1, Domain2, DomainX, Domains  -- an important re-export
  ) where

import Prelude

import qualified Data.Array.Convert
import qualified Data.Array.Dynamic as OTB
import qualified Data.Array.DynamicS as OT
import           Data.Array.Internal (valueOf)
import           Data.Array.Shape (DivRoundUp)
import qualified Data.Array.Shaped as OSB
import qualified Data.Array.ShapedS as OS
import           Data.List.Index (imap)
import           Data.MonoTraversable (MonoFunctor (omap))
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.TypeLits
  (KnownNat, Nat, natVal, type (+), type (-), type (<=))
import           Numeric.LinearAlgebra (Matrix, Numeric, Vector)
import qualified Numeric.LinearAlgebra as HM

import HordeAd.Core.DualClass
import HordeAd.Internal.Delta
  (Domain0, Domain1, Domain2, DomainX, Domains, isTensorDummy)

-- | Sizes of tensor dimensions, of batches, etc., packed for passing
-- between functions as witnesses of type variable values.
data StaticNat (n :: Nat) where
  MkSN :: KnownNat n => StaticNat n

staticNatValue :: forall n i. (KnownNat n, Num i) => StaticNat n -> i
{-# INLINE staticNatValue #-}
staticNatValue = fromInteger . natVal

staticNatFromProxy :: KnownNat n => Proxy n -> StaticNat n
staticNatFromProxy Proxy = MkSN

-- * The main dual number types

-- | Values the objective functions operate on (a generalization of scalars
-- and of vectors, matrices, tensors and any other supported containers
-- of scalars). The first type argument is the differentiation mode
-- and the second is the underlying values (the scalars, vectors, etc.).
--
-- Here, the datatype is implemented as dual numbers (hence @D@),
-- where the \"numbers\" can be any containers of scalars.
-- Thus, the primal component has the type given as the second type argument
-- and the dual component (named @Dual@) is special and defined elsewhere.
data ADVal (d :: ADMode) a = D a (Dual d a)

addParameters :: (Numeric r, Num (Vector r))
              => Domains r -> Domains r -> Domains r
addParameters (a0, a1, a2, aX) (b0, b1, b2, bX) =
  (a0 + b0, V.zipWith (+) a1 b1, V.zipWith (+) a2 b2, V.zipWith (+) aX bX)

-- Dot product and sum respective ranks and sum it all.
dotParameters :: Numeric r => Domains r -> Domains r -> r
dotParameters (a0, a1, a2, aX) (b0, b1, b2, bX) =
  a0 HM.<.> b0
  + V.sum (V.zipWith (\v1 u1 ->
      if V.null v1 || V.null u1
      then 0
      else v1 HM.<.> u1) a1 b1)
  + V.sum (V.zipWith (\v2 u2 ->
      if HM.rows v2 <= 0 || HM.rows u2 <= 0
      then 0
      else HM.flatten v2 HM.<.> HM.flatten u2) a2 b2)
  + V.sum (V.zipWith (\vX uX ->
      if isTensorDummy vX || isTensorDummy uX
      then 0
      else OT.toVector vX HM.<.> OT.toVector uX) aX bX)


-- * General operations, for any tensor rank

-- These instances are required by the @Real@ instance, which is required
-- by @RealFloat@, which gives @atan2@. No idea what properties
-- @Real@ requires here, so let it crash if it's really needed.
instance Eq (ADVal d a) where

instance Ord (ADVal d a) where

-- These instances are dangerous due to possible subexpression copies
-- leading to allocation explosion. Expressions should be wrapped in
-- the monadic @returnLet@ whenever there is a possibility they can be
-- used multiple times in a larger expression.
instance (Num a, IsPrimal d a) => Num (ADVal d a) where
  D u u' + D v v' = D (u + v) (dAdd u' v')
  D u u' - D v v' = D (u - v) (dAdd u' (dScale (-1) v'))
  D u u' * D v v' = D (u * v) (dAdd (dScale v u') (dScale u v'))
  negate (D v v') = D (negate v) (dScale (-1) v')
  abs (D v v') = D (abs v) (dScale (signum v) v')
  signum (D v _) = D (signum v) dZero
  fromInteger = constant . fromInteger

instance (Real a, IsPrimal d a) => Real (ADVal d a) where
  toRational = undefined  -- TODO?

instance (Fractional a, IsPrimal d a) => Fractional (ADVal d a) where
  D u u' / D v v' =
    let recipSq = recip (v * v)
    in D (u / v) (dAdd (dScale (v * recipSq) u') (dScale (- u * recipSq) v'))
  recip (D v v') =
    let minusRecipSq = - recip (v * v)
    in D (recip v) (dScale minusRecipSq v')
  fromRational = constant . fromRational

instance (Floating a, IsPrimal d a) => Floating (ADVal d a) where
  pi = constant pi
  exp (D u u') = let expU = exp u
                 in D expU (dScale expU u')
  log (D u u') = D (log u) (dScale (recip u) u')
  sqrt (D u u') = let sqrtU = sqrt u
                  in D sqrtU (dScale (recip (sqrtU + sqrtU)) u')
  D u u' ** D v v' = D (u ** v) (dAdd (dScale (v * (u ** (v - 1))) u')
                                      (dScale ((u ** v) * log u) v'))
  logBase x y = log y / log x
  sin (D u u') = D (sin u) (dScale (cos u) u')
  cos (D u u') = D (cos u) (dScale (- (sin u)) u')
  tan (D u u') = let cosU = cos u
                 in D (tan u) (dScale (recip (cosU * cosU)) u')
  asin (D u u') = D (asin u) (dScale (recip (sqrt (1 - u*u))) u')
  acos (D u u') = D (acos u) (dScale (- recip (sqrt (1 - u*u))) u')
  atan (D u u') = D (atan u) (dScale (recip (1 + u*u)) u')
  sinh (D u u') = D (sinh u) (dScale (cosh u) u')
  cosh (D u u') = D (cosh u) (dScale (sinh u) u')
  tanh (D u u') = let y = tanh u
                  in D y (dScale (1 - y * y) u')
  asinh (D u u') = D (asinh u) (dScale (recip (sqrt (1 + u*u))) u')
  acosh (D u u') = D (acosh u) (dScale (recip (sqrt (u*u - 1))) u')
  atanh (D u u') = D (atanh u) (dScale (recip (1 - u*u)) u')

instance (RealFrac a, IsPrimal d a) => RealFrac (ADVal d a) where
  properFraction = undefined
    -- very low priority, since these are all extremely not continuous

instance (RealFloat a, IsPrimal d a) => RealFloat (ADVal d a) where
  atan2 (D u u') (D v v') =
    let t = 1 / (u * u + v * v)
    in D (atan2 u v) (dAdd (dScale (- u * t) v') (dScale (v * t) u'))
      -- we can be selective here and omit the other methods,
      -- most of which don't even have a differentiable codomain

constant :: IsPrimal d a => a -> ADVal d a
constant a = D a dZero

scale :: (Num a, IsPrimal d a) => a -> ADVal d a -> ADVal d a
scale a (D u u') = D (a * u) (dScale a u')

logistic :: (Floating a, IsPrimal d a) => ADVal d a -> ADVal d a
logistic (D u u') =
  let y = recip (1 + exp (- u))
  in D y (dScale (y * (1 - y)) u')

-- Optimized and more clearly written @u ** 2@.
square :: (Num a, IsPrimal d a) => ADVal d a -> ADVal d a
square (D u u') = D (u * u) (dScale (2 * u) u')

squaredDifference :: (Num a, IsPrimal d a)
                  => a -> ADVal d a -> ADVal d a
squaredDifference targ res = square $ res - constant targ

relu :: (ADModeAndNum d r, IsPrimalAndHasFeatures d a r)
     => ADVal d a -> ADVal d a
relu v@(D u _) =
  let oneIfGtZero = omap (\x -> if x > 0 then 1 else 0) u
  in scale oneIfGtZero v

reluLeaky :: (ADModeAndNum d r, IsPrimalAndHasFeatures d a r)
          => ADVal d a -> ADVal d a
reluLeaky v@(D u _) =
  let oneIfGtZero = omap (\x -> if x > 0 then 1 else 0.01) u
  in scale oneIfGtZero v


-- * Operations resulting in a scalar

sumElements0 :: ADModeAndNum d r => ADVal d (Vector r) -> ADVal d r
sumElements0 (D u u') = D (HM.sumElements u) (dSumElements0 u' (V.length u))

index0 :: ADModeAndNum d r => ADVal d (Vector r) -> Int -> ADVal d r
index0 (D u u') ix = D (u V.! ix) (dIndex0 u' ix (V.length u))

minimum0 :: ADModeAndNum d r => ADVal d (Vector r) -> ADVal d r
minimum0 (D u u') =
  D (HM.minElement u) (dIndex0 u' (HM.minIndex u) (V.length u))

maximum0 :: ADModeAndNum d r => ADVal d (Vector r) -> ADVal d r
maximum0 (D u u') =
  D (HM.maxElement u) (dIndex0 u' (HM.maxIndex u) (V.length u))

-- If @v'@ is a @Input1@, this is much faster due to the optimization
-- in @Index0@.
foldl'0 :: ADModeAndNum d r
        => (ADVal d r -> ADVal d r -> ADVal d r)
        -> ADVal d r -> ADVal d (Vector r)
        -> ADVal d r
foldl'0 f uu' (D v v') =
  let k = V.length v
      g !acc ix p = f (D p (dIndex0 v' ix k)) acc
  in V.ifoldl' g uu' v

altSumElements0 :: ADModeAndNum d r => ADVal d (Vector r) -> ADVal d r
altSumElements0 = foldl'0 (+) 0

-- | Dot product.
infixr 8 <.>!
(<.>!) :: ADModeAndNum d r
       => ADVal d (Vector r) -> ADVal d (Vector r) -> ADVal d r
(<.>!) (D u u') (D v v') = D (u HM.<.> v) (dAdd (dDot0 v u') (dDot0 u v'))

-- | Dot product with a constant vector.
infixr 8 <.>!!
(<.>!!) :: ADModeAndNum d r
        => ADVal d (Vector r) -> Vector r -> ADVal d r
(<.>!!) (D u u') v = D (u HM.<.> v) (dDot0 v u')

infixr 8 <.>$
(<.>$) :: (ADModeAndNum d r, KnownNat n)
       => ADVal d (OS.Array '[n] r) -> ADVal d (OS.Array '[n] r)
       -> ADVal d r
(<.>$) d e = fromS1 d <.>! fromS1 e

fromX0 :: ADModeAndNum d r => ADVal d (OT.Array r) -> ADVal d r
fromX0 (D u u') = D (OT.unScalar u) (dFromX0 u')

fromS0 :: ADModeAndNum d r => ADVal d (OS.Array '[] r) -> ADVal d r
fromS0 (D u u') = D (OS.unScalar u) (dFromS0 u')

sumElementsVectorOfDual
  :: ADModeAndNum d r => Data.Vector.Vector (ADVal d r) -> ADVal d r
sumElementsVectorOfDual = V.foldl' (+) 0

softMax :: ADModeAndNum d r
        => Data.Vector.Vector (ADVal d r)
        -> Data.Vector.Vector (ADVal d r)
softMax us =
  let -- This has to be let-bound, because it's used two times below
      -- and we want it shared and cse may or may not be turned on.
      expUs = V.map exp us
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
                  => Vector r
                  -> ADVal d (Vector r)
                  -> ADVal d r
lossCrossEntropyV targ res = negate $ log res <.>!! targ

-- Note that this is equivalent to a composition of softMax and cross entropy
-- only when @target@ is one-hot. Otherwise, results vary wildly. In our
-- rendering of the MNIST data all labels are on-hot.
lossSoftMaxCrossEntropyV
  :: ADModeAndNum d r
  => Vector r -> ADVal d (Vector r) -> ADVal d r
lossSoftMaxCrossEntropyV target (D u u') =
  -- The following protects from underflows, overflows and exploding gradients
  -- and is required by the QuickCheck test in TestMnistCNN.
  -- See https://github.com/tensorflow/tensorflow/blob/5a566a7701381a5cf7f70fce397759483764e482/tensorflow/core/kernels/sparse_softmax_op.cc#L106
  -- and https://github.com/tensorflow/tensorflow/blob/5a566a7701381a5cf7f70fce397759483764e482/tensorflow/core/kernels/xent_op.h
  let expU = exp (u - HM.scalar (HM.maxElement u))
      sumExpU = HM.sumElements expU
      recipSum = recip sumExpU
-- not exposed: softMaxU = HM.scaleRecip sumExpU expU
      softMaxU = HM.scale recipSum expU
  in D (negate $ log softMaxU HM.<.> target)  -- TODO: avoid: log . exp
       (dDot0 (softMaxU - target) u')


-- * Operations resulting in a vector

-- @1@ means rank one, so the dual component represents a vector.
seq1 :: ADModeAndNum d r
     => Data.Vector.Vector (ADVal d r) -> ADVal d (Vector r)
seq1 v = D (V.convert $ V.map (\(D u _) -> u) v)  -- I hope this fuses
           (dSeq1 $ V.map (\(D _ u') -> u') v)

konst1 :: ADModeAndNum d r => ADVal d r -> Int -> ADVal d (Vector r)
konst1 (D u u') n = D (HM.konst u n) (dKonst1 u' n)

append1 :: ADModeAndNum d r
        => ADVal d (Vector r) -> ADVal d (Vector r)
        -> ADVal d (Vector r)
append1 (D u u') (D v v') = D (u V.++ v) (dAppend1 u' (V.length u) v')

slice1 :: ADModeAndNum d r
       => Int -> Int -> ADVal d (Vector r) -> ADVal d (Vector r)
slice1 i n (D u u') = D (V.slice i n u) (dSlice1 i n u' (V.length u))

sumRows1 :: ADModeAndNum d r => ADVal d (Matrix r) -> ADVal d (Vector r)
sumRows1 (D u u') = D (V.fromList $ map HM.sumElements $ HM.toRows u)
                      (dSumRows1 u' (HM.cols u))

sumColumns1 :: ADModeAndNum d r
            => ADVal d (Matrix r) -> ADVal d (Vector r)
sumColumns1 (D u u') = D (V.fromList $ map HM.sumElements $ HM.toColumns u)
                         (dSumColumns1 u' (HM.rows u))

-- If @v'@ is a @Input1@, this is much faster due to the optimization
-- in @Index0@. The detour through a boxed vector (list probably fuses away)
-- is costly, but only matters if @f@ is cheap.
map1 :: ADModeAndNum d r
     => (ADVal d r -> ADVal d r) -> ADVal d (Vector r)
     -> ADVal d (Vector r)
map1 f (D v v') =
  let k = V.length v
      g ix p = f $ D p (dIndex0 v' ix k)
      ds = imap g $ V.toList v
  in seq1 $ V.fromList ds

-- | Dense matrix-vector product.
infixr 8 #>!
(#>!) :: ADModeAndNum d r
      => ADVal d (Matrix r) -> ADVal d (Vector r)
      -> ADVal d (Vector r)
(#>!) (D u u') (D v v') = D (u HM.#> v) (dAdd (dMD_V1 u' v) (dM_VD1 u v'))

-- | Dense matrix-vector product with a constant vector.
infixr 8 #>!!
(#>!!) :: ADModeAndNum d r
       => ADVal d (Matrix r) -> Vector r
       -> ADVal d (Vector r)
(#>!!) (D u u') v = D (u HM.#> v) (dMD_V1 u' v)

fromX1 :: ADModeAndNum d r => ADVal d (OT.Array r) -> ADVal d (Vector r)
fromX1 (D u u') = D (OT.toVector u) (dFromX1 u')

fromS1 :: forall len d r. (KnownNat len, ADModeAndNum d r)
       => ADVal d (OS.Array '[len] r) -> ADVal d (Vector r)
fromS1 (D u u') = D (OS.toVector u) (dFromS1 u')

reverse1 :: ADModeAndNum d r => ADVal d (Vector r) -> ADVal d (Vector r)
reverse1 (D u u') = D (V.reverse u) (dReverse1 u')

flatten1 :: ADModeAndNum d r => ADVal d (Matrix r) -> ADVal d (Vector r)
flatten1 (D u u') = let (rows, cols) = HM.size u
                    in D (HM.flatten u) (dFlatten1 rows cols u')

flattenX1 :: ADModeAndNum d r
          => ADVal d (OT.Array r) -> ADVal d (Vector r)
flattenX1 (D u u') = let sh = OT.shapeL u
                     in D (OT.toVector u) (dFlattenX1 sh u')

flattenS1 :: (ADModeAndNum d r, OS.Shape sh)
          => ADVal d (OS.Array sh r) -> ADVal d (Vector r)
flattenS1 (D u u') = D (OS.toVector u) (dFlattenS1 u')

corr1 :: ADModeAndNum d r
      => ADVal d (Vector r) -> ADVal d (Vector r)
      -> ADVal d (Vector r)
corr1 ker@(D u _) vv@(D v _) = case (V.length u, V.length v) of
  (0, lenV) -> konst1 0 lenV
  (lenK, lenV) -> if lenK <= lenV
                  then vectorSlices2 lenK vv #>! ker
                  else error $ "corr1: len kernel " ++ show lenK
                               ++ " > len vector " ++ show lenV

-- This is not optimally implemented: @append1@ is costly compared
-- to the @mconcat@ counterpart.
conv1 :: ADModeAndNum d r
      => ADVal d (Vector r) -> ADVal d (Vector r)
      -> ADVal d (Vector r)
conv1 ker@(D u _) vv@(D v _) =
  let lenK = V.length u
      lenV = V.length v
      kerRev = reverse1 ker
      z = konst1 0 (lenK - 1)
      vvPadded = append1 z $ append1 vv z
  in if lenK == 0
     then konst1 0 lenV
     else corr1 kerRev vvPadded

-- No padding; remaining areas ignored.
maxPool1 :: ADModeAndNum d r
         => Int -> Int -> ADVal d (Vector r) -> ADVal d (Vector r)
maxPool1 ksize stride v@(D u _) =
  let slices = [slice1 i ksize v | i <- [0, stride .. V.length u - ksize]]
  in seq1 $ V.fromList $ map maximum0 slices

softMaxV :: ADModeAndNum d r
         => ADVal d (Vector r) -> ADVal d (Vector r)
softMaxV d@(D u _) =
  let expU = exp d  -- shared in 2 places, though cse may do this for us
      sumExpU = sumElements0 expU
  in konst1 (recip sumExpU) (V.length u) * expU

-- Note that this is equivalent to a composition of softMax and cross entropy
-- only when @target@ is one-hot. Otherwise, results vary wildly. In our
-- rendering of the MNIST data all labels are one-hot.
lossSoftMaxCrossEntropyL
  :: ADModeAndNum d r
  => Matrix r
  -> ADVal d (Matrix r)
  -> ADVal d (Vector r)
lossSoftMaxCrossEntropyL target (D u u') =
  let expU = exp (u - HM.scalar (HM.maxElement u))  -- vs exploding gradients
      sumExpU = V.fromList $ map HM.sumElements $ HM.toColumns expU
      recipSum = recip sumExpU
      softMaxU = HM.asRow recipSum * expU
                   -- this @asRow@ is safe; multiplied at once
      scaled = D (negate $ log softMaxU * target)
                 (dScale (softMaxU - target) u')
  in sumColumns1 scaled


-- * Operations resulting in a matrix

-- @2@ means rank two, so the dual component represents a matrix.
fromRows2 :: ADModeAndNum d r
          => Data.Vector.Vector (ADVal d (Vector r))
          -> ADVal d (Matrix r)
fromRows2 v = D (HM.fromRows $ map (\(D u _) -> u) $ V.toList v)
                (dFromRows2 $ V.map (\(D _ u') -> u') v)

fromColumns2 :: ADModeAndNum d r
             => Data.Vector.Vector (ADVal d (Vector r))
             -> ADVal d (Matrix r)
fromColumns2 v = D (HM.fromRows $ map (\(D u _) -> u) $ V.toList v)
                   (dFromColumns2 $ V.map (\(D _ u') -> u') v)

konst2 :: ADModeAndNum d r
       => ADVal d r -> (Int, Int) -> ADVal d (Matrix r)
konst2 (D u u') sz = D (HM.konst u sz) (dKonst2 u' sz)

transpose2 :: ADModeAndNum d r => ADVal d (Matrix r) -> ADVal d (Matrix r)
transpose2 (D u u') = D (HM.tr' u) (dTranspose2 u')

-- | Dense matrix-matrix product.
--
-- If @u@ is a m x n (number of rows x number of columns) matrix
-- and @v@ is a n x p matrix then the result of @u <>! v@ is a m x p matrix.
infixr 8 <>!
(<>!) :: ADModeAndNum d r
      => ADVal d (Matrix r) -> ADVal d (Matrix r)
      -> ADVal d (Matrix r)
(<>!) (D u u') (D v v') = D (u HM.<> v) (dAdd (dMD_M2 u' v) (dM_MD2 u v'))

-- | Dense matrix-matrix product with a constant matrix.
infixr 8 <>!!
(<>!!) :: ADModeAndNum d r
       => ADVal d (Matrix r) -> Matrix r
       -> ADVal d (Matrix r)
(<>!!) (D u u') v = D (u HM.<> v) (dMD_M2 u' v)

rowAppend2 :: ADModeAndNum d r
           => ADVal d (Matrix r) -> ADVal d (Matrix r)
           -> ADVal d (Matrix r)
rowAppend2 (D u u') (D v v') =
  D (u HM.=== v) (dRowAppend2 u' (HM.rows u) v')

columnAppend2 :: ADModeAndNum d r
              => ADVal d (Matrix r) -> ADVal d (Matrix r)
              -> ADVal d (Matrix r)
columnAppend2 (D u u') (D v v') =
  D (u HM.||| v) (dColumnAppend2 u' (HM.cols u) v')

rowSlice2 :: ADModeAndNum d r
          => Int -> Int -> ADVal d (Matrix r)
          -> ADVal d (Matrix r)
rowSlice2 i n (D u u') = D (HM.subMatrix (i, 0) (n, HM.cols u) u)
                           (dRowSlice2 i n u' (HM.rows u))

columnSlice2 :: ADModeAndNum d r
             => Int -> Int -> ADVal d (Matrix r)
             -> ADVal d (Matrix r)
columnSlice2 i n (D u u') = D (HM.subMatrix (0, i) (HM.rows u, n) u)
                              (dColumnSlice2 i n u' (HM.rows u))

asRow2 :: ADModeAndNum d r
       => ADVal d (Vector r) -> Int -> ADVal d (Matrix r)
asRow2 (D u u') n = D (HM.fromRows $ replicate n u) (dAsRow2 u')

asColumn2 :: ADModeAndNum d r
          => ADVal d (Vector r) -> Int -> ADVal d (Matrix r)
asColumn2 (D u u') n = D (HM.fromColumns $ replicate n u) (dAsColumn2 u')

fromX2 :: ADModeAndNum d r => ADVal d (OT.Array r) -> ADVal d (Matrix r)
fromX2 (D u u') = case OT.shapeL u of
  [_, cols] -> D (HM.reshape cols $ OT.toVector u) (dFromX2 u')
  dims -> error $ "fromX2: the tensor has wrong dimensions " ++ show dims

fromS2 :: forall rows cols d r.
          (KnownNat rows, KnownNat cols, ADModeAndNum d r)
       => ADVal d (OS.Array '[rows, cols] r) -> ADVal d (Matrix r)
fromS2 (D u u') = D (HM.reshape (valueOf @cols) $ OS.toVector u) (dFromS2 u')

flipud2 :: ADModeAndNum d r => ADVal d (Matrix r) -> ADVal d (Matrix r)
flipud2 (D u u') = D (HM.flipud u) (dFlipud2 u')

fliprl2 :: ADModeAndNum d r => ADVal d (Matrix r) -> ADVal d (Matrix r)
fliprl2 (D u u') = D (HM.fliprl u) (dFliprl2 u')

vectorSlices2 :: ADModeAndNum d r
              => Int -> ADVal d (Vector r) -> ADVal d (Matrix r)
vectorSlices2 n vv@(D v _) =
  fromRows2 $ V.fromList [slice1 i n vv | i <- [0 .. V.length v - n]]

reshape2 :: ADModeAndNum d r
         => Int -> ADVal d (Vector r) -> ADVal d (Matrix r)
reshape2 cols (D u u') = D (HM.reshape cols u) (dReshape2 cols u')

-- TODO: This has list of matrices result instead of a cube tensor.
matrixSlices2 :: ADModeAndNum d r
              => Int -> ADVal d (Matrix r) -> [ADVal d (Matrix r)]
matrixSlices2 dr m@(D u _) =
  let (rows, cols) = HM.size u
      n = dr * cols
      f k = reshape2 cols $ slice1 (k * cols) n (flatten1 m)
  in map f [0 .. rows - dr]

-- Not optimal: matrix is constructed and destructed immediately,
-- which is costly when evaluating delta expressions. The transposes
-- may not be optimal, either. This goes down to individual deltas
-- of scalars, which is horrible for performance. Unlike @corr1@
-- this uses the slow dot product instead of the fast matrix-vector
-- (or matrix-matrix) multiplication.
corr2 :: forall d r. ADModeAndNum d r
      => ADVal d (Matrix r) -> ADVal d (Matrix r)
      -> ADVal d (Matrix r)
corr2 ker@(D u _) m@(D v _) =
  let (rowsK, colsK) = HM.size u
      (rowsM, colsM) = HM.size v
      rr = rowsM - rowsK + 1
      rc = colsM - colsK + 1
  in if | rowsK <= 0 || colsK <= 0 ->
          error $ "corr2: empty kernel not handled: " ++ show (rowsK, colsK)
        | rr <= 0 || rc <= 0 ->
          error $ "corr2: dim kernel " ++ show (rowsK, colsK)
                  ++ " > dim matrix " ++ show (rowsM, colsM)
        | otherwise ->
          let kerTransV = flatten1 (transpose2 ker)
              dotColSlices :: ADVal d (Matrix r) -> [ADVal d r]
              dotColSlices tm =
                let ttm = transpose2 tm
                    colSlices = matrixSlices2 colsK ttm
                    f :: ADVal d (Matrix r) -> ADVal d r
                    f sm = kerTransV <.>! flatten1 sm
                in map f colSlices
              rowSlices = matrixSlices2 rowsK m
              dotSlicesOfSlices = map dotColSlices rowSlices
          in reshape2 rc $ seq1 $ V.fromList $ concat dotSlicesOfSlices

conv2 :: forall d r. ADModeAndNum d r
      => ADVal d (Matrix r) -> ADVal d (Matrix r)
      -> ADVal d (Matrix r)
conv2 ker@(D u _) m@(D v _) =
  let (rowsK, colsK) = HM.size u
      (rowsM, colsM) = HM.size v
  in if | rowsK <= 0 || colsK <= 0 ->
          konst2 0 (rowsM + rowsK - 1, colsM + colsK - 1)
        | otherwise ->
          let zRow = konst2 0 (rowsK - 1, colsM)
              rowPadded = rowAppend2 zRow $ rowAppend2 m zRow
              zCol = konst2 0 (rowsM + 2 * (rowsK - 1), colsK - 1)
              padded = columnAppend2 zCol $ columnAppend2 rowPadded zCol
          in corr2 (fliprl2 . flipud2 $ ker) padded

conv2' :: ADModeAndNum d r
       => ADVal d (Matrix r) -> ADVal d (Matrix r)
       -> ADVal d (Matrix r)
conv2' (D u u') (D v v') = D (HM.conv2 u v) (dAdd (dConv2 u v') (dConv2 v u'))

-- A variant with limited padding, corresponding to SAME padding
-- from Tensorflow. Data size does not change with this padding.
-- It also performs convolution wrt flipped kernel (and so saves
-- on flipping it here), which makes no practical difference when
-- the kernel is initialized randomly.
convSame2 :: forall d r. ADModeAndNum d r
          => ADVal d (Matrix r) -> ADVal d (Matrix r)
          -> ADVal d (Matrix r)
convSame2 ker@(D u _) m@(D v _) =
  let (rowsK, colsK) = HM.size u
      (rowsM, colsM) = HM.size v
  in if | rowsK <= 0 || colsK <= 0 ->
          konst2 0 (rowsM, colsM)
        | otherwise ->
          let zRow = konst2 0 ((rowsK - 1) `div` 2, colsM)
              rowPadded = rowAppend2 zRow $ rowAppend2 m zRow
              zCol = konst2 0 (rowsM + rowsK - 1, (colsK - 1) `div` 2)
              padded = columnAppend2 zCol $ columnAppend2 rowPadded zCol
          in corr2 ker padded

-- No padding; remaining areas ignored.
maxPool2 :: forall d r. ADModeAndNum d r
         => Int -> Int -> ADVal d (Matrix r) -> ADVal d (Matrix r)
maxPool2 ksize stride m@(D u _) =
  let (rows, cols) = HM.size u
      colsOut = cols `div` stride
      resultRows = [0, stride .. rows - ksize]
      resultCols = [0, stride .. cols - ksize]
      resultCoords = [(r, c) | r <- resultRows, c <- resultCols]
      getArea :: (Int, Int) -> ADVal d (Vector r)
      getArea (r0, c0) =
        let getAreaAtRow r1 =
              append1 (slice1 (r1 * cols + c0) ksize (flatten1 m))
        in foldr getAreaAtRow (seq1 V.empty) [r0 .. r0 + ksize - 1]
      mins = map (maximum0 . getArea) resultCoords
  in reshape2 colsOut $ seq1 $ V.fromList mins


-- * Operations resulting in an arbitrary untyped tensor

konstX :: ADModeAndNum d r
       => ADVal d r -> OT.ShapeL -> ADVal d (OT.Array r)
konstX (D u u') sh = D (OT.constant sh u) (dKonstX u' sh)

appendX :: ADModeAndNum d r
        => ADVal d (OT.Array r) -> ADVal d (OT.Array r)
        -> ADVal d (OT.Array r)
appendX (D u u') (D v v') =
  D (u `OT.append` v) (dAppendX u' (head $ OT.shapeL u) v')

sliceX :: ADModeAndNum d r
       => Int -> Int -> ADVal d (OT.Array r) -> ADVal d (OT.Array r)
sliceX i n (D u u') = D (OT.slice [(i, n)] u)
                        (dSliceX i n u' (head $ OT.shapeL u))

indexX :: ADModeAndNum d r
       => ADVal d (OT.Array r) -> Int -> ADVal d (OT.Array r)
indexX (D u u') ix = D (OT.index u ix)
                       (dIndexX u' ix (head $ OT.shapeL u))

ravelFromListX :: ADModeAndNum d r
               => [ADVal d (OT.Array r)] -> ADVal d (OT.Array r)
ravelFromListX ld =
  let (lu, lu') = unzip $ map (\(D u u') -> (u, u')) ld
      sh = case lu of
        u : _ -> length lu : OT.shapeL u
        [] -> []
  in D (OT.ravel $ OTB.fromList sh lu) (dRavelFromListX lu')

unravelToListX :: ADModeAndNum d r
               => ADVal d (OT.Array r) -> [ADVal d (OT.Array r)]
unravelToListX (D v v') = case OT.shapeL v of
  k : _ ->
    let g ix p = D p (dIndexX v' ix k)
    in imap g $ OTB.toList $ OT.unravel v
  [] -> error "unravelToListX: wrong tensor dimensions"  -- catch early

mapX :: ADModeAndNum d r
     => (ADVal d (OT.Array r) -> ADVal d (OT.Array r))
     -> ADVal d (OT.Array r)
     -> ADVal d (OT.Array r)
mapX f = ravelFromListX . map f . unravelToListX

zipWithX :: ADModeAndNum d r
         => (ADVal d (OT.Array r) -> ADVal d (OT.Array r)
             -> ADVal d (OT.Array r))
         -> ADVal d (OT.Array r) -> ADVal d (OT.Array r)
         -> ADVal d (OT.Array r)
zipWithX f d e =
  ravelFromListX $ zipWith f (unravelToListX d) (unravelToListX e)

reshapeX :: ADModeAndNum d r
         => OT.ShapeL -> ADVal d (OT.Array r) -> ADVal d (OT.Array r)
reshapeX sh' (D u u') = D (OT.reshape sh' u) (dReshapeX (OT.shapeL u) sh' u')

from0X :: ADModeAndNum d r => ADVal d r -> ADVal d (OT.Array r)
from0X (D u u') = D (OT.scalar u) (dFrom0X u')

from1X :: ADModeAndNum d r => ADVal d (Vector r) -> ADVal d (OT.Array r)
from1X (D u u') = D (OT.fromVector [V.length u] u) (dFrom1X u')

from2X :: ADModeAndNum d r => ADVal d (Matrix r) -> ADVal d (OT.Array r)
from2X (D u u') = D (OT.fromVector [HM.rows u, HM.cols u] $ HM.flatten u)
                    (dFrom2X u' (HM.cols u))

fromSX :: forall sh d r. (ADModeAndNum d r, OS.Shape sh)
       => ADVal d (OS.Array sh r) -> ADVal d (OT.Array r)
fromSX (D u u') = D (Data.Array.Convert.convert u) (dFromSX u')


#if defined(VERSION_ghc_typelits_natnormalise)
-- * Operations resulting in an arbitrary fully typed Shaped tensor

konstS :: (ADModeAndNum d r, OS.Shape sh)
       => ADVal d r -> ADVal d (OS.Array sh r)
konstS (D u u') = D (OS.constant u) (dKonstS u')

appendS :: (KnownNat m, KnownNat n, ADModeAndNum d r, OS.Shape sh)
        => ADVal d (OS.Array (m ': sh) r)
        -> ADVal d (OS.Array (n ': sh) r)
        -> ADVal d (OS.Array ((m + n) ': sh) r)
appendS (D u u') (D v v') = D (u `OS.append` v) (dAppendS u' v')

-- The API of this modules should not have proxies (but StaticNat instead).
-- However, lower level APIs are fine with Proxies. Not using StaticNat
-- there may even prevent mixing up high and mid or low APIs.
-- Or accessing low level APIs directly instead of via high level.
sliceS :: forall i n k rest d r.
          (KnownNat i, KnownNat n, KnownNat k, ADModeAndNum d r, OS.Shape rest)
       => ADVal d (OS.Array (i + n + k ': rest) r)
       -> ADVal d (OS.Array (n ': rest) r)
sliceS (D u u') = D (OS.slice @'[ '(i, n) ] u)
                    (dSliceS (Proxy :: Proxy i) Proxy u')

indexS :: forall ix k rest d r.
          (KnownNat ix, KnownNat k, ADModeAndNum d r, OS.Shape rest)
       => ADVal d (OS.Array (ix + 1 + k ': rest) r)
       -> ADVal d (OS.Array rest r)
indexS (D u u') = D (OS.index u (valueOf @ix))
                    (dIndexS u' (Proxy :: Proxy ix))

ravelFromListS :: forall rest k d r.
                  (KnownNat k, ADModeAndNum d r, OS.Shape rest)
               => [ADVal d (OS.Array rest r)]
               -> ADVal d (OS.Array (k : rest) r)
ravelFromListS ld =
  let (lu, lu') = unzip $ map (\(D u u') -> (u, u')) ld
  in D (OS.ravel $ OSB.fromList lu) (dRavelFromListS lu')

unravelToListS :: forall k rest d r.
                  (KnownNat k, ADModeAndNum d r, OS.Shape rest)
               => ADVal d (OS.Array (k : rest) r)
               -> [ADVal d (OS.Array rest r)]
unravelToListS (D v v') =
  -- @dIndexS@ is rigid, with type-level bound-checking, so we have to switch
  -- to @dIndexX@ for this function.
  let g ix p = D p (dFromXS $ dIndexX (dFromSX v') ix (valueOf @k))
  in imap g $ OSB.toList $ OS.unravel v

mapS :: forall k sh1 sh d r.
        (KnownNat k, ADModeAndNum d r, OS.Shape sh, OS.Shape sh1)
     => (ADVal d (OS.Array sh1 r) -> ADVal d (OS.Array sh r))
     -> ADVal d (OS.Array (k : sh1) r)
     -> ADVal d (OS.Array (k : sh) r)
mapS f = ravelFromListS . map f . unravelToListS

zipWithS :: forall k sh1 sh2 sh d r.
            ( KnownNat k, ADModeAndNum d r, OS.Shape sh, OS.Shape sh1, OS.Shape sh2)
         => (ADVal d (OS.Array sh1 r) -> ADVal d (OS.Array sh2 r)
             -> ADVal d (OS.Array sh r))
         -> ADVal d (OS.Array (k : sh1) r)
         -> ADVal d (OS.Array (k : sh2) r)
         -> ADVal d (OS.Array (k : sh) r)
zipWithS f d e =
  ravelFromListS $ zipWith f (unravelToListS d) (unravelToListS e)

reshapeS :: forall sh sh' d r.
            ( ADModeAndNum d r
            , OS.Shape sh, OS.Shape sh', OS.Size sh ~ OS.Size sh' )
         => ADVal d (OS.Array sh r) -> ADVal d (OS.Array sh' r)
reshapeS (D u u') = D (OS.reshape u) (dReshapeS u')

-- TODO: generalize as broadcast or stretch
asRowS :: forall k n d r. (ADModeAndNum d r, KnownNat k, KnownNat n)
       => ADVal d (OS.Array '[k] r) -> ADVal d (OS.Array '[n, k] r)
asRowS d = from2S $ asRow2 (fromS1 d) (valueOf @n)

asColumnS :: forall k n d r. (ADModeAndNum d r, KnownNat k, KnownNat n)
          => ADVal d (OS.Array '[k] r) -> ADVal d (OS.Array '[k, n] r)
asColumnS d = from2S $ asColumn2 (fromS1 d) (valueOf @n)

from0S :: ADModeAndNum d r => ADVal d r -> ADVal d (OS.Array '[] r)
from0S (D u u') = D (OS.scalar u) (dFrom0S u')

from1S :: (KnownNat n, ADModeAndNum d r)
       => ADVal d (Vector r) -> ADVal d (OS.Array '[n] r)
from1S (D u u') = D (OS.fromVector u) (dFrom1S u')

from2S :: (KnownNat rows, KnownNat cols, ADModeAndNum d r)
       => ADVal d (Matrix r) -> ADVal d (OS.Array '[rows, cols] r)
from2S (D u u') = D (OS.fromVector $ HM.flatten u) (dFrom2S Proxy u')

fromXS :: (ADModeAndNum d r, OS.Shape sh)
       => ADVal d (OT.Array r) -> ADVal d (OS.Array sh r)
fromXS (D u u') = D (Data.Array.Convert.convert u) (dFromXS u')

-- TODO: generalize to arbitrary permutations of arbitrarily many ranks using https://hackage.haskell.org/package/orthotope/docs/Data-Array-ShapedS.html#v:transpose
transpose2S :: (ADModeAndNum d r, KnownNat rows, KnownNat cols)
            => ADVal d (OS.Array '[rows, cols] r)
            -> ADVal d (OS.Array '[cols, rows] r)
transpose2S = from2S . transpose2 . fromS2

infixr 8 #>$
(#>$) :: (ADModeAndNum d r, KnownNat rows, KnownNat cols)
      => ADVal d (OS.Array '[rows, cols] r)
      -> ADVal d (OS.Array '[cols] r)
      -> ADVal d (OS.Array '[rows] r)
(#>$) d e = from1S $ fromS2 d #>! fromS1 e

infixr 8 <>$
(<>$) :: (ADModeAndNum d r, KnownNat m, KnownNat n, KnownNat p)
      => ADVal d (OS.Array '[m, n] r)
      -> ADVal d (OS.Array '[n, p] r)
      -> ADVal d (OS.Array '[m, p] r)
(<>$) d e = from2S $ fromS2 d <>! fromS2 e

conv2S :: forall d r kheight_minus_1 kwidth_minus_1 in_height in_width.
          ( KnownNat kheight_minus_1, KnownNat kwidth_minus_1
          , KnownNat in_height, KnownNat in_width
          , ADModeAndNum d r )
       => ADVal d (OS.Array '[kheight_minus_1 + 1, kwidth_minus_1 + 1] r)
       -> ADVal d (OS.Array '[in_height, in_width] r)
       -> ADVal d (OS.Array '[ in_height + kheight_minus_1
                                 , in_width + kwidth_minus_1 ] r)
conv2S ker x = from2S $ conv2' (fromS2 ker) (fromS2 x)

-- Convolution of many matrices at once. Some of the names of dimensions
-- are from https://www.tensorflow.org/api_docs/python/tf/nn/conv2d
conv24 :: forall kheight_minus_1 kwidth_minus_1
                 out_channels in_height in_width batch_size in_channels d r.
          ( KnownNat kheight_minus_1, KnownNat kwidth_minus_1
          , KnownNat out_channels, KnownNat in_height, KnownNat in_width
          , KnownNat batch_size, KnownNat in_channels
          , ADModeAndNum d r )
       => ADVal d (OS.Array '[ out_channels, in_channels
                                 , kheight_minus_1 + 1, kwidth_minus_1 + 1 ] r)
       -> ADVal d (OS.Array '[ batch_size, in_channels
                                  , in_height, in_width ] r)
       -> ADVal d (OS.Array '[ batch_size, out_channels
                                 , in_height + kheight_minus_1
                                 , in_width + kwidth_minus_1 ] r)
conv24 ker = mapS conv23 where
  conv23 :: ADVal d (OS.Array '[in_channels, in_height, in_width] r)
         -> ADVal d (OS.Array '[ out_channels
                                   , in_height + kheight_minus_1
                                   , in_width + kwidth_minus_1 ] r)
  conv23 x = mapS (convFilters x) ker
  convFilters
    :: ADVal d (OS.Array '[in_channels, in_height, in_width] r)
    -> ADVal d (OS.Array '[ in_channels
                              , kheight_minus_1 + 1, kwidth_minus_1 + 1 ] r)
    -> ADVal d (OS.Array '[ in_height + kheight_minus_1
                              , in_width + kwidth_minus_1 ] r)
  convFilters x ker1 = sumOutermost $ zipWithS conv2S ker1 x
  sumOutermost :: ADVal d (OS.Array '[ in_channels
                                         , in_height + kheight_minus_1
                                         , in_width + kwidth_minus_1 ] r)
               -> ADVal d (OS.Array '[ in_height + kheight_minus_1
                                         , in_width + kwidth_minus_1 ] r)
  sumOutermost = sum . unravelToListS
    -- slow; should go through Tensor2, or the Num instance should when possible

-- No proxies or anything similar needed here, but we may introduce StaticNat
-- regardless, if the implicitly passed tensor sizes become confusing
-- or if they start being passes explicitly via type application too often.
maxPool24
  :: forall ksize_minus_1 stride in_height in_width batch_size channels d r.
     ( KnownNat ksize_minus_1, KnownNat stride
     , KnownNat in_height, KnownNat in_width
     , KnownNat batch_size, KnownNat channels
     , 1 <= stride
     , ksize_minus_1 <= in_height
     , ksize_minus_1 <= in_width
     , 1 <= in_height - ksize_minus_1 + stride
     , 1 <= in_width - ksize_minus_1 + stride
     , ADModeAndNum d r )
     => ADVal d (OS.Array '[batch_size, channels, in_height, in_width] r)
     -> ADVal d
          (OS.Array '[ batch_size, channels
                     , (in_height - ksize_minus_1) `DivRoundUp` stride
                     , (in_width - ksize_minus_1) `DivRoundUp` stride ] r)
maxPool24 d =
  let res = mapS (mapS (from2S
                        . maxPool2 (valueOf @ksize_minus_1 + 1)
                                   (valueOf @stride)
                        . fromS2)) d
  in res
#endif

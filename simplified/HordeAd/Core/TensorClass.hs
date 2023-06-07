{-# LANGUAGE OverloadedLists, UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fno-specialise #-}
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module HordeAd.Core.TensorClass
  ( IndexOf, ShapeInt, Tensor(..), HasPrimal(..), ADReady
  ) where


import Prelude

import qualified Data.Array.Convert
import qualified Data.Array.DynamicS as OT
import           Data.Array.Internal (valueOf)
import qualified Data.Array.RankedS as OR
import           Data.Boolean
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.TypeLits (KnownNat, Nat, type (+))
import           Numeric.LinearAlgebra (Numeric)

import HordeAd.Core.SizedIndex
import HordeAd.Internal.TensorOps

import Data.Proxy
import GHC.TypeLits
import Data.Typeable
import Debug.Trace

-- * Tensor class definition

-- @IntOf r@ as size or shape gives more expressiveness,
-- but leads to irregular tensors, especially after vectorization,
-- and prevents statically known shapes.
-- However, if we switched to @Data.Array.Shaped@ and moved most of the shapes
-- to the type level, we'd recover some of the expressiveness, while retaining
-- statically known (type-parameterized) shapes.

-- | Thanks to the OverloadedLists mechanism, values of this type can be
-- written using the normal list notation. However, such values, if not
-- explicitly typed, do not inform the compiler about the length
-- of the list until runtime. That means that some errors are hidden
-- and also extra type applications may be needed to satisfy the compiler.
-- Therefore, there is a real trade-off between @[2]@ and @(2 :. ZI).
type IndexOf n r = Index n (IntOf r)

-- TODO: when we have several times more operations, split into
-- Array (Container) and Tensor (Numeric), with the latter containing the few
-- Ord and Num operations and numeric superclasses.
-- | The superclasses indicate that it's not only a container array,
-- but also a mathematical tensor, sporting numeric operations.
-- The @VectorNumeric@ superclass is for @IntOf@ and potential interoperability
-- (TODO: add coversions between VectorOf and TensorOf to facilitate this)
-- but all its operations have straightforwardly generalized analogues below.
-- Eventually, we'll remove @VectorNumeric@ or define it in terms of @Tensor@.
class ( RealFloat r, RealFloat (TensorOf 0 r), RealFloat (TensorOf 1 r)
      , Integral (IntOf r) )
      => Tensor r where
  type TensorOf (n :: Nat) r = result | result -> n r
  type IntOf r

  -- Integer codomain
  tshape :: KnownNat n => TensorOf n r -> ShapeInt n
  --trank :: forall n. KnownNat n => TensorOf n r -> Int
  --trank _ = valueOf @n
  --tsize :: KnownNat n => TensorOf n r -> Int
  --tsize = sizeShape . tshape
  tlength :: KnownNat n => TensorOf (1 + n) r -> Int
  tlength v = case tshape v of
    ZS -> error "tlength: impossible pattern needlessly required"
    k :$ _ -> k
  --tminIndex0 :: TensorOf 1 r -> IntOf r
  --tminIndex :: KnownNat n => TensorOf n r -> IndexOf n r
  --tminIndex t = fromLinearIdx (fmap fromIntegral $ tshape t)
  --                           (tminIndex0 (tflatten t))
  tmaxIndex0 :: TensorOf 1 r -> IntOf r
  tmaxIndex :: KnownNat n => TensorOf n r -> IndexOf n r
  tmaxIndex t = fromLinearIdx (fmap fromIntegral $ tshape t)
                              (tmaxIndex0 (tflatten t))
  --default tfloor  -- a more narrow type to rule out Ast
  --  :: IntOf r ~ Int => TensorOf 0 r -> IntOf r
  --tfloor = floor . tunScalar

  -- Typically scalar codomain, often tensor reduction
  -- (a number suffix in the name indicates the rank of codomain)
  tindex, (!) :: (KnownNat m, KnownNat n)
              => TensorOf (m + n) r -> IndexOf m r -> TensorOf n r
  infixl 9 !
  (!) = tindex  -- prefix form better when type applications are necessary
--  tsum :: KnownNat n => TensorOf (1 + n) r -> TensorOf n r
--  tsum0 :: KnownNat n => TensorOf n r -> TensorOf 0 r
--  tsum0 = tsum . tflatten
--  tdot0 :: KnownNat n => TensorOf n r -> TensorOf n r -> TensorOf 0 r
--  tdot0 t u = tsum (tflatten t * tflatten u)
--  tmatmul1 :: TensorOf 2 r -> TensorOf 1 r -> TensorOf 1 r
--  tmatmul1 m v = tbuild1 (tlength m) (\i -> tdot0 v (m ! [i]))
-- how to generalize (#69)? these equivalent definitions generalize differently:
-- tmatmul1 m v = tbuild1 (tlength m) (\i -> tsum (v * m ! [i]))
-- tmatmul1 m v = tflatten $ tmap1 (tkonst 1 . tdot0 v) m
--  tmatmul2 :: TensorOf 2 r -> TensorOf 2 r -> TensorOf 2 r
--  tmatmul2 m1 m2 = tmap1 (tmatmul1 (ttr m2)) m1
  --tminimum :: KnownNat n => TensorOf n r -> TensorOf 0 r
  --tminimum t = t ! tminIndex t
--  tmaximum :: KnownNat n => TensorOf n r -> TensorOf 0 r
--  tmaximum t = t ! tmaxIndex t
  --tfromIndex0 :: IntOf r -> TensorOf 0 r
  --default tfromIndex0  -- the more narrow type rules out Ast
  --  :: IntOf r ~ Int => IntOf r -> TensorOf 0 r
  --tfromIndex0 = tscalar . fromIntegral
--  tfromIndex1 :: IndexOf n r -> TensorOf 1 r
--  tfromIndex1 = tfromList . map tfromIndex0 . indexToList
  -- TODO: scatter doesn't yet vectorize, so it's only for internal use
  {-
  tscatter :: (KnownNat m, KnownNat n, KnownNat p)
           => ShapeInt (p + n) -> TensorOf (m + n) r
           -> (IndexOf m r -> IndexOf p r)
           -> TensorOf (p + n) r
  tscatter1 :: (KnownNat n, KnownNat p)
            => ShapeInt (p + n) -> TensorOf (1 + n) r
            -> (IntOf r -> IndexOf p r)
            -> TensorOf (p + n) r
  tscatter1 sh v f = tscatter @r @1 sh v
                                    (\(i :. ZI) -> f i)
                                    -}

  -- Tensor codomain, often tensor construction, sometimes transformation
  -- (for these, suffix 1 doesn't mean codomain rank 1, but building up
  -- by one rank, and is omitted if a more general variant is not defined)
  tfromList :: KnownNat n => [TensorOf n r] -> TensorOf (1 + n) r
  tfromList0N :: KnownNat n => ShapeInt n -> [r] -> TensorOf n r
  tfromList0N sh = treshape sh . tfromList . map tscalar
  tfromVector :: KnownNat n
              => Data.Vector.Vector (TensorOf n r) -> TensorOf (1 + n) r
  tfromVector v = tfromList (V.toList v)  -- horribly inefficient for large vs
  tfromVector0N :: KnownNat n
                => ShapeInt n -> Data.Vector.Vector r -> TensorOf n r
  tfromVector0N sh = treshape sh . tfromVector . V.map tscalar
  tkonst :: KnownNat n => Int -> TensorOf n r -> TensorOf (1 + n) r
  tkonst0N :: KnownNat n => ShapeInt n -> TensorOf 0 r -> TensorOf n r
  tkonst0N sh = treshape sh . tkonst (sizeShape sh)
  tappend :: KnownNat n
          => TensorOf (1 + n) r -> TensorOf (1 + n) r -> TensorOf (1 + n) r
  tslice :: KnownNat n => Int -> Int -> TensorOf (1 + n) r -> TensorOf (1 + n) r
  treverse :: KnownNat n => TensorOf (1 + n) r -> TensorOf (1 + n) r
  ttr :: KnownNat n => TensorOf (2 + n) r -> TensorOf (2 + n) r
  ttr = ttranspose [1, 0]
  ttranspose :: KnownNat n => Permutation -> TensorOf n r -> TensorOf n r
  tflatten :: KnownNat n => TensorOf n r -> TensorOf 1 r
  tflatten u = treshape (flattenShape $ tshape u) u
  treshape :: (KnownNat n, KnownNat m)
           => ShapeInt m -> TensorOf n r -> TensorOf m r
  tbuild :: forall m n. (KnownNat m, KnownNat n)
         => ShapeInt (m + n) -> (IndexOf m r -> TensorOf n r)
         -> TensorOf (m + n) r
  tbuild sh0 f0 =
    let buildSh :: KnownNat m1
                => ShapeInt m1 -> (IndexOf m1 r -> TensorOf n r)
                -> TensorOf (m1 + n) r
        buildSh ZS f = f ZI
        buildSh (k :$ sh) f = tbuild1 k (\i -> buildSh sh (\ix -> f (i :. ix)))
    in buildSh (takeShape @m @n sh0) f0
  tbuild1 :: KnownNat n  -- this form requires less type applications
          => Int -> (IntOf r -> TensorOf n r) -> TensorOf (1 + n) r
  tmap :: (KnownNat m, KnownNat n)
       => (TensorOf n r -> TensorOf n r)
       -> TensorOf (m + n) r -> TensorOf (m + n) r
  tmap f v = tbuild (tshape v) (\ix -> f (v ! ix))
  tmap1 :: KnownNat n
        => (TensorOf n r -> TensorOf n r)
        -> TensorOf (1 + n) r -> TensorOf (1 + n) r
  tmap1 f u = tbuild1 (tlength u) (\i -> f (u ! [i]))
  tmap0N :: KnownNat n
         => (TensorOf 0 r -> TensorOf 0 r) -> TensorOf n r -> TensorOf n r
  tmap0N f v = tbuild (tshape v) (\ix -> f $ v ! ix)
  tzipWith :: (KnownNat m, KnownNat n)
           => (TensorOf n r -> TensorOf n r -> TensorOf n r)
           -> TensorOf (m + n) r -> TensorOf (m + n) r -> TensorOf (m + n) r
  tzipWith f u v = tbuild (tshape v) (\ix -> f (u ! ix) (v ! ix))
  tzipWith1 :: KnownNat n
            => (TensorOf n r -> TensorOf n r -> TensorOf n r)
            -> TensorOf (1 + n) r -> TensorOf (1 + n) r -> TensorOf (1 + n) r
  tzipWith1 f u v = tbuild1 (tlength u) (\i -> f (u ! [i]) (v ! [i]))
  tzipWith0N :: KnownNat n
             => (TensorOf 0 r -> TensorOf 0 r -> TensorOf 0 r)
             -> TensorOf n r -> TensorOf n r -> TensorOf n r
  tzipWith0N f u v = tbuild (tshape v) (\ix -> f (u ! ix) (v ! ix))
  {-
  tgather :: (KnownNat m, KnownNat n, KnownNat p)
          => ShapeInt (m + n) -> TensorOf (p + n) r
          -> (IndexOf m r -> IndexOf p r)
          -> TensorOf (m + n) r
          -}

          {-
  tgather1 :: (KnownNat n, KnownNat p)
           => Int -> TensorOf (p + n) r
           -> (IntOf r -> IndexOf p r)
           -> TensorOf (1 + n) r
  tgather1 k v f = tgather @r @1 (k :$ dropShape (tshape v)) v
                                 (\(i :. ZI) -> f i)
                                 -}

  tscalar :: r -> TensorOf 0 r
  --tunScalar :: TensorOf 0 r -> r

  -- Needed to avoid Num (TensorOf n r) constraints all over the place
  -- and also wrong shape in @0@ with ranked (not shaped) tensors.
  --tzero :: KnownNat n
  --      => ShapeInt n -> TensorOf n r
  --tzero sh = tkonst0N sh 0

type ADReady r =
  ( Tensor r, HasPrimal r, Tensor (Primal r), Show r
    , OrdB (TensorOf 12 (Primal r)
  ))
  -- any of the @BooleanOf r ~ ...@ lines above breaks GHC <= 9.0.2

-- * Tensor class instances for arrays

-- These instances are a faster way to get an objective function value.
-- However, they don't do vectorization, so won't work on GPU, ArrayFire, etc.
-- For vectorization, go through Ast and valueOnDomains.
instance Tensor Double where
  type TensorOf n Double = OR.Array n Double
  type IntOf Double = Int
--  tshape = tshapeR
--  tminIndex0 = tminIndexR
-- tmaxIndex0 = tmaxIndexR
--  tfloor = floor . tunScalar
--  tindex = tindexZR
--  tsum = tsumR
--  tsum0 = tscalar . tsum0R
  --tdot0 u v = tscalar $ tdot0R u v
--  tscatter = tscatterNR
--  tscatter1 = tscatter1R
--  tfromList = tfromListR
--  tfromList0N = tfromList0NR
--  tfromVector = tfromVectorR
--  tfromVector0N = tfromVector0NR
--  tkonst = tkonstR
  --tkonst0N sh = tkonst0NR sh . tunScalar
--  tappend = tappendR
{-
  tslice = tsliceR
  treverse = treverseR
  ttranspose = ttransposeR
  treshape = treshapeR
  tbuild = tbuildNR
  tbuild1 = tbuild1R
  -}
  {-
  tgather = tgatherZR
  tgather1 = tgatherZ1R
  tscalar = tscalarR
  tunScalar = tunScalarR
  -}



-- * HasPrimal class and instances for all relevant types

-- We could accept any @RealFloat@ instead of @Primal a@, but then
-- we'd need to coerce, e.g., via realToFrac, which is risky and lossy.
-- Also, the stricter typing is likely to catch real errors most of the time,
-- not just sloppy omission ofs explicit coercions.
class HasPrimal r where
  type ScalarOf r
  type Primal r
  type DualOf (n :: Nat) r
  tconst :: KnownNat n => OR.Array n (ScalarOf r) -> TensorOf n r
  tconstant :: KnownNat n => TensorOf n (Primal r) -> TensorOf n r
  tprimalPart :: TensorOf n r -> TensorOf n (Primal r)
  tdualPart :: TensorOf n r -> DualOf n r
  tD :: KnownNat n => TensorOf n (Primal r) -> DualOf n r -> TensorOf n r
  -- TODO: we'd probably also need dZero, dIndex0 and all others;
  -- basically DualOf a needs to have IsPrimal and HasRanks instances
  -- (and HasInputs?)
  -- TODO: if DualOf is supposed to be user-visible, we needed
  -- a better name for it; TangentOf? CotangentOf? SecondaryOf?

  type DynamicTensor r = result | result -> r
  tdummyD :: DynamicTensor r
  tisDummyD :: DynamicTensor r -> Bool
  taddD :: DynamicTensor r -> DynamicTensor r -> DynamicTensor r
  tfromR :: KnownNat n
         => TensorOf n r -> DynamicTensor r
  tfromD :: KnownNat n
         => DynamicTensor r -> TensorOf n r

instance HasPrimal Double where
  type ScalarOf Double = Double
  type Primal Double = Double
  type DualOf n Double = ()
  tconst = id
  tconstant = id
  tprimalPart = id
  tdualPart _ = ()
  tD u _ = u
  type DynamicTensor Double = OT.Array Double
--  tdummyD = dummyTensor
--  tisDummyD = isTensorDummy
  taddD = (+)
  tfromR = Data.Array.Convert.convert
  tfromD = Data.Array.Convert.convert


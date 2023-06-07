{-# LANGUAGE OverloadedLists, UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module HordeAd.Core.ADValTensor ()
   where



import Prelude

import qualified Data.Array.RankedS as OR

import HordeAd.Core.DualClass ( dBuild1 )
import HordeAd.Core.DualNumber ( ADVal(..), ADModeAndNum(..), dD )
import HordeAd.Core.TensorClass (Tensor(..))
import HordeAd.Internal.TensorOps (tbuild1R)


instance ADModeAndNum d Double => Tensor (ADVal d Double) where
  type TensorOf n (ADVal d Double) = ADVal d (OR.Array n Double)
  type IntOf (ADVal d Double) = Int
  tbuild1 k f =
    let g i = let D u _ = f i in u
        h i = let D _ u' = f i in u'
    in dD (tbuild1R k g) (dBuild1 k h)


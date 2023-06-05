{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-spec -ddump-rule-rewrites  #-}
module TestHighRankSimplified where

import Prelude

import qualified Data.Array.RankedS as OR
import           GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)

import HordeAd
import HordeAd.Core.DualClass (inputConstant)

import Tool.EqEpsilon
import Debug.Trace
import GHC.TypeLits
import Data.Typeable
import Data.Proxy

spec1 ::  forall m n. (KnownNat m, KnownNat n)
         => ShapeInt (m + n) -> (IndexOf m (ADVal ADModeGradient Double) -> TensorOf n (ADVal ADModeGradient Double))
         -> TensorOf (m + n) (ADVal ADModeGradient Double)
spec1 a b = tbuild @(ADVal ADModeGradient Double) a b

spec2 ::  ShapeInt (33 + 0) -> (IndexOf 33 (ADVal ADModeGradient Double) -> TensorOf 0 (ADVal ADModeGradient Double))
         -> TensorOf (33 + 0) (ADVal ADModeGradient Double)
spec2 a b = tbuild @(ADVal ADModeGradient Double) @33 @0 a b

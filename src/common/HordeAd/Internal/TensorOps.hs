{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Miscellaneous more or less general purpose tensor operations.
module HordeAd.Internal.TensorOps
  ( module HordeAd.Internal.TensorOps
  ) where

import Prelude

import           Control.Arrow (first, second)
import           Control.Exception.Assert.Sugar
import qualified Data.Array.Convert
import qualified Data.Array.DynamicS as OT
import           Data.Array.Internal (valueOf)
import qualified Data.Array.Internal
import qualified Data.Array.Internal.DynamicG
import qualified Data.Array.Internal.DynamicS
import qualified Data.Array.Internal.RankedG
import qualified Data.Array.Internal.RankedS
import qualified Data.Array.Internal.ShapedG
import qualified Data.Array.Internal.ShapedS
import qualified Data.Array.Ranked as ORB
import qualified Data.Array.RankedS as OR
import qualified Data.Array.ShapedS as OS
import           Data.List (foldl')
import qualified Data.Strict.Map as M
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.TypeLits (KnownNat, type (+))
import           Numeric.LinearAlgebra (Matrix, Numeric, Vector)
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Devel

import HordeAd.Core.SizedIndex
import HordeAd.Internal.OrthotopeOrphanInstances (liftVR, liftVR2)
import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import Debug.Trace
import GHC.Stack

type IndexInt n = Index n Int

tbuild1R
  :: (KnownNat n, Numeric r)
  => Int -> (Int -> OR.Array n r) -> OR.Array (1 + n) r
tbuild1R k f = OR.ravel $ ORB.fromList [k]
               $ map f [0 .. k - 1]  -- hope this fuses


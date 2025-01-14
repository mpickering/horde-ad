-- | Tools for implementing (and debugging the use of) gradient descent schemes.
module HordeAd.External.OptimizerTools
  ( updateWithGradient, updateWithGradientR
--  , gradientIsNil, minimumGradient, maximumGradient
  , ArgsAdam(..), defaultArgsAdam
  , StateAdam(..), initialStateAdam
  , updateWithGradientAdam
  ) where

import Prelude

import           Control.Monad.ST.Strict (runST)
import qualified Data.Array.DynamicS as OD
import qualified Data.Array.RankedS as OR
import           Data.Bifunctor.Flip
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import           Numeric.LinearAlgebra (Numeric, Vector)
import qualified Numeric.LinearAlgebra as LA

import HordeAd.Core.Domains
import HordeAd.Core.TensorClass
import HordeAd.Internal.OrthotopeOrphanInstances (liftVR, liftVR2, liftVT2)
import HordeAd.Internal.TensorOps (isTensorDummy)

updateWithGradient
  :: ( Numeric r, Show r, Floating (Vector r), Tensor r, DomainsCollection r
     , DTensorOf r ~ OD.Array r, TensorOf 1 r ~ Flip OR.Array r 1 )
  => r -> Domains r -> Domains r -> Domains r
updateWithGradient gamma params gradient =
  let params0 = domains0 params
      paramsR = domainsR params
      gradient0 = domains0 gradient
      gradientR = domainsR gradient
      updateVector i r = i - LA.scale gamma r
      !params0New = Flip $ liftVR2 updateVector (runFlip params0)
                                                (runFlip gradient0)
      updateR i r = if isTensorDummy r  -- eval didn't update it, would crash
                    then i
                    else liftVT2 updateVector i r
      !paramsRNew = V.zipWith updateR paramsR gradientR
  in mkDomains params0New paramsRNew
{-# SPECIALIZE updateWithGradient :: Double -> Domains Double -> Domains Double -> Domains Double #-}

updateWithGradientR
  :: ( Numeric r, Floating (Vector r), DTensorOf r ~ OD.Array r
     , DomainsCollection r )
  => r -> Domains r -> Domains r -> Domains r
updateWithGradientR gamma params gradient =
  let updateVector i r = i - LA.scale gamma r
      updateR i r = if isTensorDummy r  -- eval didn't update it, would crash
                    then i
                    else liftVT2 updateVector i r
  in fromVectorDoms
     $ V.zipWith updateR (toVectorDoms params) (toVectorDoms gradient)
{-# SPECIALIZE updateWithGradientR :: Double -> Domains Double -> Domains Double -> Domains Double #-}

{-
gradientIsNil :: (Eq r, Numeric r) => Domains r -> Bool
gradientIsNil (Domains gradient0 gradientR) =
  V.all (== 0) gradient0
  && V.all isTensorDummy gradientR

minimumGradient :: (Ord r, Numeric r) => Domains r -> r
minimumGradient (Domains gradient0 gradientR) =
  min (if V.null gradient0 then 0 else LA.minElement gradient0)
      (if V.null gradientR then 0
       else V.minimum (V.map OD.minimumA gradientR))

maximumGradient :: (Ord r, Numeric r) => Domains r -> r
maximumGradient (Domains gradient0 gradientR) =
  max (if V.null gradient0 then 0 else LA.maxElement gradient0)
      (if V.null gradientR then 0
       else V.maximum (V.map OD.maximumA gradientR))
-}

data ArgsAdam r = ArgsAdam
  { alpha   :: r
  , betaOne :: r
  , betaTwo :: r
  , epsilon :: r
  }

-- The defaults taken from
-- https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adam
defaultArgsAdam :: Fractional r => ArgsAdam r
defaultArgsAdam = ArgsAdam
  { alpha = 0.001
  , betaOne = 0.9
  , betaTwo = 0.999
  , epsilon = 1e-7
  }

data StateAdam r = StateAdam
  { tAdam :: Int  -- iteration count
  , mAdam :: Domains r
  , vAdam :: Domains r
  }

-- The arguments are just sample params0, for dimensions.
zeroParameters
  :: ( Numeric r, DTensorOf r ~ OD.Array r, TensorOf 1 r ~ Flip OR.Array r 1
     , Tensor r, DomainsCollection r )
  => Domains r -> Domains r
zeroParameters params =
  let zeroVector v = runST $ do
        vThawed <- V.thaw v
        VM.set vThawed 0
        V.unsafeFreeze vThawed
  in mkDomains (Flip $ liftVR zeroVector (runFlip $ domains0 params))
               (V.map (\a -> OD.constant (OD.shapeL a) 0) (domainsR params))

initialStateAdam
  :: ( Numeric r, DTensorOf r ~ OD.Array r, TensorOf 1 r ~ Flip OR.Array r 1
     , Tensor r, DomainsCollection r )
  => Domains r -> StateAdam r
initialStateAdam parameters0 =
  let zeroP = zeroParameters parameters0
  in StateAdam
       { tAdam = 0
       , mAdam = zeroP
       , vAdam = zeroP
       }

-- TOOD: make sure this is not worse that OD.zipWith3A when transposing
-- between each application or that we never encounter such situations
--
-- | Application of a vector function on the flattened arrays elements.
liftArray43 :: ( Numeric a, Numeric b, Numeric c, Numeric d
               , Numeric x, Numeric y, Numeric z )
            => (Vector a -> Vector b -> Vector c -> Vector d
                -> (Vector x, Vector y, Vector z))
            -> OD.Array a -> OD.Array b -> OD.Array c -> OD.Array d
            -> (OD.Array x, OD.Array y, OD.Array z)
liftArray43 f m1 m2 m3 m4 =
  let sz = OD.shapeL m1
  in if sz == OD.shapeL m2 && sz == OD.shapeL m3 && sz == OD.shapeL m4
     then let (vx, vy, vz) = f (OD.toVector m1) (OD.toVector m2)
                               (OD.toVector m3) (OD.toVector m4)
          in ( OD.fromVector sz vx
             , OD.fromVector sz vy
             , OD.fromVector sz vz
             )
     else error
          $ "nonconformant arrays in liftArray43: "
            ++ show (OD.shapeL m1, OD.shapeL m2, OD.shapeL m3, OD.shapeL m4)

updateWithGradientAdam
  :: forall r.
     ( Numeric r, Floating (Vector r)
     , Tensor r, DomainsCollection r
     , DTensorOf r ~ OD.Array r, TensorOf 1 r ~ Flip OR.Array r 1 )
  => ArgsAdam r -> StateAdam r -> Domains r -> Domains r
  -> (Domains r, StateAdam r)
updateWithGradientAdam ArgsAdam{..} StateAdam{tAdam, mAdam, vAdam}
                       params gradient =
  let mAdam0 = domains0 mAdam
      mAdamR = domainsR mAdam
      vAdam0 = domains0 vAdam
      vAdamR = domainsR vAdam
      params0 = domains0 params
      paramsR = domainsR params
      gradient0 = domains0 gradient
      gradientR = domainsR gradient
      tAdamNew = tAdam + 1
      oneMinusBeta1 = 1 - betaOne
      oneMinusBeta2 = 1 - betaTwo
      updateVector :: Vector r -> Vector r
                   -> Vector r -> Vector r
                   -> (Vector r, Vector r, Vector r)
      updateVector mA vA p g =
        let mANew = LA.scale betaOne mA + LA.scale oneMinusBeta1 g
            vANew = LA.scale betaTwo vA + LA.scale oneMinusBeta2 (g * g)
            alphat = alpha * sqrt (1 - betaTwo ^ tAdamNew)
                             / (1 - betaOne ^ tAdamNew)
        in ( mANew
           , vANew
           , p - LA.scale alphat mANew
                 / (sqrt vANew + LA.scalar epsilon) )  -- the @scalar@ is safe
                      -- @addConstant@ would be better, but it's not exposed
      (!mAdam0New, !vAdam0New, !params0New) =
        updateVector
          (OR.toVector $ runFlip mAdam0) (OR.toVector $ runFlip vAdam0)
          (OR.toVector $ runFlip params0) (OR.toVector $ runFlip gradient0)
      updateR mA vA p g = if isTensorDummy g  -- eval didn't update it
                          then (mA, vA, p)
                          else liftArray43 updateVector mA vA p g
      (!mAdamRNew, !vAdamRNew, !paramsRNew) =
        V.unzip3 $ V.zipWith4 updateR mAdamR vAdamR paramsR gradientR
  in ( mkDomains (Flip $ OR.fromVector [V.length params0New] params0New)
                 paramsRNew
     , StateAdam
         { tAdam = tAdamNew
         , mAdam = mkDomains
                     (Flip $ OR.fromVector [V.length mAdam0New] mAdam0New)
                     mAdamRNew
         , vAdam = mkDomains
                     (Flip $ OR.fromVector [V.length vAdam0New] vAdam0New)
                     vAdamRNew
         }
     )

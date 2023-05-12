{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module TestHighRankSimplified (testTrees) where

import Prelude

import qualified Data.Array.RankedS as OR
import           GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)

import HordeAd
import HordeAd.Core.DualClass (inputConstant)
import Data.Proxy
import GHC.TypeLits
import Debug.Trace
import Tool.EqEpsilon
import GHC.Stack
import Data.Proxy
import GHC.TypeLits
import Data.Typeable
import Data.Typeable

rev' :: forall a r n m.
        ( KnownNat n, KnownNat m, HasDelta r, ADReady r
        , a ~ OR.Array m r, ScalarOf r ~ r
        , TensorOf n r ~ OR.Array n r
        , TensorOf n (ADVal 'ADModeGradient r)
          ~ ADVal 'ADModeGradient (OR.Array n r)
        , TensorOf m (ADVal 'ADModeGradient r)
          ~ ADVal 'ADModeGradient (OR.Array m r)
        , ADReady (ADVal 'ADModeGradient r), HasCallStack)
     => (forall x. ADReady x => TensorOf n x -> TensorOf m x)
     -> OR.Array n r
     -> ( TensorOf m r, a )
rev' f vals =
 let ?callStack = pushCallStack (show ("TR_top_2", typeRep (Proxy @(n)), natVal (Proxy @(n)), typeRep (Proxy @m), natVal (Proxy @m)), SrcLoc "" "" "" 0 0 0 0) callStack
 in
 let value0 = traceShow ?callStack f vals
     dt = inputConstant @a 1
     g inputs = f $ parseADInputs vals inputs
     (_, value1) = revOnDomainsFun dt g (toDomains vals)
  in traceShow ?callStack ( value0, value1 )

assertEqualUpToEpsilon'
    :: ( AssertEqualUpToEpsilon z b
       , HasCallStack )
    => z  -- ^ error margin (i.e., the epsilon)
    -> (b, b)
         -- ^ actual values
    -> Assertion
assertEqualUpToEpsilon'
    errMargin
    ( value0, value1 ) = do
  assertEqualUpToEpsilonWithMark "Val ADVal" errMargin value1 value1

testTrees :: [TestTree]
testTrees =
  [ testCase "3nestedBuildMap7" testNestedBuildMap7
  , testCase "3concatBuild" testConcatBuild
  ]

nestedBuildMap :: forall n r. (ADReady r, n <= 77, KnownNat n)
               => TensorOf 0 r -> TensorOf n r
nestedBuildMap r =
  tmap0N id $ tkonst0N (takeShape @n @(114 - n)
                                  (2 :$ 4 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ 2 :$ 4 :$ 2 :$ 1 :$ 3 :$ 2 :$ ZS))
                       (tindex (tkonst 1 r) (0 :. ZI))

rep1 = typeNatTypeRep @1

rep2 = typeNatTypeRep @2

testNestedBuildMap7 :: Assertion
testNestedBuildMap7 = traceShow (rep1, rep2) $
  assertEqualUpToEpsilon 1e-8
    2176.628439128524
    (rev @(OR.Array 32 Double) nestedBuildMap 0.6)




concatBuild :: forall n r . (ADReady r, KnownNat (1 + n), KnownNat (2 + n), HasCallStack)
            => TensorOf (1 + n) r -> TensorOf (3 + n) r
concatBuild r =
  let ?callStack = pushCallStack (show ("TR_top", typeRep (Proxy @(1 + n)), natVal (Proxy @(1 + n))), SrcLoc "" "" "" 0 0 0 0) callStack
  in traceShow ?callStack (tkonst 1 (tkonst 1 (tmap0N id r)))

testConcatBuild :: Assertion
testConcatBuild = traceShow ("reps", rep1, rep2) $
  assertEqualUpToEpsilon' 1e-10
    (rev' @(OR.Array 3 Double) @Double @1 @3 concatBuild (tkonst 7 3.4))

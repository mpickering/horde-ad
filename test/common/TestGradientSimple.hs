{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleInstances,
             FunctionalDependencies, RankNTypes, TypeFamilies,
             TypeOperators #-}
module TestGradientSimple (testTrees, finalCounter) where

import Prelude

import           Control.Arrow (first)
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           System.IO (hPutStrLn, stderr)
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Text.Printf

import HordeAd hiding (sumElementsVectorOfDual)
import HordeAd.Core.DualClass (unsafeGetFreshId)

import Tool.EqEpsilon
import Tool.Shared

testTrees :: [TestTree]
testTrees = [ testDReverse0
            , testDReverse1
            , testPrintDf
            , testDForward
            , testDFastForward
            , quickCheckForwardAndBackward
            , oldReadmeTests
            , oldReadmeTestsV
            ]

revIO0
  :: HasDelta r
  => (ADInputs 'ADModeGradient r
      -> ADVal 'ADModeGradient r)
  -> [r]
  -> IO ([r], r)
revIO0 f deltaInput = do
  ((!results, _), !v) <-
    first domainsTo01
    <$> revIO 1 f (domainsFrom01 (V.fromList deltaInput) V.empty)
  return (V.toList results, v)

fX :: ADInputs 'ADModeGradient Float
   -> ADVal 'ADModeGradient Float
fX inputs = at0 inputs 0

fXp1 :: ADInputs 'ADModeGradient Float
     -> ADVal 'ADModeGradient Float
fXp1 inputs =
  let x = at0 inputs 0
  in x + 1

fXpX :: ADInputs 'ADModeGradient Float
     -> ADVal 'ADModeGradient Float
fXpX inputs =
  let x = at0 inputs 0
  in x + x

fXX :: ADInputs 'ADModeGradient Float
    -> ADVal 'ADModeGradient Float
fXX inputs =
  let x = at0 inputs 0
  in x * x

fX1X :: ADInputs 'ADModeGradient Float
     -> ADVal 'ADModeGradient Float
fX1X inputs =
  let x = at0 inputs 0
      x1 = x + 1
  in x1 * x

fX1Y :: ADInputs 'ADModeGradient Float
     -> ADVal 'ADModeGradient Float
fX1Y inputs =
  let x = at0 inputs 0
      y = at0 inputs 1
      x1 = x + 1
  in x1 * y

fY1X :: ADInputs 'ADModeGradient Float
     -> ADVal 'ADModeGradient Float
fY1X inputs =
  let x = at0 inputs 0
      y = at0 inputs 1
      x1 = y + 1
  in x1 * x

fXXY ::  ADInputs 'ADModeGradient Float
     -> ADVal 'ADModeGradient Float
fXXY inputs =
  let x = at0 inputs 0
      y = at0 inputs 1
      xy = x * y
  in x * xy

fXYplusZ :: ADInputs 'ADModeGradient Float
         -> ADVal 'ADModeGradient Float
fXYplusZ inputs =
  let x = at0 inputs 0
      y = at0 inputs 1
      z = at0 inputs 2
      xy = x * y
  in xy + z

fXtoY :: ADInputs 'ADModeGradient Float
      -> ADVal 'ADModeGradient Float
fXtoY inputs =
  let x = at0 inputs 0
      y = at0 inputs 1
  in x ** y

freluX :: ADInputs 'ADModeGradient Float
       -> ADVal 'ADModeGradient Float
freluX inputs =
  let x = at0 inputs 0
  in relu x

testDReverse0 :: TestTree
testDReverse0 = testGroup "Simple revIO application tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ do
          res <- revIO0 f v
          res @?~ expected)
    [ ("fX", fX, [99], ([1.0],99.0))
    , ("fXagain", fX, [99], ([1.0],99.0))
    , ("fXp1", fXp1, [99], ([1.0],100))
    , ("fXpX", fXpX, [99], ([2.0],198))
    , ("fXX", fXX, [2], ([4],4))
    , ("fX1X", fX1X, [2], ([5],6))
    , ("fX1Y", fX1Y, [3, 2], ([2.0,4.0],8.0))
    , ("fY1X", fY1X, [2, 3], ([4.0,2.0],8.0))
    , ("fXXY", fXXY, [3, 2], ([12.0,9.0],18.0))
    , ("fXYplusZ", fXYplusZ, [1, 2, 3], ([2.0,1.0,1.0],5.0))
    , ( "fXtoY", fXtoY, [0.00000000000001, 2]
      , ([2.0e-14,-3.2236188e-27],9.9999994e-29) )
    , ("fXtoY2", fXtoY, [1, 2], ([2.0,0.0],1.0))
    , ("freluX", freluX, [-1], ([0.0],0.0))
    , ("freluX2", freluX, [0], ([0.0],0.0))
    , ("freluX3", freluX, [0.0001], ([1.0],1.0e-4))
    , ("freluX4", freluX, [99], ([1.0],99.0))
    , ("fquad", fquad, [2, 3], ([4.0,6.0],18.0))
    , ("scalarSum", vec_scalarSum_aux, [1, 1, 3], ([1.0,1.0,1.0],5.0))
    ]

vec_scalarSum_aux
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
vec_scalarSum_aux = foldlDual' (+) 0

sumElementsV
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
sumElementsV inputs =
  let x = at1 inputs 0
  in sumElements0 x

altSumElementsV
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
altSumElementsV inputs =
  let x = at1 inputs 0
  in altSumElements0 x

-- hlint would complain about spurious @id@, so we need to define our own.
id2 :: a -> a
id2 x = x

sinKonst
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
sinKonst inputs =
  let x = at1 inputs 0
  in sumElements0 $
       sin x + (id2 $ id2 $ id2 $ konst1 1 2)

powKonst
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
powKonst inputs =
  let x = at1 inputs 0
  in sumElements0 $
       x ** (sin x + (id2 $ id2 $ id2 $ konst1 (sumElements0 x) 2))

dReverse1
  :: (r ~ Float, d ~ 'ADModeGradient)
  => (ADInputs d r -> ADVal d r)
  -> [[r]]
  -> IO ([[r]], r)
dReverse1 f deltaInput = do
  ((_, !results), !v) <-
    first domainsTo01
    <$> revIO 1 f
          (domainsFrom01 V.empty (V.fromList (map V.fromList deltaInput)))
  return (map V.toList $ V.toList results, v)

testDReverse1 :: TestTree
testDReverse1 = testGroup "Simple revIO application to vectors tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ do
          res <- dReverse1 f v
          res @?~ expected)
    [ ("sumElementsV", sumElementsV, [[1, 1, 3]], ([[1.0,1.0,1.0]],5.0))
    , ("altSumElementsV", altSumElementsV, [[1, 1, 3]], ([[1.0,1.0,1.0]],5.0))
    , ( "sinKonst", sinKonst, [[1, 3]]
      , ([[0.5403023,-0.9899925]],2.982591) )
    , ( "powKonst", powKonst, [[1, 3]]
      , ([[108.7523,131.60072]],95.58371) )
    ]

testPrintDf :: TestTree
testPrintDf = testGroup "Pretty printing test" $
  map (\(txt, f, v, expected) ->
        testCase txt $ do
          let output =
                prettyPrintDf f
                  (domainsFrom01 V.empty (V.fromList (map V.fromList v)))
          length output @?= expected)
    [ ( "sumElementsV", sumElementsV, [[1 :: Float, 1, 3]]
      , 54 )
    , ( "altSumElementsV", altSumElementsV, [[1, 1, 3]]
      , 337 )
    , ( "sinKonst", sinKonst, [[1, 3]]
      , 237 )
    , ( "powKonst", powKonst, [[1, 3]]
      , 692 )
    ]

testDForward :: TestTree
testDForward =
 testGroup "Simple slowFwd application tests" $
  map (\(txt, f, v, expected) ->
        let vp = listsToParameters v
        in testCase txt $ do
          res <- slowFwd vp f vp
          res @?~ expected)
    [ ("fquad", fquad, ([2 :: Double, 3], []), (26.0, 18.0))
    , ( "atanOldReadme", atanOldReadme, ([1.1, 2.2, 3.3], [])
      , (7.662345305800865, 4.9375516951604155) )
    , ( "vatanOldReadme", vatanOldReadme, ([], [1.1, 2.2, 3.3])
      , (7.662345305800865, 4.9375516951604155) )
    ]

testDFastForward :: TestTree
testDFastForward =
 testGroup "Simple fwdFun application tests" $
  map (\(txt, f, v, expected) ->
        let vp = listsToParameters v
        in testCase txt $ fwdFun vp f vp @?~ expected)
    [ ("fquad", fquad, ([2 :: Double, 3], []), (26.0, 18.0))
    , ( "atanOldReadme", atanOldReadme, ([1.1, 2.2, 3.3], [])
      , (7.662345305800865, 4.9375516951604155) )
    , ( "vatanOldReadme", vatanOldReadme, ([], [1.1, 2.2, 3.3])
      , (7.662345305800865, 4.9375516951604155) )
    ]

-- The formula for comparing derivative and gradient is due to @awf
-- at https://github.com/Mikolaj/horde-ad/issues/15#issuecomment-1063251319
quickCheckForwardAndBackward :: TestTree
quickCheckForwardAndBackward =
  testGroup "Simple QuickCheck of gradient vs derivative vs perturbation"
    [ quickCheckTest0 "fquad" fquad (\(x, y, _z) -> ([x, y], [], [], []))
    , quickCheckTest0 "atanOldReadme" atanOldReadme
             (\(x, y, z) -> ([x, y, z], [], [], []))
    , quickCheckTest0 "vatanOldReadme" vatanOldReadme
             (\(x, y, z) -> ([], [x, y, z], [], []))
    , quickCheckTest0 "sinKonst" sinKonst  -- powKonst NaNs immediately
             (\(x, _, z) -> ([], [x, z], [], []))
   ]

-- A function that goes from `R^3` to `R^2`, with a representation
-- of the input and the output tuple that is convenient for interfacing
-- with the library.
atanOldReadmeOriginal :: RealFloat a => a -> a -> a -> Data.Vector.Vector a
atanOldReadmeOriginal x y z =
  let w = x * sin y
  in V.fromList [atan2 z w, z * x]

-- Here we instantiate the function to dual numbers
-- and add a glue code that selects the function inputs from
-- a uniform representation of objective function parameters
-- represented as delta-inputs (`ADInputs`).
atanOldReadmeInputs
  :: ADModeAndNum d r
  => ADInputs d r -> Data.Vector.Vector (ADVal d r)
atanOldReadmeInputs inputs =
  case map (at0 inputs) [0 ..] of
    x : y : z : _ -> atanOldReadmeOriginal x y z
    _ -> error "atanOldReadmeInputs"

-- According to the paper, to handle functions with non-scalar results,
-- we dot-product them with dt which, for simplicity, we here set
-- to a record containing only ones. We could also apply the dot-product
-- automatically in the library code (though perhaps we should
-- emit a warning too, in case the user just forgot to apply
-- a loss function and that's the only reason the result is not a scalar?).
-- For now, let's perform the dot product in user code.

-- Here is the function for dot product with ones, which is just the sum
-- of elements of a vector.
sumElementsOfADVals
  :: ADModeAndNum d r
  => Data.Vector.Vector (ADVal d r) -> ADVal d r
sumElementsOfADVals = V.foldl' (+) 0

-- Here we apply the function.
atanOldReadme
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
atanOldReadme = sumElementsOfADVals . atanOldReadmeInputs

-- The underscores and empty vectors are placeholders for the vector,
-- matrix and arbitrary tensor components of the parameters tuple,
-- which we here don't use (above we construct a vector output,
-- but it's a vector of scalar parameters, not a single parameter
-- of rank 1).
atanOldReadmeDReverse :: HasDelta r
                   => Domain0 r -> IO (Domain0 r, r)
atanOldReadmeDReverse ds = do
  ((!result, _), !v) <-
    first domainsTo01 <$> revIO 1 atanOldReadme (domainsFrom01 ds V.empty)
  return (result, v)

oldReadmeTests :: TestTree
oldReadmeTests = testGroup "Simple tests for README"
  [ testCase " Float (1.1, 2.2, 3.3)" $ do
      res <- atanOldReadmeDReverse (V.fromList [1.1 :: Float, 2.2, 3.3])
      res @?~ (V.fromList [3.0715904, 0.18288425, 1.1761366], 4.937552)
  , testCase " Double (1.1, 2.2, 3.3)" $ do
      res <- atanOldReadmeDReverse (V.fromList [1.1 :: Double, 2.2, 3.3])
      res @?~ ( V.fromList [ 3.071590389300859
                           , 0.18288422990948425
                           , 1.1761365368997136 ]
              , 4.9375516951604155 )
  ]

-- And here's a version of the example that uses vector parameters
-- (quite wasteful in this case) and transforms intermediate results
-- via a primitive differentiable type of vectors instead of inside
-- vectors of primitive differentiable scalars.

vatanOldReadme
  :: ADModeAndNum d r
  => ADInputs d r -> ADVal d r
vatanOldReadme inputs =
  let xyzVector = at1 inputs 0
      f = index0 xyzVector
      (x, y, z) = (f 0, f 1, f 2)
      v = seq1 $ atanOldReadmeOriginal x y z
  in sumElements0 v

vatanOldReadmeDReverse :: HasDelta r
                    => Domain1 r -> IO (Domain1 r, r)
vatanOldReadmeDReverse dsV = do
  ((_, !result), !v) <-
    first domainsTo01 <$> revIO 1 vatanOldReadme (domainsFrom01 V.empty dsV)
  return (result, v)

oldReadmeTestsV :: TestTree
oldReadmeTestsV = testGroup "Simple tests of vector-based code for README"
  [ testCase "V Float (1.1, 2.2, 3.3)" $ do
      res <- vatanOldReadmeDReverse (V.singleton $ V.fromList [1.1 :: Float, 2.2, 3.3])
      res @?~ ( V.singleton $ V.fromList [3.0715904, 0.18288425, 1.1761366]
              , 4.937552 )
  , testCase "V Double (1.1, 2.2, 3.3)" $ do
      res <- vatanOldReadmeDReverse (V.singleton $ V.fromList [1.1 :: Double, 2.2, 3.3])
      res @?~ ( V.singleton $ V.fromList [ 3.071590389300859
                                         , 0.18288422990948425
                                         , 1.1761365368997136 ]
              , 4.9375516951604155 )
  ]

finalCounter :: TestTree
finalCounter = testCase "Final counter value" $ do
  counter <- unsafeGetFreshId
  hPutStrLn stderr $ printf "\nFinal counter value: %d" counter
  assertBool "counter dangerously high" $ counter < 2 ^ (62 :: Int)
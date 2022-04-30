{-# LANGUAGE RankNTypes, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TestSingleGradient (testTrees) where

import Prelude

import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import qualified Numeric.LinearAlgebra as HM
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Test.Tasty.QuickCheck

import HordeAd hiding (sumElementsVectorOfDual)

testTrees :: [TestTree]
testTrees = [ dfTests
            , vectorTests
            , dfTestsForward
            , dfTestsFastForward
            , quickCheckForwardAndBackward
            , readmeTests
            , readmeTestsV
            ]

-- Unfortunately, monadic versions of the operations below are not
-- polymorphic over whether they operate on scalars, vectors or other types,
-- so we should probably abandon them.

(+\) :: DualMonad r m => DualNumber r -> DualNumber r -> m (DualNumber r)
(+\) u v = returnLet $ u + v

(*\) :: DualMonad r m => DualNumber r -> DualNumber r -> m (DualNumber r)
(*\) u v = returnLet $ u * v

(**\) :: DualMonad r m
      => DualNumber r -> DualNumber r -> m (DualNumber r)
(**\) u v = returnLet $ u ** v

squareDual :: DualMonad r m => DualNumber r -> m (DualNumber r)
squareDual = returnLet . square

dfShow :: HasDelta r
       => (DualNumberVariables r -> DualMonadGradient r (DualNumber r))
       -> [Primal r]
       -> ([Primal r], Primal r)
dfShow f deltaInput =
  let ((results, _, _, _), value) =
        dReverse f (V.fromList deltaInput, V.empty, V.empty, V.empty)
  in (V.toList results, value)

fX :: DualMonad (Delta0 Float) m
   => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
fX variables = do
  let x = var0 variables 0
  return x

fX1Y :: DualMonad (Delta0 Float) m
     => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
fX1Y variables = do
  let x = var0 variables 0
      y = var0 variables 1
  x1 <- x +\ 1
  x1 *\ y

fXXY :: DualMonad (Delta0 Float) m
     => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
fXXY variables = do
  let x = var0 variables 0
      y = var0 variables 1
  xy <- x *\ y
  x *\ xy

fXYplusZ :: DualMonad (Delta0 Float) m
         => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
fXYplusZ variables = do
  let x = var0 variables 0
      y = var0 variables 1
      z = var0 variables 2
  xy <- x *\ y
  xy +\ z

fXtoY :: DualMonad (Delta0 Float) m
      => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
fXtoY variables = do
  let x = var0 variables 0
      y = var0 variables 1
  x **\ y

freluX :: DualMonad (Delta0 Float) m
       => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
freluX variables = do
  let x = var0 variables 0
  reluAct x

fquad :: DualMonad r m => DualNumberVariables r -> m (DualNumber r)
fquad variables = do
  let x = var0 variables 0
      y = var0 variables 1
  x2 <- squareDual x
  y2 <- y *\ y
  tmp <- x2 +\ y2
  tmp +\ 5

dfTests :: TestTree
dfTests = testGroup "Simple dReverse application tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ dfShow f v @?= expected)
    [ ("fX", fX, [99], ([1.0],99.0))
    , ("fX1Y", fX1Y, [3, 2], ([2.0,4.0],8.0))
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
    , ("scalarSum", vec_omit_scalarSum_aux, [1, 1, 3], ([1.0,1.0,1.0],5.0))
    ]

vec_omit_scalarSum_aux
  :: DualMonad r m
  => DualNumberVariables r -> m (DualNumber r)
vec_omit_scalarSum_aux vec = returnLet $ foldlDual' (+) 0 vec

sumElementsV
  :: DualMonad r m
  => DualNumberVariables r -> m (DualNumber r)
sumElementsV variables = do
  let x = var1 variables 0
  returnLet $ sumElements0 x

altSumElementsV
  :: DualMonad r m
  => DualNumberVariables r -> m (DualNumber r)
altSumElementsV variables = do
  let x = var1 variables 0
  returnLet $ altSumElements0 x

dfVectorShow
  :: (HasDelta r, Primal r ~ Float)
  => (DualNumberVariables r -> DualMonadGradient r (DualNumber r))
  -> [[Float]]
  -> ([[Float]], Float)
dfVectorShow f deltaInput =
  let ((_, results, _, _), value) =
        dReverse f
          (V.empty, V.fromList (map V.fromList deltaInput), V.empty, V.empty)
  in (map V.toList $ V.toList results, value)

vectorTests :: TestTree
vectorTests = testGroup "Simple dReverse application to vectors tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ dfVectorShow f v @?= expected)
    [ ("sumElementsV", sumElementsV, [[1, 1, 3]], ([[1.0,1.0,1.0]],5.0))
    , ("altSumElementsV", altSumElementsV, [[1, 1, 3]], ([[1.0,1.0,1.0]],5.0))
    ]

dForwardShow
  :: HasDelta r
  => (DualNumberVariables r -> DualMonadGradient r (DualNumber r))
  -> ([Primal r], [Primal r])
  -> ([Primal r], [Primal r])
  -> (Primal r, Primal r)
dForwardShow f (deltaInput, deltaInputV) (ds0, ds1) =
  dForward f ( V.fromList deltaInput, V.singleton $ V.fromList deltaInputV
             , V.empty, V.empty )
             ( V.fromList ds0, V.singleton $ V.fromList ds1
             , V.empty, V.empty )

dfTestsForward :: TestTree
dfTestsForward =
 testGroup "Simple dReverse (Forward Double) application tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ dForwardShow f v v @?= expected)
    [ ("fquad", fquad, ([2 :: Double, 3], []), (26.0, 18.0))
    , ( "atanReadmeM", atanReadmeM, ([1.1, 2.2, 3.3], [])
      , (7.662345305800865, 4.9375516951604155) )
    , ( "vatanReadmeM", vatanReadmeM, ([], [1.1, 2.2, 3.3])
      , (7.662345305800865, 4.9375516951604155) )
    ]

dFastForwardShow
  :: HasForward r
  => (DualNumberVariables r
      -> DualMonadForward r (DualNumber r))
  -> ([Primal r], [Primal r])
  -> ([Primal r], [Primal r])
  -> (Primal r, Primal r)
dFastForwardShow f (deltaInput, deltaInputV) (ds0, ds1) =
  dFastForward f ( V.fromList deltaInput, V.singleton $ V.fromList deltaInputV
                 , V.empty, V.empty )
                 ( V.fromList ds0, V.singleton $ V.fromList ds1
                 , V.empty, V.empty )

dfTestsFastForward :: TestTree
dfTestsFastForward =
 testGroup "Simple dReverse (FastForward Double) application tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ dFastForwardShow f v v @?= expected)
    [ ("fquad", fquad, ([2 :: Double, 3], []), (26.0, 18.0))
    , ( "atanReadmeM", atanReadmeM, ([1.1, 2.2, 3.3], [])
      , (7.662345305800865, 4.9375516951604155) )
    , ( "vatanReadmeM", vatanReadmeM, ([], [1.1, 2.2, 3.3])
      , (7.662345305800865, 4.9375516951604155) )
    ]

dfDotShow
  :: HasDelta r
  => (DualNumberVariables r -> DualMonadGradient r (DualNumber r))
  -> ([Primal r], [Primal r])
  -> ([Primal r], [Primal r])
  -> (Primal r, Primal r)
dfDotShow f (deltaInput, deltaInputV) (ds0, ds1) =
  let ((res0, res1, _, _), value) =
        dReverse f ( V.fromList deltaInput, V.singleton $ V.fromList deltaInputV
             , V.empty, V.empty )
  in ( res0 HM.<.> V.fromList ds0
       + V.head res1 HM.<.> V.fromList ds1  -- we assume res0 or res1 is empty
     , value )

-- The formula for comparing derivative and gradient is due to @awf
-- at https://github.com/Mikolaj/horde-ad/issues/15#issuecomment-1063251319
quickCheckForwardAndBackward :: TestTree
quickCheckForwardAndBackward =
  testGroup "Simple case of verifying two forward derivative methods and one backprop gradient method give compatible results" $
    let qcTest :: TestName
               -> (forall r m. DualMonad r m
                   => DualNumberVariables r -> m (DualNumber r))
               -> ((Double, Double, Double) -> ([Double], [Double]))
               -> TestTree
        qcTest txt f fArg =
          testProperty txt
          $ forAll (choose ((-2, -2, -2), (2, 2, 2))) $ \xyz xyz2 ->
              let args = fArg xyz
                  ds = fArg xyz2
                  ff = dFastForwardShow f args ds
                  close a b = abs (a - b) <= 0.000001
                  close1 (a1, b1) (a2, b2) = close a1 a2 .&&. b1 === b2
              in dForwardShow f args ds === ff
                 .&&. close1 (dfDotShow f args ds) ff
    in [ qcTest "fquad" fquad (\(x, y, _z) -> ([x, y], []))
       , qcTest "atanReadmeM" atanReadmeM
                (\(x, y, z) -> ([x, y, z], []))
       , qcTest "vatanReadmeM" vatanReadmeM
                (\(x, y, z) -> ([], [x, y, z]))
       ]

-- A function that goes from `R^3` to `R^2`, with a representation
-- of the input and the output tuple that is convenient for interfacing
-- with the library.
atanReadmeOriginal :: RealFloat a => a -> a -> a -> Data.Vector.Vector a
atanReadmeOriginal x y z =
  let w = x * sin y
  in V.fromList [atan2 z w, z * x]

-- Here we instantiate the function to dual numbers
-- and add a glue code that selects the function inputs from
-- a uniform representation of objective function parameters
-- represented as delta-variables (`DualNumberVariables`).
atanReadmeVariables
  :: IsScalar r
  => DualNumberVariables r -> Data.Vector.Vector (DualNumber r)
atanReadmeVariables variables =
  let x : y : z : _ = vars variables
  in atanReadmeOriginal x y z

-- According to the paper, to handle functions with non-scalar results,
-- we dot-product them with dt which, for simplicity, we here set
-- to a record containing only ones. We could also apply the dot-product
-- automatically in the library code (though perhaps we should
-- emit a warning too, in case the user just forgot to apply
-- a loss function and that's the only reason the result is not a scalar?).
-- For now, let's perform the dot product in user code.

-- Here is the function for dot product with ones, which is just the sum
-- of elements of a vector.
sumElementsOfDualNumbers
  :: IsScalar r
  => Data.Vector.Vector (DualNumber r) -> DualNumber r
sumElementsOfDualNumbers = V.foldl' (+) 0

-- Here we apply the function.
atanReadmeScalar
  :: IsScalar r
  => DualNumberVariables r -> DualNumber r
atanReadmeScalar = sumElementsOfDualNumbers . atanReadmeVariables

-- Here we introduce a single delta-let binding (`returnLet`) to ensure
-- that if this code is used in a larger context and repeated,
-- no explosion of delta-expressions can happen.
-- If the code above had any repeated non-variable expressions
-- (e.g., if @w@ appeared twice) the user would need to make it monadic
-- and apply @returnLet@ already there.
atanReadmeM
  :: DualMonad r m
  => DualNumberVariables r -> m (DualNumber r)
atanReadmeM = returnLet . atanReadmeScalar

-- The underscores and empty vectors are placeholders for the vector,
-- matrix and arbitrary tensor components of the parameters tuple,
-- which we here don't use (above we construct a vector output,
-- but it's a vector of scalar parameters, not a single parameter
-- of rank 1).
atanReadmeDReverse :: HasDelta r
                   => Domain0 r -> (Domain0 r, Primal r)
atanReadmeDReverse ds =
  let ((result, _, _, _), value) =
        dReverse atanReadmeM (ds, V.empty, V.empty, V.empty)
  in (result, value)

readmeTests :: TestTree
readmeTests = testGroup "Simple tests for README"
  [ testCase " Float (1.1, 2.2, 3.3)"
    $ atanReadmeDReverse (V.fromList [1.1 :: Float, 2.2, 3.3])
      @?= (V.fromList [3.0715904, 0.18288425, 1.1761366], 4.937552)
  , testCase " Double (1.1, 2.2, 3.3)"
    $ atanReadmeDReverse (V.fromList [1.1 :: Double, 2.2, 3.3])
      @?= ( V.fromList [ 3.071590389300859
                       , 0.18288422990948425
                       , 1.1761365368997136 ]
          , 4.9375516951604155 )
  ]

-- And here's a version of the example that uses vector parameters
-- (quite wasteful in this case) and transforms intermediate results
-- via a primitive differentiable type of vectors instead of inside
-- vectors of primitive differentiable scalars.

vatanReadmeM
  :: DualMonad r m
  => DualNumberVariables r -> m (DualNumber r)
vatanReadmeM variables = do
  let xyzVector = var1 variables 0
      [x, y, z] = map (index0 xyzVector) [0, 1, 2]
      v = seq1 $ atanReadmeOriginal x y z
  returnLet $ sumElements0 v

vatanReadmeDReverse :: HasDelta r
                    => Domain1 r -> (Domain1 r, Primal r)
vatanReadmeDReverse dsV =
  let ((_, result, _, _), value) =
        dReverse vatanReadmeM (V.empty, dsV, V.empty, V.empty)
  in (result, value)

readmeTestsV :: TestTree
readmeTestsV = testGroup "Simple tests of vector-based code for README"
  [ testCase "V Float (1.1, 2.2, 3.3)"
    $ vatanReadmeDReverse (V.singleton $ V.fromList [1.1 :: Float, 2.2, 3.3])
      @?= ( V.singleton $ V.fromList [3.0715904, 0.18288425, 1.1761366]
          , 4.937552 )
  , testCase "V Double (1.1, 2.2, 3.3)"
    $ vatanReadmeDReverse (V.singleton $ V.fromList [1.1 :: Double, 2.2, 3.3])
      @?= ( V.singleton $ V.fromList [ 3.071590389300859
                                     , 0.18288422990948425
                                     , 1.1761365368997136 ]
          , 4.9375516951604155 )
  ]

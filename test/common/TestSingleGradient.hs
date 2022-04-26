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
  :: DualMonad (Delta0 Float) m
  => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
vec_omit_scalarSum_aux vec = returnLet $ foldlDual' (+) 0 vec

sumElementsV
  :: DualMonad (Delta0 Float) m
  => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
sumElementsV variables = do
  let x = var1 variables 0
  returnLet $ sumElements0 x

altSumElementsV
  :: DualMonad (Delta0 Float) m
  => DualNumberVariables (Delta0 Float) -> m (DualNumber (Delta0 Float))
altSumElementsV variables = do
  let x = var1 variables 0
  returnLet $ altSumElements0 x

dfVectorShow
  :: (DualNumberVariables (Delta0 Float)
      -> DualMonadGradient (Delta0 Float) (DualNumber (Delta0 Float)))
  -> [[Float]]
  -> ([[Float]], Float)
dfVectorShow f deltaInput =
  let ((_, results, _, _), value) =
        dReverse f (V.empty, V.fromList (map V.fromList deltaInput), V.empty, V.empty)
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
dfTestsForward = testGroup "Simple dReverse (Forward Double) application tests" $
  map (\(txt, f, v, expected) ->
        testCase txt $ dForwardShow f v v @?= expected)
    [ ("fquad", fquad, ([2 :: Double, 3], []), (26.0, 18.0))
    , ( "atanReadmeMPoly", atanReadmeMPoly, ([1.1, 2.2, 3.3], [])
      , (7.662345305800865, 4.9375516951604155) )
    , ( "atanReadmeMPolyV", atanReadmeMPolyV, ([], [1.1, 2.2, 3.3])
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
    , ( "atanReadmeMPoly", atanReadmeMPoly, ([1.1, 2.2, 3.3], [])
      , (7.662345305800865, 4.9375516951604155) )
    , ( "atanReadmeMPolyV", atanReadmeMPolyV, ([], [1.1, 2.2, 3.3])
      , (7.662345305800865, 4.9375516951604155) )
    ]

dfDotShow
  :: r ~ Delta0 Double
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
  testGroup "Verify two forward derivative methods and one backprop gradient method give compatible results" $
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
       , qcTest "atanReadmeMPoly" atanReadmeMPoly
                (\(x, y, z) -> ([x, y, z], []))
       , qcTest "atanReadmeMPolyV" atanReadmeMPolyV
                (\(x, y, z) -> ([], [x, y, z]))
       ]

-- The input vector is meant to have 3 elements, the output vector
-- two elements. In the future we may use something like
-- https://hackage.haskell.org/package/vector-sized-1.5.0
-- to express the sizes in types, or we may be able to handle tuples
-- automatically. For now, the user has to translate from tuples
-- to vectors manually and we omit this straightforward boilerplate code here.
-- TODO: while we use weakly-typed vectors, work on user-friendly errors
-- if the input record is too short.
atanReadmePoly :: IsScalar r
               => DualNumberVariables r -> Data.Vector.Vector (DualNumber r)
atanReadmePoly variables =
  let x : y : z : _ = vars variables
      w = x * sin y
  in V.fromList [atan2 z w, z * x]

-- According to the paper, to handle functions with non-scalar results,
-- we dot-product them with dt which, for simplicity, we here set
-- to a record containing only ones. We could also apply the dot-product
-- automatically in the library code (though perhaps we should
-- emit a warning too, in case the user just forgot to apply
-- a loss function and that's the only reason the result is not a scalar?).
-- For now, let's perform the dot product in user code.
-- Here is the code for dot product with ones, which is just the sum
-- of elements of a vector:
sumElementsVectorOfDual :: IsScalar r
                        => Data.Vector.Vector (DualNumber r)
                        -> DualNumber r
sumElementsVectorOfDual = V.foldl' (+) 0

-- Here we introduce the only Delta-let binding (@returnLet@) to ensure
-- that if this code is used in a larger context and repeated,
-- no explosion of delta-expression can happen.
-- If the code above had any repeated non-variable expressions
-- (e.g., if @w@ appeared twice) the user would need to make it monadic
-- and apply @returnLet@ already there.
atanReadmeMPoly :: DualMonad r m
                => DualNumberVariables r -> m (DualNumber r)
atanReadmeMPoly variables =
  returnLet $ sumElementsVectorOfDual $ atanReadmePoly variables

-- The underscores and empty vectors are placeholders for the vector
-- and matrix components of the parameters triple, which we here don't use
-- (we construct vectors, but from scalar parameters).
dfAtanReadmeMPoly :: HasDelta r
                  => Domain0 r -> (Domain0 r, Primal r)
dfAtanReadmeMPoly ds =
  let ((result, _, _, _), value) =
        dReverse atanReadmeMPoly (ds, V.empty, V.empty, V.empty)
  in (result, value)

readmeTests :: TestTree
readmeTests = testGroup "Tests of code from the library's README"
  [ testCase "Poly Float (1.1, 2.2, 3.3)"
    $ dfAtanReadmeMPoly (V.fromList [1.1 :: Float, 2.2, 3.3])
      @?= (V.fromList [3.0715904, 0.18288425, 1.1761366], 4.937552)
  , testCase "Poly Double (1.1, 2.2, 3.3)"
    $ dfAtanReadmeMPoly (V.fromList [1.1 :: Double, 2.2, 3.3])
      @?= ( V.fromList [ 3.071590389300859
                       , 0.18288422990948425
                       , 1.1761365368997136 ]
          , 4.9375516951604155 )
  ]

-- And here's a version of the example that uses vector parameters
-- (quite wasteful in this case) and transforms intermediate results
-- via a primitive differentiable type of vectors instead of inside
-- vectors of primitive differentiable scalars.

atanReadmePolyV :: IsScalar r
                => DualNumberVariables r -> DualNumber (Tensor1 r)
atanReadmePolyV variables =
  let xyzVector = var1 variables 0
      x = index0 xyzVector 0
      y = index0 xyzVector 1
      z = index0 xyzVector 2
      w = x * sin y
  in seq1 $ V.fromList [atan2 z w, z * x]

atanReadmeMPolyV :: DualMonad r m
                 => DualNumberVariables r -> m (DualNumber r)
atanReadmeMPolyV variables =
  returnLet $ atanReadmePolyV variables <.>!! HM.konst 1 2

-- The underscores and empty vectors are placeholders for the vector
-- and matrix components of the parameters triple, which we here don't use
-- (we construct vectors, but from scalar parameters).
dfAtanReadmeMPolyV :: HasDelta r
                   => Domain1 r -> (Domain1 r, Primal r)
dfAtanReadmeMPolyV dsV =
  let ((_, result, _, _), value) =
        dReverse atanReadmeMPolyV (V.empty, dsV, V.empty, V.empty)
  in (result, value)

readmeTestsV :: TestTree
readmeTestsV = testGroup "Tests of vector-based code from the library's README"
  [ testCase "PolyV Float (1.1, 2.2, 3.3)"
    $ dfAtanReadmeMPolyV (V.singleton $ V.fromList [1.1 :: Float, 2.2, 3.3])
      @?= ( V.singleton $ V.fromList [3.0715904, 0.18288425, 1.1761366]
          , 4.937552 )
  , testCase "PolyV Double (1.1, 2.2, 3.3)"
    $ dfAtanReadmeMPolyV (V.singleton $ V.fromList [1.1 :: Double, 2.2, 3.3])
      @?= ( V.singleton $ V.fromList [ 3.071590389300859
                                     , 0.18288422990948425
                                     , 1.1761365368997136 ]
          , 4.9375516951604155 )
  ]
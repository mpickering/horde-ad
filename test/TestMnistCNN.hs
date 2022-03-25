{-# LANGUAGE TypeFamilies #-}
module TestMnistCNN (testTrees, shortTestForCITrees) where

import Prelude

import           Control.Monad (foldM)
import           Data.List (foldl', unfoldr)
import qualified Data.Vector.Generic as V
import           Numeric.LinearAlgebra (Matrix, Vector)
import qualified Numeric.LinearAlgebra as HM
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Text.Printf

import HordeAd
import HordeAd.Core.OutdatedOptimizer
import HordeAd.Tool.MnistTools

testTrees :: [TestTree]
testTrees = [ sinRNNTests
            , mnistRNNTestsShort
            , mnistRNNTestsLong
            ]

shortTestForCITrees :: [TestTree]
shortTestForCITrees = [ sinRNNTests
                      , mnistRNNTestsShort
                      ]

hiddenLayerMnistRNNB :: (DualMonad r m, Floating (Primal (Tensor2 r)))
                     => Primal (Tensor2 r)  -- the mini-batch of data 28x150
                     -> DualNumber (Tensor2 r)  -- state for mini-batch 128x150
                     -> DualNumberVariables r
                     -> m (DualNumber (Tensor2 r), DualNumber (Tensor2 r))
hiddenLayerMnistRNNB x s variables = do
  let wX = var2 variables 0  -- 128x28
      wS = var2 variables 1  -- 128x128
      b = var1 variables 0  -- 128
      batchSize = HM.cols x
      y = wX <>!! x + wS <>! s + asColumn2 b batchSize
  yTanh <- returnLet $ tanh y
  return (yTanh, yTanh)

middleLayerMnistRNNB :: (DualMonad r m, Floating (Primal (Tensor2 r)))
                     => DualNumber (Tensor2 r)  -- 128x150
                     -> DualNumber (Tensor2 r)  -- 128x150
                     -> DualNumberVariables r
                     -> m (DualNumber (Tensor2 r), DualNumber (Tensor2 r))
middleLayerMnistRNNB batchOfVec@(D u _) s variables = do
  let wX = var2 variables 3  -- 128x128
      wS = var2 variables 4  -- 128x128
      b = var1 variables 2  -- 128
      batchSize = HM.cols u
      y = wX <>! batchOfVec + wS <>! s + asColumn2 b batchSize
  yTanh <- returnLet $ tanh y
  return (yTanh, yTanh)

outputLayerMnistRNNB :: DualMonad r m
                     => DualNumber (Tensor2 r)  -- 128x150
                     -> DualNumberVariables r
                     -> m (DualNumber (Tensor2 r))
outputLayerMnistRNNB batchOfVec@(D u _) variables = do
  let w = var2 variables 2  -- 10x128
      b = var1 variables 1  -- 10
      batchSize = HM.cols u
  returnLet $ w <>! batchOfVec + asColumn2 b batchSize

fcfcrnnMnistB :: (DualMonad r m, Floating (Primal (Tensor2 r)))
              => Primal (Tensor2 r)
              -> DualNumber (Tensor2 r)
              -> DualNumberVariables r
              -> m (DualNumber (Tensor2 r), DualNumber (Tensor2 r))
fcfcrnnMnistB = hiddenLayerMnistRNNB

fcfcrnnMnistB2 :: (DualMonad r m, Floating (Primal (Tensor2 r)))
               => Primal (Tensor2 r)  -- 28x150
               -> DualNumber (Tensor2 r)  -- 256x150
               -> DualNumberVariables r
               -> m (DualNumber (Tensor2 r), DualNumber (Tensor2 r))
fcfcrnnMnistB2 x s@(D u _) variables = do
  let len = HM.rows u `div` 2
      s1 = rowSlice2 0 len s
      s2 = rowSlice2 len len s
  (vec1, s1') <- hiddenLayerMnistRNNB x s1 variables
  (vec2, s2') <- middleLayerMnistRNNB vec1 s2 variables
  return (vec2, rowAppend2 s1' s2')

nnMnistRNNB :: (DualMonad r m, Floating (Primal (Tensor2 r)))
            => Int
            -> [Primal (Tensor2 r)]
            -> DualNumberVariables r
            -> m (DualNumber (Tensor2 r))
nnMnistRNNB width xs variables = do
  let batchSize = HM.cols $ head xs
  rnnLayer <- zeroStateB (width, batchSize) (unrollLastG fcfcrnnMnistB)
                         xs variables
  outputLayerMnistRNNB rnnLayer variables

nnMnistRNNB2 :: (DualMonad r m, Floating (Primal (Tensor2 r)))
             => Int
             -> [Primal (Tensor2 r)]
             -> DualNumberVariables r
             -> m (DualNumber (Tensor2 r))
nnMnistRNNB2 width xs variables = do
  let batchSize = HM.cols $ head xs
  rnnLayer <- zeroStateB (2 * width, batchSize) (unrollLastG fcfcrnnMnistB2)
                         xs variables
  outputLayerMnistRNNB rnnLayer variables

nnMnistRNNLossB :: (DualMonad r m, Floating (Primal (Tensor2 r)))
                => Int
                -> ([Primal (Tensor2 r)], Primal (Tensor2 r))
                -> DualNumberVariables r
                -> m (DualNumber r)
nnMnistRNNLossB width (xs, target) variables = do
  result <- nnMnistRNNB width xs variables
  vec@(D u _) <- lossSoftMaxCrossEntropyL target result
    -- this @asRow@ is safe, because it gets multiplied/subtracted right away
  returnLet $ scale (recip $ fromIntegral $ V.length u) $ sumElements0 vec

nnMnistRNNLossB2 :: (DualMonad r m, Floating (Primal (Tensor2 r)))
                 => Int
                 -> ([Primal (Tensor2 r)], Primal (Tensor2 r))
                 -> DualNumberVariables r
                 -> m (DualNumber r)
nnMnistRNNLossB2 width (xs, target) variables = do
  result <- nnMnistRNNB2 width xs variables
  vec@(D u _) <- lossSoftMaxCrossEntropyL target result
  returnLet $ scale (recip $ fromIntegral $ V.length u) $ sumElements0 vec

mnistTestCaseRNNB
  :: String
  -> Int
  -> Int
  -> (Int
      -> ([Matrix Double], Matrix Double)
      -> DualNumberVariables (Delta0 Double)
      -> DualMonadGradient (Delta0 Double) (DualNumber (Delta0 Double)))
  -> (Int -> [([Vector Double], Vector Double)] -> Domains (Delta0 Double) -> Double)
  -> (Int -> Int -> (Int, [Int], [(Int, Int)]))
  -> Int
  -> Int
  -> Double
  -> TestTree
mnistTestCaseRNNB prefix epochs maxBatches f ftest flen width nLayers
                  expected =
  let ((nParams0, nParams1, nParams2), totalParams, range, parameters0) =
        initializerFixed 44 0.2 (flen width nLayers)
      name = prefix ++ " "
             ++ unwords [ show epochs, show maxBatches
                        , show width, show nLayers
                        , show nParams0, show nParams1, show nParams2
                        , show totalParams, show range ]
  in testCase name $ do
       let rws (input, target) =
             ( map (\k -> V.slice (k * 28) 28 input) [0 .. 27]
             , target )
       trainData <- map rws <$> loadMnistData trainGlyphsPath trainLabelsPath
       testData <- map rws <$> loadMnistData testGlyphsPath testLabelsPath
       let packChunk :: [([Vector Double], Vector Double)]
                     -> ([Matrix Double], Matrix Double)
           packChunk chunk =
             let (inputs, outputs) = unzip chunk
                 behead !acc ([] : _) = reverse acc
                 behead !acc l = behead (HM.fromColumns (map head l) : acc)
                                        (map tail l)
             in (behead [] inputs, HM.fromColumns outputs)
           -- There is some visual feedback, because some of these take long.
           runBatch :: (Domains (Delta0 Double), StateAdam (Delta0 Double))
                    -> (Int, [([Vector Double], Vector Double)])
                    -> IO (Domains (Delta0 Double), StateAdam (Delta0 Double))
           runBatch (parameters@(!_, !_, !_, !_), stateAdam) (k, chunk) = do
             printf "(Batch %d with %d points)\n" k (length chunk)
             let res@(parameters2, _) =
                   sgdAdam (f width) (map packChunk $ chunksOf 150 chunk)
                           parameters stateAdam
                 trainScore = ftest width chunk parameters2
                 testScore = ftest width testData parameters2
             printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
             printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)
             return res
           runEpoch :: Int
                    -> (Domains (Delta0 Double), StateAdam (Delta0 Double))
                    -> IO (Domains (Delta0 Double))
           runEpoch n (params2, _) | n > epochs = return params2
           runEpoch n paramsStateAdam = do
             printf "[Epoch %d]\n" n
             let trainDataShuffled = shuffle (mkStdGen $ n + 5) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf 5000 trainDataShuffled
             !res <- foldM runBatch paramsStateAdam chunks
             runEpoch (succ n) res
       printf "\nEpochs to run/max batches per epoch: %d/%d\n"
              epochs maxBatches
       res <- runEpoch 1 (parameters0, initialStateAdam parameters0)
       let testErrorFinal = 1 - ftest width testData res
       testErrorFinal @?= expected

mnistRNNTestsLong :: TestTree
mnistRNNTestsLong = testGroup "MNIST RNN long tests"
  [ mnistTestCaseRNN "99LL 1 epoch, all batches" 1 99
                     nnMnistRNNLossL (testMnistRNNL @(Delta0 Double)) lenMnistRNNL 128 1
                     8.209999999999995e-2
  , mnistTestCaseRNNB "99BB 1 epoch, all batches" 1 99
                      nnMnistRNNLossB (testMnistRNNL @(Delta0 Double)) lenMnistRNNL 128 1
                      8.209999999999995e-2
  , mnistTestCaseRNN "99LL2 1 epoch, all batches" 1 99
                     nnMnistRNNLossL2 (testMnistRNNL2 @(Delta0 Double)) lenMnistRNNL 128 2
                     6.259999999999999e-2
  , mnistTestCaseRNNB "99BB2 1 epoch, all batches" 1 99
                      nnMnistRNNLossB2 (testMnistRNNL2 @(Delta0 Double)) lenMnistRNNL 128 2
                      6.259999999999999e-2
  , mnistTestCaseRNN "99VV 1 epoch, all batches" 1 99
                     nnMnistRNNLossV (testMnistRNNV @(Delta0 Double)) lenMnistRNNV 128 1
                     6.740000000000002e-2
  ]

mnistRNNTestsShort :: TestTree
mnistRNNTestsShort = testGroup "MNIST RNN short tests"
  [ let glyph = V.unfoldrExactN sizeMnistGlyph (uniformR (0, 1))
        label = V.unfoldrExactN sizeMnistLabel (uniformR (0, 1))
        rws v = map (\k -> V.slice (k * 28) 28 v) [0 .. 27]
        trainData = map ((\g -> (rws (glyph g), label g)) . mkStdGen) [1 .. 140]
    in sgdTestCaseAlt "randomLL 140"
                      (nnMnistRNNLossL 128)
                      (lenMnistRNNL 128 1)
                      (return trainData)
                      [39.26529628894595, 39.26534445638497]
  , let rws (input, target) =
          (map (\k -> V.slice (k * 28) 28 input) [0 .. 27], target)
    in sgdTestCase "firstLL 100 trainset samples only"
                   (nnMnistRNNLossL 128)
                   (lenMnistRNNL 128 1)
                   (map rws . take 100
                    <$> loadMnistData trainGlyphsPath trainLabelsPath)
                   2.779085689596527
  , mnistTestCaseRNN "1LL 1 epoch, 1 batch" 1 1
                     nnMnistRNNLossL (testMnistRNNL @(Delta0 Double)) lenMnistRNNL 128 1
                     0.2845
  , mnistTestCaseRNNB "1BB 1 epoch, 1 batch" 1 1
                      nnMnistRNNLossB (testMnistRNNL @(Delta0 Double)) lenMnistRNNL 128 1
                      0.2845
  , let glyph = V.unfoldrExactN sizeMnistGlyph (uniformR (0, 1))
        label = V.unfoldrExactN sizeMnistLabel (uniformR (0, 1))
        rws v = map (\k -> V.slice (k * 28) 28 v) [0 .. 27]
        trainData = map ((\g -> (rws (glyph g), label g)) . mkStdGen) [1 .. 140]
    in sgdTestCaseAlt "randomLL2 140"
                      (nnMnistRNNLossL2 128)
                      (lenMnistRNNL 128 2)
                      (return trainData)
                      [30.061856005913285, 30.06186534722257]
  , let rws (input, target) =
          (map (\k -> V.slice (k * 28) 28 input) [0 .. 27], target)
    in sgdTestCase "firstLL2 99 trainset samples only"
                   (nnMnistRNNLossL2 128)
                   (lenMnistRNNL 128 2)
                   (map rws . take 99
                    <$> loadMnistData trainGlyphsPath trainLabelsPath)
                   2.772595855528805
  , mnistTestCaseRNN "1LL2 1 epoch, 1 batch" 1 1
                     nnMnistRNNLossL2 (testMnistRNNL2 @(Delta0 Double)) lenMnistRNNL 128 2
                     0.2945
  , mnistTestCaseRNNB "1BB2 1 epoch, 1 batch" 1 1
                      nnMnistRNNLossB2 (testMnistRNNL2 @(Delta0 Double)) lenMnistRNNL 128 2
                      0.2945
  , let glyph = V.unfoldrExactN sizeMnistGlyph (uniformR (0, 1))
        label = V.unfoldrExactN sizeMnistLabel (uniformR (0, 1))
        rws v = map (\k -> V.slice (k * 28) 28 v) [0 .. 27]
        trainData = map ((\g -> (rws (glyph g), label g)) . mkStdGen) [1 .. 100]
    in sgdTestCase "randomVV 100"
                   (nnMnistRNNLossV 128)
                   (lenMnistRNNV 128 1)
                   (return trainData)
                   48.93543453075899
  , let rws (input, target) =
          (map (\k -> V.slice (k * 28) 28 input) [0 .. 27], target)
    in sgdTestCase "firstVV 100 trainset samples only"
                   (nnMnistRNNLossV 128)
                   (lenMnistRNNV 128 1)
                   (map rws . take 100
                    <$> loadMnistData trainGlyphsPath trainLabelsPath)
                   2.7494107689380805
  , mnistTestCaseRNN "1VV 1 epoch, 1 batch" 1 1
                     nnMnistRNNLossV (testMnistRNNV @(Delta0 Double)) lenMnistRNNV 128 1
                     0.3024
  ]

module TestMnistFCNNR
  ( testTrees
  ) where

import Prelude

import           Control.Monad (foldM, unless)
import qualified Data.Array.DynamicS as OD
import qualified Data.Array.RankedS as OR
import qualified Data.EnumMap.Strict as EM
import           Data.List.Index (imap)
import           Data.MonoTraversable (Element)
import qualified Data.Strict.IntMap as IM
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           Numeric.LinearAlgebra (Vector)
import qualified Numeric.LinearAlgebra as LA
import           System.IO (hPutStrLn, stderr)
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Text.Printf

import HordeAd.Core.Ast
import HordeAd.Core.AstInterpret
import HordeAd.Core.AstSimplify
import HordeAd.Core.DualNumber (ADVal, dDnotShared)
import HordeAd.Core.Engine
import HordeAd.Core.SizedIndex
import HordeAd.Core.TensorADVal (ADTensor)
import HordeAd.Core.TensorClass
import HordeAd.External.Adaptor
import HordeAd.External.CommonRankedOps
import HordeAd.External.Optimizer
import HordeAd.External.OptimizerTools

import EqEpsilon

import           MnistData
import qualified MnistFcnnRanked1
import qualified MnistFcnnRanked2

testTrees :: [TestTree]
testTrees = [ tensorADValMnistTests
            , tensorIntermediateMnistTests
            , tensorADOnceMnistTests
            , tensorADValMnistTests2
            , tensorIntermediateMnistTests2
            , tensorADOnceMnistTests2
            , tensorMnistTestsPP
            ]


-- * Using vectors, which is rank 1

-- POPL differentiation, straight via the ADVal instance of Tensor
mnistTestCase2VTA
  :: forall r.
     ( ADReady r, ADReady (ADVal r), ScalarOf r ~ r, ScalarOf (ADVal r) ~ r
     , TensorOf 0 (ADVal r) ~ ADVal (OR.Array 0 r)
     , TensorOf 1 (ADVal r) ~ ADVal (OR.Array 1 r)
     , DTensorOf (ADVal r) ~ ADVal (OD.Array r)
     , PrintfArg r, AssertEqualUpToEpsilon r
     , Floating (Vector r), ADTensor r
     , DynamicTensor r, DomainsTensor r, Element r ~ r
     , DTensorOf r ~ OD.Array r, TensorOf 1 r ~ OR.Array 1 r
     , DomainsOf r ~ Data.Vector.Vector (OD.Array r) )
  => String
  -> Int -> Int -> Int -> Int -> r -> Int -> r
  -> TestTree
mnistTestCase2VTA prefix epochs maxBatches widthHidden widthHidden2
                  gamma batchSize expected =
  let nParams1 = MnistFcnnRanked1.afcnnMnistLen1 widthHidden widthHidden2
      params1Init = V.fromList $
        imap (\i nPV -> OD.fromVector [nPV]
                        $ V.map realToFrac
                        $ LA.randomVector (44 + nPV + i) LA.Uniform nPV
                          - LA.scalar 0.5)
             nParams1
      -- This is a very ugly and probably unavoidable boilerplate:
      -- we have to manually define a dummy value of type ADFcnnMnist1Parameters
      -- with the correct list lengths (vector lengths can be fake)
      -- to bootstrap the adaptor machinery. Such boilerplate can be
      -- avoided only with shapely typed tensors and scalars or when
      -- not using adaptors.
      emptyR = OR.fromList [0] []
      valsInit :: MnistFcnnRanked1.ADFcnnMnist1Parameters r
      valsInit = ( (replicate widthHidden emptyR, emptyR)
                 , (replicate widthHidden2 emptyR, emptyR)
                 , (replicate sizeMnistLabelInt emptyR, emptyR) )
      name = prefix ++ ": "
             ++ unwords [ show epochs, show maxBatches
                        , show widthHidden, show widthHidden2
                        , show (length nParams1), show (sum nParams1)
                        , show gamma ]
      ftest :: [MnistData r] -> Domains r -> r
      ftest mnist testParams =
        MnistFcnnRanked1.afcnnMnistTest1
          widthHidden widthHidden2 mnist
          (\f -> OR.toVector $ f $ parseDomains valsInit testParams)
  in testCase name $ do
       hPutStrLn stderr $
         printf "\n%s: Epochs to run/max batches per epoch: %d/%d"
                prefix epochs maxBatches
       trainData <- loadMnistData trainGlyphsPath trainLabelsPath
       testData <- take (batchSize * maxBatches)
                   <$> loadMnistData testGlyphsPath testLabelsPath
       -- Mimic how backprop tests and display it, even though tests
       -- should not print, in principle.
       let runBatch :: Domains r -> (Int, [MnistData r]) -> IO (Domains r)
           runBatch !domains (k, chunk) = do
             let f mnist adinputs =
                   MnistFcnnRanked1.afcnnMnistLoss1
                     widthHidden widthHidden2
                     mnist (parseADInputs valsInit adinputs)
                 res = fst $ sgd gamma f chunk domains
                 trainScore = ftest chunk res
                 testScore = ftest testData res
                 lenChunk = length chunk
             unless (widthHidden < 10) $ do
               hPutStrLn stderr $ printf "\n%s: (Batch %d with %d points)" prefix k lenChunk
               hPutStrLn stderr $ printf "%s: Training error:   %.2f%%" prefix ((1 - trainScore) * 100)
               hPutStrLn stderr $ printf "%s: Validation error: %.2f%%" prefix ((1 - testScore ) * 100)
             return res
       let runEpoch :: Int -> Domains r -> IO (Domains r)
           runEpoch n params | n > epochs = return params
           runEpoch n params = do
             unless (widthHidden < 10) $
               hPutStrLn stderr $ printf "\n%s: [Epoch %d]" prefix n
             let trainDataShuffled = shuffle (mkStdGen $ n + 5) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf batchSize trainDataShuffled
             !res <- foldM runBatch params chunks
             runEpoch (succ n) res
       res <- runEpoch 1 (mkDomains emptyR params1Init)
       let testErrorFinal = 1 - ftest testData res
       testErrorFinal @?~ expected

tensorADValMnistTests :: TestTree
tensorADValMnistTests = testGroup "ShortRanked ADVal MNIST tests"
  [ mnistTestCase2VTA "VTA 1 epoch, 1 batch" 1 1 300 100 0.02 5000
                      (0.16600000000000004 :: Double)
  , mnistTestCase2VTA "VTA artificial 1 2 3 4 5" 1 2 3 4 5 5000
                      (0.8972 :: Float)
  , mnistTestCase2VTA "VTA artificial 5 4 3 2 1" 5 4 3 2 1 5000
                      (0.6585 :: Double)
  ]

-- POPL differentiation, Ast term defined only once but differentiated each time
mnistTestCase2VTI
  :: forall r.
     ( ADReady r, ADReady (ADVal r), ScalarOf r ~ r, ScalarOf (ADVal r) ~ r
     , TensorOf 1 (ADVal r) ~ ADVal (OR.Array 1 r)
     , DTensorOf (ADVal r) ~ ADVal (OD.Array r)
     , InterpretAst (ADVal r)
     , PrintfArg r, AssertEqualUpToEpsilon r
     , Floating (Vector r), ADTensor r
     , DynamicTensor r, DomainsTensor r, Element r ~ r
     , DTensorOf r ~ OD.Array r, TensorOf 1 r ~ OR.Array 1 r
     , DomainsOf r ~ Data.Vector.Vector (OD.Array r) )
  => String
  -> Int -> Int -> Int -> Int -> r -> Int -> r
  -> TestTree
mnistTestCase2VTI prefix epochs maxBatches widthHidden widthHidden2
                  gamma batchSize expected =
  let nParams1 = MnistFcnnRanked1.afcnnMnistLen1 widthHidden widthHidden2
      params1Init = V.fromList $
        imap (\i nPV -> OD.fromVector [nPV]
                        $ V.map realToFrac
                        $ LA.randomVector (44 + nPV + i) LA.Uniform nPV
                          - LA.scalar 0.5)
             nParams1
      emptyR = OR.fromList [0] []
      domainsInit = mkDomains emptyR params1Init
      -- This is a very ugly and probably unavoidable boilerplate:
      -- we have to manually define a dummy value of type ADFcnnMnist1Parameters
      -- with the correct list lengths (vector lengths can be fake)
      -- to bootstrap the adaptor machinery. Such boilerplate can be
      -- avoided only with shapely typed tensors and scalars or when
      -- not using adaptors.
      -- TODO: generate this from afcnnMnistLen1.
      valsInit :: MnistFcnnRanked1.ADFcnnMnist1Parameters r
      valsInit = ( (replicate widthHidden emptyR, emptyR)
                 , (replicate widthHidden2 emptyR, emptyR)
                 , (replicate sizeMnistLabelInt emptyR, emptyR) )
      name = prefix ++ ": "
             ++ unwords [ show epochs, show maxBatches
                        , show widthHidden, show widthHidden2
                        , show (length nParams1), show (sum nParams1)
                        , show gamma ]
      ftest :: [MnistData r] -> Domains r -> r
      ftest mnist testParams =
        MnistFcnnRanked1.afcnnMnistTest1
          widthHidden widthHidden2 mnist
          (\f -> OR.toVector $ f $ parseDomains valsInit testParams)
  in testCase name $ do
       hPutStrLn stderr $
         printf "\n%s: Epochs to run/max batches per epoch: %d/%d"
                prefix epochs maxBatches
       trainData <- loadMnistData trainGlyphsPath trainLabelsPath
       testData <- take (batchSize * maxBatches)
                   <$> loadMnistData testGlyphsPath testLabelsPath
       let shapes1 = map (: []) nParams1
           (vars1, asts1) = unzip $ map funToAstD shapes1
           doms = mkDomains (AstConst emptyR) (V.fromList asts1)
           (varGlyph, astGlyph) =
             funToAstR (singletonShape sizeMnistGlyphInt) id
           (varLabel, astLabel) =
             funToAstR (singletonShape sizeMnistLabelInt) id
           ast :: Ast 0 r
           ast = tscalar
                 $ MnistFcnnRanked1.afcnnMnistLoss1TensorData
                     widthHidden widthHidden2 (astGlyph, astLabel)
                     (parseDomainsAst valsInit doms)
       -- Mimic how backprop tests and display it, even though tests
       -- should not print, in principle.
       let runBatch :: Domains r -> (Int, [MnistData r]) -> IO (Domains r)
           runBatch !domains (k, chunk) = do
             let f :: MnistData r -> ADInputs r -> ADVal r
                 f (glyph, label) varInputs =
                   let env1 = foldr extendEnvD EM.empty
                              $ zip vars1 $ V.toList
                              $ V.zipWith (dDnotShared emptyADShare)
                                          (inputPrimal1 varInputs)
                                          (inputDual1 varInputs)
                       envMnist =
                         extendEnvR varGlyph
                           (tconst $ OR.fromVector [sizeMnistGlyphInt] glyph)
                         $ extendEnvR varLabel
                             (tconst $ OR.fromVector [sizeMnistLabelInt] label)
                             env1
                   in tunScalar $ snd $ interpretAst envMnist emptyMemo ast
                 res = fst $ sgd gamma f chunk domains
                 trainScore = ftest chunk res
                 testScore = ftest testData res
                 lenChunk = length chunk
             unless (widthHidden < 10) $ do
               hPutStrLn stderr $ printf "\n%s: (Batch %d with %d points)" prefix k lenChunk
               hPutStrLn stderr $ printf "%s: Training error:   %.2f%%" prefix ((1 - trainScore) * 100)
               hPutStrLn stderr $ printf "%s: Validation error: %.2f%%" prefix ((1 - testScore ) * 100)
             return res
       let runEpoch :: Int -> Domains r -> IO (Domains r)
           runEpoch n params | n > epochs = return params
           runEpoch n params = do
             unless (widthHidden < 10) $
               hPutStrLn stderr $ printf "\n%s: [Epoch %d]" prefix n
             let trainDataShuffled = shuffle (mkStdGen $ n + 1) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf batchSize trainDataShuffled
                              -- 5000 times less data per batch
             !res <- foldM runBatch params chunks
             runEpoch (succ n) res
       res <- runEpoch 1 domainsInit
       let testErrorFinal = 1 - ftest testData res
       testErrorFinal @?~ expected

tensorIntermediateMnistTests :: TestTree
tensorIntermediateMnistTests = testGroup "ShortRankedIntermediate MNIST tests"
  [ mnistTestCase2VTI "VTI 1 epoch, 1 batch" 1 1 300 100 0.02 5000
                      (0.16479999999999995 :: Double)
  , mnistTestCase2VTI "VTI artificial 1 2 3 4 5" 1 2 3 4 5 5000
                      (0.9108 :: Float)
  , mnistTestCase2VTI "VTI artificial 5 4 3 2 1" 5 4 3 2 1 5000
                      (0.5859 :: Double)
  ]

-- JAX differentiation, Ast term built and differentiated only once
mnistTestCase2VTO
  :: forall r.
     ( ADReady r, ScalarOf r ~ r, InterpretAst r
     , PrintfArg r, AssertEqualUpToEpsilon r
     , Floating (Vector r), ADTensor r, DomainsTensor r
     , DTensorOf r ~ OD.Array r, TensorOf 1 r ~ OR.Array 1 r )
  => String
  -> Int -> Int -> Int -> Int -> r -> Int -> r
  -> TestTree
mnistTestCase2VTO prefix epochs maxBatches widthHidden widthHidden2
                  gamma batchSize expected =
  let nParams1 = MnistFcnnRanked1.afcnnMnistLen1 widthHidden widthHidden2
      params1Init = V.fromList $
        imap (\i nPV -> OD.fromVector [nPV]
                        $ V.map realToFrac
                        $ LA.randomVector (44 + nPV + i) LA.Uniform nPV
                          - LA.scalar 0.5)
             nParams1
      emptyR = OR.fromList [0] []
      domainsInit = mkDomains emptyR params1Init
      -- This is a very ugly and probably unavoidable boilerplate:
      -- we have to manually define a dummy value of type ADFcnnMnist1Parameters
      -- with the correct list lengths (vector lengths can be fake)
      -- to bootstrap the adaptor machinery. Such boilerplate can be
      -- avoided only with shapely typed tensors and scalars or when
      -- not using adaptors.
      -- TODO: generate this from afcnnMnistLen1.
      valsInit :: MnistFcnnRanked1.ADFcnnMnist1Parameters r
      valsInit = ( (replicate widthHidden emptyR, emptyR)
                 , (replicate widthHidden2 emptyR, emptyR)
                 , (replicate sizeMnistLabelInt emptyR, emptyR) )
      name = prefix ++ ": "
             ++ unwords [ show epochs, show maxBatches
                        , show widthHidden, show widthHidden2
                        , show (length nParams1), show (sum nParams1)
                        , show gamma ]
      ftest :: [MnistData r] -> Domains r -> r
      ftest mnist testParams =
        MnistFcnnRanked1.afcnnMnistTest1
          widthHidden widthHidden2 mnist
          (\f -> OR.toVector $ f $ parseDomains valsInit testParams)
  in testCase name $ do
       hPutStrLn stderr $
         printf "\n%s: Epochs to run/max batches per epoch: %d/%d"
                prefix epochs maxBatches
       trainData <- loadMnistData trainGlyphsPath trainLabelsPath
       testData <- take (batchSize * maxBatches)
                   <$> loadMnistData testGlyphsPath testLabelsPath
       let shapes1 = map (: []) nParams1
           (varGlyph, astGlyph) =
             funToAstR (singletonShape sizeMnistGlyphInt) id
           (varLabel, astLabel) =
             funToAstR (singletonShape sizeMnistLabelInt) id
           inputVars = [AstDynamicVarName varGlyph, AstDynamicVarName varLabel]
           fInterpret
             :: ADInputs (Ast0 r) -> Domains (Ast0 r)
             -> (ADAstVarNames n r, ADAstVars n r)
             -> ADVal (Ast 0 r)
           {-# INLINE fInterpret #-}
           fInterpret varInputs domains ((_, _, vars1), _) =
             -- TODO: with larger examples benchmark if not working in rank 0
             -- is costly (tscalar below)
             let ast :: Ast 0 r
                 ast = tscalar
                       $ MnistFcnnRanked1.afcnnMnistLoss1TensorData
                           widthHidden widthHidden2 (astGlyph, astLabel)
                           (parseDomainsAst valsInit domains)
                 vars1AndInput = vars1 ++ inputVars
                 env1 = foldr extendEnvD EM.empty
                        $ zip vars1AndInput
                        $ V.toList (V.zipWith (dDnotShared emptyADShare)
                                              (inputPrimal1 varInputs)
                                              (inputDual1 varInputs))
                          ++ [ dfromR $ tconstant astGlyph
                             , dfromR $ tconstant astLabel ]
             in snd $ interpretAst env1 emptyMemo ast
           (((var0Again, varDtAgain, vars1Again), gradient, primal), _) =
             revAstOnDomainsFun 0 shapes1 fInterpret
           vars1AndInputAgain = vars1Again ++ inputVars
           vars = (var0Again, varDtAgain, vars1AndInputAgain)
           go :: [MnistData r] -> Domains r -> Domains r
           go [] parameters = parameters
           go ((glyph, label) : rest) parameters =
             let glyphD = OD.fromVector [sizeMnistGlyphInt] glyph
                 labelD = OD.fromVector [sizeMnistLabelInt] label
                 parametersAndInput = parameters `V.snoc` glyphD `V.snoc` labelD
                 gradientDomain =
                   fst $ revAstOnDomainsEval (vars, gradient, primal)
                                             parametersAndInput Nothing
             in go rest (updateWithGradient gamma parameters gradientDomain)
       -- Mimic how backprop tests and display it, even though tests
       -- should not print, in principle.
       let runBatch :: Domains r -> (Int, [MnistData r]) -> IO (Domains r)
           runBatch !domains (k, chunk) = do
             let res = go chunk domains
                 trainScore = ftest chunk res
                 testScore = ftest testData res
                 lenChunk = length chunk
             unless (widthHidden < 10) $ do
               hPutStrLn stderr $ printf "\n%s: (Batch %d with %d points)" prefix k lenChunk
               hPutStrLn stderr $ printf "%s: Training error:   %.2f%%" prefix ((1 - trainScore) * 100)
               hPutStrLn stderr $ printf "%s: Validation error: %.2f%%" prefix ((1 - testScore ) * 100)
             return res
       let runEpoch :: Int -> Domains r -> IO (Domains r)
           runEpoch n params | n > epochs = return params
           runEpoch n params = do
             unless (widthHidden < 10) $
               hPutStrLn stderr $ printf "\n%s: [Epoch %d]" prefix n
             let trainDataShuffled = shuffle (mkStdGen $ n + 1) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf batchSize trainDataShuffled
                              -- 5000 times less data per batch
             !res <- foldM runBatch params chunks
             runEpoch (succ n) res
       res <- runEpoch 1 domainsInit
       let testErrorFinal = 1 - ftest testData res
       testErrorFinal @?~ expected

tensorADOnceMnistTests :: TestTree
tensorADOnceMnistTests = testGroup "ShortRankedOnce MNIST tests"
  [ mnistTestCase2VTO "VTO 1 epoch, 1 batch" 1 1 300 100 0.02 5000
                      (0.16479999999999995 :: Double)
  , mnistTestCase2VTO "VTO artificial 1 2 3 4 5" 1 2 3 4 5 5000
                      (0.9108 :: Float)
  , mnistTestCase2VTO "VTO artificial 5 4 3 2 1" 5 4 3 2 1 5000
                      (0.8304 :: Double)
  ]


-- * Using matrices, which is rank 2

-- POPL differentiation, straight via the ADVal instance of Tensor
mnistTestCase2VT2A
  :: forall r.
     ( ADReady r, ADReady (ADVal r), ScalarOf r ~ r, ScalarOf (ADVal r) ~ r
     , TensorOf 0 (ADVal r) ~ ADVal (OR.Array 0 r)
     , TensorOf 1 (ADVal r) ~ ADVal (OR.Array 1 r)
     , TensorOf 2 (ADVal r) ~ ADVal (OR.Array 2 r)
     , DTensorOf (ADVal r) ~ ADVal (OD.Array r)
     , PrintfArg r, AssertEqualUpToEpsilon r
     , Floating (Vector r), ADTensor r
     , DynamicTensor r, DomainsTensor r, Element r ~ r
     , DTensorOf r ~ OD.Array r, DomainsOf r ~ Data.Vector.Vector (OD.Array r)
     , TensorOf 1 r ~ OR.Array 1 r, TensorOf 2 r ~ OR.Array 2 r )
  => String
  -> Int -> Int -> Int -> Int -> r -> Int -> r
  -> TestTree
mnistTestCase2VT2A prefix epochs maxBatches widthHidden widthHidden2
                   gamma batchSize expected =
  let nParams1 = MnistFcnnRanked2.afcnnMnistLen2 widthHidden widthHidden2
      params1Init = V.fromList $
        imap (\i sh -> OD.fromVector sh
                       $ V.map realToFrac
                       $ LA.randomVector (44 + product sh + i) LA.Uniform
                                         (product sh)
                         - LA.scalar 0.5)
             nParams1
      -- This is a very ugly and probably unavoidable boilerplate:
      -- we have to manually define a dummy value of type ADFcnnMnist1Parameters
      -- with the correct list lengths (vector lengths can be fake)
      -- to bootstrap the adaptor machinery. Such boilerplate can be
      -- avoided only with shapely typed tensors and scalars or when
      -- not using adaptors.
      emptyR = OR.fromList [0] []
      emptyR2 = OR.fromList [0, 0] []
      valsInit :: MnistFcnnRanked2.ADFcnnMnist2Parameters r
      valsInit = ( (emptyR2, emptyR)
                 , (emptyR2, emptyR)
                 , (emptyR2, emptyR) )
      name = prefix ++ ": "
             ++ unwords [ show epochs, show maxBatches
                        , show widthHidden, show widthHidden2
                        , show (length nParams1)
                        , show (sum $ map product nParams1)
                        , show gamma ]
      ftest :: [MnistData r] -> Domains r -> r
      ftest mnist testParams =
        MnistFcnnRanked2.afcnnMnistTest2 mnist
          (\f -> OR.toVector $ f $ parseDomains valsInit testParams)
  in testCase name $ do
       hPutStrLn stderr $
         printf "\n%s: Epochs to run/max batches per epoch: %d/%d"
                prefix epochs maxBatches
       trainData <- loadMnistData trainGlyphsPath trainLabelsPath
       testData <- take (batchSize * maxBatches)
                   <$> loadMnistData testGlyphsPath testLabelsPath
       -- Mimic how backprop tests and display it, even though tests
       -- should not print, in principle.
       let runBatch :: Domains r -> (Int, [MnistData r]) -> IO (Domains r)
           runBatch !domains (k, chunk) = do
             let f mnist adinputs =
                   MnistFcnnRanked2.afcnnMnistLoss2
                     mnist (parseADInputs valsInit adinputs)
                 res = fst $ sgd gamma f chunk domains
                 trainScore = ftest chunk res
                 testScore = ftest testData res
                 lenChunk = length chunk
             unless (widthHidden < 10) $ do
               hPutStrLn stderr $ printf "\n%s: (Batch %d with %d points)" prefix k lenChunk
               hPutStrLn stderr $ printf "%s: Training error:   %.2f%%" prefix ((1 - trainScore) * 100)
               hPutStrLn stderr $ printf "%s: Validation error: %.2f%%" prefix ((1 - testScore ) * 100)
             return res
       let runEpoch :: Int -> Domains r -> IO (Domains r)
           runEpoch n params | n > epochs = return params
           runEpoch n params = do
             unless (widthHidden < 10) $
               hPutStrLn stderr $ printf "\n%s: [Epoch %d]" prefix n
             let trainDataShuffled = shuffle (mkStdGen $ n + 5) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf batchSize trainDataShuffled
             !res <- foldM runBatch params chunks
             runEpoch (succ n) res
       res <- runEpoch 1 (mkDomains emptyR params1Init)
       let testErrorFinal = 1 - ftest testData res
       testErrorFinal @?~ expected

tensorADValMnistTests2 :: TestTree
tensorADValMnistTests2 = testGroup "ShortRanked2 ADVal MNIST tests"
  [ mnistTestCase2VT2A "VT2A 1 epoch, 1 batch" 1 1 300 100 0.02 5
                       (0.8 :: Double)
  , mnistTestCase2VT2A "VT2A artificial 1 2 3 4 5" 1 2 3 4 5 5
                       (0.8 :: Float)
  , mnistTestCase2VT2A "VT2A artificial 5 4 3 2 1" 5 4 3 2 1 5
                       (0.95 :: Double)
  ]

-- POPL differentiation, Ast term defined only once but differentiated each time
mnistTestCase2VT2I
  :: forall r.
     ( ADReady r, ADReady (ADVal r), ScalarOf r ~ r, ScalarOf (ADVal r) ~ r
     , TensorOf 1 (ADVal r) ~ ADVal (OR.Array 1 r)
     , DTensorOf (ADVal r) ~ ADVal (OD.Array r)
     , InterpretAst (ADVal r)
     , PrintfArg r, AssertEqualUpToEpsilon r
     , Floating (Vector r), ADTensor r
     , DynamicTensor r, DomainsTensor r, Element r ~ r
     , DTensorOf r ~ OD.Array r, DomainsOf r ~ Data.Vector.Vector (OD.Array r)
     , TensorOf 1 r ~ OR.Array 1 r, TensorOf 2 r ~ OR.Array 2 r )
  => String
  -> Int -> Int -> Int -> Int -> r -> Int -> r
  -> TestTree
mnistTestCase2VT2I prefix epochs maxBatches widthHidden widthHidden2
                   gamma batchSize expected =
  let nParams1 = MnistFcnnRanked2.afcnnMnistLen2 widthHidden widthHidden2
      params1Init = V.fromList $
        imap (\i sh -> OD.fromVector sh
                       $ V.map realToFrac
                       $ LA.randomVector (44 + product sh + i) LA.Uniform
                                         (product sh)
                         - LA.scalar 0.5)
             nParams1
      emptyR = OR.fromList [0] []
      emptyR2 = OR.fromList [0, 0] []
      domainsInit = mkDomains emptyR params1Init
      -- This is a very ugly and probably unavoidable boilerplate:
      -- we have to manually define a dummy value of type ADFcnnMnist1Parameters
      -- with the correct list lengths (vector lengths can be fake)
      -- to bootstrap the adaptor machinery. Such boilerplate can be
      -- avoided only with shapely typed tensors and scalars or when
      -- not using adaptors.
      -- TODO: generate this from afcnnMnistLen1.
      valsInit :: MnistFcnnRanked2.ADFcnnMnist2Parameters r
      valsInit = ( (emptyR2, emptyR)
                 , (emptyR2, emptyR)
                 , (emptyR2, emptyR) )
      name = prefix ++ ": "
             ++ unwords [ show epochs, show maxBatches
                        , show widthHidden, show widthHidden2
                        , show (length nParams1)
                        , show (sum $ map product nParams1)
                        , show gamma ]
      ftest :: [MnistData r] -> Domains r -> r
      ftest mnist testParams =
        MnistFcnnRanked2.afcnnMnistTest2 mnist
          (\f -> OR.toVector $ f $ parseDomains valsInit testParams)
  in testCase name $ do
       hPutStrLn stderr $
         printf "\n%s: Epochs to run/max batches per epoch: %d/%d"
                prefix epochs maxBatches
       trainData <- loadMnistData trainGlyphsPath trainLabelsPath
       testData <- take (batchSize * maxBatches)
                   <$> loadMnistData testGlyphsPath testLabelsPath
       let shapes1 = nParams1
           (vars1, asts1) = unzip $ map funToAstD shapes1
           doms = mkDomains (AstConst emptyR) (V.fromList asts1)
           (varGlyph, astGlyph) =
             funToAstR (singletonShape sizeMnistGlyphInt) id
           (varLabel, astLabel) =
             funToAstR (singletonShape sizeMnistLabelInt) id
           ast :: Ast 0 r
           ast = tscalar
                 $ MnistFcnnRanked2.afcnnMnistLoss2TensorData
                     (astGlyph, astLabel) (parseDomainsAst valsInit doms)
       -- Mimic how backprop tests and display it, even though tests
       -- should not print, in principle.
       let runBatch :: Domains r -> (Int, [MnistData r]) -> IO (Domains r)
           runBatch !domains (k, chunk) = do
             let f :: MnistData r -> ADInputs r -> ADVal r
                 f (glyph, label) varInputs =
                   let env1 = foldr extendEnvD EM.empty
                              $ zip vars1 $ V.toList
                              $ V.zipWith (dDnotShared emptyADShare)
                                          (inputPrimal1 varInputs)
                                          (inputDual1 varInputs)
                       envMnist =
                         extendEnvR varGlyph
                           (tconst $ OR.fromVector [sizeMnistGlyphInt] glyph)
                         $ extendEnvR varLabel
                             (tconst $ OR.fromVector [sizeMnistLabelInt] label)
                             env1
                   in tunScalar $ snd $ interpretAst envMnist emptyMemo ast
                 res = fst $ sgd gamma f chunk domains
                 trainScore = ftest chunk res
                 testScore = ftest testData res
                 lenChunk = length chunk
             unless (widthHidden < 10) $ do
               hPutStrLn stderr $ printf "\n%s: (Batch %d with %d points)" prefix k lenChunk
               hPutStrLn stderr $ printf "%s: Training error:   %.2f%%" prefix ((1 - trainScore) * 100)
               hPutStrLn stderr $ printf "%s: Validation error: %.2f%%" prefix ((1 - testScore ) * 100)
             return res
       let runEpoch :: Int -> Domains r -> IO (Domains r)
           runEpoch n params | n > epochs = return params
           runEpoch n params = do
             unless (widthHidden < 10) $
               hPutStrLn stderr $ printf "\n%s: [Epoch %d]" prefix n
             let trainDataShuffled = shuffle (mkStdGen $ n + 1) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf batchSize trainDataShuffled
                              -- 5000 times less data per batch
             !res <- foldM runBatch params chunks
             runEpoch (succ n) res
       res <- runEpoch 1 domainsInit
       let testErrorFinal = 1 - ftest testData res
       testErrorFinal @?~ expected

tensorIntermediateMnistTests2 :: TestTree
tensorIntermediateMnistTests2 = testGroup "ShortRankedIntermediate2 MNIST tests"
  [ mnistTestCase2VT2I "VT2I 1 epoch, 1 batch" 1 1 300 100 0.02 500
                       (0.42200000000000004 :: Double)
  , mnistTestCase2VT2I "VT2I artificial 1 2 3 4 5" 1 2 3 4 5 500
                       (0.884 :: Float)
  , mnistTestCase2VT2I "VT2I artificial 5 4 3 2 1" 5 4 3 2 1 500
                       (0.7324999999999999 :: Double)
  ]

-- JAX differentiation, Ast term built and differentiated only once
mnistTestCase2VT2O
  :: forall r.
     ( ADReady r, ScalarOf r ~ r, InterpretAst r
     , PrintfArg r, AssertEqualUpToEpsilon r
     , Floating (Vector r), ADTensor r, DomainsTensor r
     , DTensorOf r ~ OD.Array r
     , TensorOf 1 r ~ OR.Array 1 r, TensorOf 2 r ~ OR.Array 2 r )
  => String
  -> Int -> Int -> Int -> Int -> r -> Int -> r
  -> TestTree
mnistTestCase2VT2O prefix epochs maxBatches widthHidden widthHidden2
                   gamma batchSize expected =
  let nParams1 = MnistFcnnRanked2.afcnnMnistLen2 widthHidden widthHidden2
      params1Init = V.fromList $
        imap (\i sh -> OD.fromVector sh
                       $ V.map realToFrac
                       $ LA.randomVector (44 + product sh + i) LA.Uniform
                                         (product sh)
                         - LA.scalar 0.5)
             nParams1
      emptyR = OR.fromList [0] []
      emptyR2 = OR.fromList [0, 0] []
      domainsInit = mkDomains emptyR params1Init
      -- This is a very ugly and probably unavoidable boilerplate:
      -- we have to manually define a dummy value of type ADFcnnMnist1Parameters
      -- with the correct list lengths (vector lengths can be fake)
      -- to bootstrap the adaptor machinery. Such boilerplate can be
      -- avoided only with shapely typed tensors and scalars or when
      -- not using adaptors.
      -- TODO: generate this from afcnnMnistLen1.
      valsInit :: MnistFcnnRanked2.ADFcnnMnist2Parameters r
      valsInit = ( (emptyR2, emptyR)
                 , (emptyR2, emptyR)
                 , (emptyR2, emptyR) )
      name = prefix ++ ": "
             ++ unwords [ show epochs, show maxBatches
                        , show widthHidden, show widthHidden2
                        , show (length nParams1)
                        , show (sum $ map product nParams1)
                        , show gamma ]
      ftest :: [MnistData r] -> Domains r -> r
      ftest mnist testParams =
        MnistFcnnRanked2.afcnnMnistTest2 mnist
          (\f -> OR.toVector $ f $ parseDomains valsInit testParams)
  in testCase name $ do
       hPutStrLn stderr $
         printf "\n%s: Epochs to run/max batches per epoch: %d/%d"
                prefix epochs maxBatches
       trainData <- loadMnistData trainGlyphsPath trainLabelsPath
       testData <- take (batchSize * maxBatches)
                   <$> loadMnistData testGlyphsPath testLabelsPath
       let shapes1 = nParams1
           (varGlyph, astGlyph) =
             funToAstR (singletonShape sizeMnistGlyphInt) id
           (varLabel, astLabel) =
             funToAstR (singletonShape sizeMnistLabelInt) id
           inputVars = [AstDynamicVarName varGlyph, AstDynamicVarName varLabel]
           fInterpret
             :: ADInputs (Ast0 r) -> Domains (Ast0 r)
             -> (ADAstVarNames n r, ADAstVars n r)
             -> ADVal (Ast 0 r)
           {-# INLINE fInterpret #-}
           fInterpret varInputs domains ((_, _, vars1), _) =
             -- TODO: with larger examples benchmark if not working in rank 0
             -- is costly (tscalar below)
             let ast :: Ast 0 r
                 ast = tscalar
                       $ MnistFcnnRanked2.afcnnMnistLoss2TensorData
                           (astGlyph, astLabel)
                           (parseDomainsAst valsInit domains)
                 vars1AndInput = vars1 ++ inputVars
                 env1 = foldr extendEnvD EM.empty
                        $ zip vars1AndInput
                        $ V.toList (V.zipWith (dDnotShared emptyADShare)
                                              (inputPrimal1 varInputs)
                                              (inputDual1 varInputs))
                          ++ [ dfromR $ tconstant astGlyph
                             , dfromR $ tconstant astLabel ]
             in snd $ interpretAst env1 emptyMemo ast
           (((var0Again, varDtAgain, vars1Again), gradient, primal), _) =
             revAstOnDomainsFun 0 shapes1 fInterpret
           vars1AndInputAgain = vars1Again ++ inputVars
           vars = (var0Again, varDtAgain, vars1AndInputAgain)
           go :: [MnistData r] -> Domains r -> Domains r
           go [] parameters = parameters
           go ((glyph, label) : rest) parameters =
             let glyphD = OD.fromVector [sizeMnistGlyphInt] glyph
                 labelD = OD.fromVector [sizeMnistLabelInt] label
                 parametersAndInput = parameters `V.snoc` glyphD `V.snoc` labelD
                 gradientDomain =
                   fst $ revAstOnDomainsEval (vars, gradient, primal)
                                             parametersAndInput Nothing
             in go rest (updateWithGradient gamma parameters gradientDomain)
       -- Mimic how backprop tests and display it, even though tests
       -- should not print, in principle.
       let runBatch :: Domains r -> (Int, [MnistData r]) -> IO (Domains r)
           runBatch !domains (k, chunk) = do
             let res = go chunk domains
                 trainScore = ftest chunk res
                 testScore = ftest testData res
                 lenChunk = length chunk
             unless (widthHidden < 10) $ do
               hPutStrLn stderr $ printf "\n%s: (Batch %d with %d points)" prefix k lenChunk
               hPutStrLn stderr $ printf "%s: Training error:   %.2f%%" prefix ((1 - trainScore) * 100)
               hPutStrLn stderr $ printf "%s: Validation error: %.2f%%" prefix ((1 - testScore ) * 100)
             return res
       let runEpoch :: Int -> Domains r -> IO (Domains r)
           runEpoch n params | n > epochs = return params
           runEpoch n params = do
             unless (widthHidden < 10) $
               hPutStrLn stderr $ printf "\n%s: [Epoch %d]" prefix n
             let trainDataShuffled = shuffle (mkStdGen $ n + 1) trainData
                 chunks = take maxBatches
                          $ zip [1 ..] $ chunksOf batchSize trainDataShuffled
                              -- 5000 times less data per batch
             !res <- foldM runBatch params chunks
             runEpoch (succ n) res
       res <- runEpoch 1 domainsInit
       let testErrorFinal = 1 - ftest testData res
       testErrorFinal @?~ expected

tensorADOnceMnistTests2 :: TestTree
tensorADOnceMnistTests2 = testGroup "ShortRankedOnce2 MNIST tests"
  [ mnistTestCase2VT2O "VT2O 1 epoch, 1 batch" 1 1 300 100 0.02 500
                       (0.42200000000000004 :: Double)
  , mnistTestCase2VT2O "VT2O artificial 1 2 3 4 5" 1 2 3 4 5 500
                       (0.884 :: Float)
  , mnistTestCase2VT2O "VT2O artificial 5 4 3 2 1" 5 4 3 2 1 500
                       (0.7324999999999999 :: Double)
  ]

tensorMnistTestsPP :: TestTree
tensorMnistTestsPP = testGroup "PP tests for Short Ranked MNIST tests"
  [ testCase "VTOPP" testVTOPP
  , testCase "VT2OPP" testVT2OPP
  ]

testVTOPP :: Assertion
testVTOPP = do
  resetVarCounter
  let renames = IM.empty
      valsInit :: MnistFcnnRanked1.ADFcnnMnist1Parameters Double
      valsInit =
        ( ( replicate 2 (OR.fromList [2] [1, 2])
          , OR.fromList [2] [1, 2] )
        , ( replicate 3 (OR.fromList [3] [1, 2, 3])
          , OR.fromList [3] [1, 2, 3] )
        , ( replicate 4 (OR.fromList [4] [1, 2, 3, 4])
          , OR.fromList [4] [1, 2, 3, 4] ) )
      blackGlyph = AstKonst sizeMnistGlyphInt 7
      afcnn2T :: MnistFcnnRanked1.ADFcnnMnist1Parameters (Ast0 Double)
              -> TensorOf 1 (Ast0 Double)
      afcnn2T = MnistFcnnRanked1.afcnnMnist1 id id 2 3 blackGlyph
      afcnn2TnonLin :: MnistFcnnRanked1.ADFcnnMnist1Parameters (Ast0 Double)
                    -> TensorOf 1 (Ast0 Double)
      afcnn2TnonLin =
        MnistFcnnRanked1.afcnnMnist1 logistic softMaxV 2 3 blackGlyph
  resetVarCounter
  let (artifact6, _) = revDtFun afcnn2T valsInit
  printGradient6Pretty renames artifact6
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x18 = tkonst 784 (tconst 7.0) in let x19 = tfromList [tsum (x3 * x18), tsum (x4 * x18)] + x5 in let x20 = tfromList [tsum (x6 * x19), tsum (x7 * x19), tsum (x8 * x19)] + x9 in let x21 = dret ! [3] in let x22 = dret ! [2] in let x23 = dret ! [1] in let x24 = dret ! [0] in let x25 = x10 * tkonst 4 x24 + x11 * tkonst 4 x23 + x12 * tkonst 4 x22 + x13 * tkonst 4 x21 in let x26 = x25 ! [2] in let x27 = x25 ! [1] in let x28 = x25 ! [0] in let x29 = x6 * tkonst 3 x28 + x7 * tkonst 3 x27 + x8 * tkonst 3 x26 in let x30 = x29 ! [1] in let x31 = x29 ! [0] in (tfromList [], x18 * tkonst 784 x31, x18 * tkonst 784 x30, x29, x19 * tkonst 2 x28, x19 * tkonst 2 x27, x19 * tkonst 2 x26, x25, x20 * tkonst 3 x24, x20 * tkonst 3 x23, x20 * tkonst 3 x22, x20 * tkonst 3 x21, dret)"
  printPrimal6Pretty renames artifact6
    @?= "\\s0 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x18 = tkonst 784 (tconst 7.0) in let x19 = tfromList [tsum (x3 * x18), tsum (x4 * x18)] + x5 in let x20 = tfromList [tsum (x6 * x19), tsum (x7 * x19), tsum (x8 * x19)] + x9 in tfromList [tsum (x10 * x20), tsum (x11 * x20), tsum (x12 * x20), tsum (x13 * x20)] + x14"
  printGradient6Pretty renames (simplifyArtifact6 artifact6)
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x18 = tconstant (tkonst 784 (tconst 7.0)) in let x19 = tfromList [tsum (x3 * x18), tsum (x4 * x18)] + x5 in let x20 = tfromList [tsum (x6 * x19), tsum (x7 * x19), tsum (x8 * x19)] + x9 in let x21 = dret ! [3] in let x22 = dret ! [2] in let x23 = dret ! [1] in let x24 = dret ! [0] in let x25 = x10 * tkonst 4 x24 + x11 * tkonst 4 x23 + x12 * tkonst 4 x22 + x13 * tkonst 4 x21 in let x26 = x25 ! [2] in let x27 = x25 ! [1] in let x28 = x25 ! [0] in let x29 = x6 * tkonst 3 x28 + x7 * tkonst 3 x27 + x8 * tkonst 3 x26 in let x30 = x29 ! [1] in let x31 = x29 ! [0] in (tfromList [], x18 * tkonst 784 x31, x18 * tkonst 784 x30, x29, x19 * tkonst 2 x28, x19 * tkonst 2 x27, x19 * tkonst 2 x26, x25, x20 * tkonst 3 x24, x20 * tkonst 3 x23, x20 * tkonst 3 x22, x20 * tkonst 3 x21, dret)"
  printPrimal6Pretty renames (simplifyArtifact6 artifact6)
    @?= "\\s0 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x18 = tconstant (tkonst 784 (tconst 7.0)) in let x19 = tfromList [tsum (x3 * x18), tsum (x4 * x18)] + x5 in let x20 = tfromList [tsum (x6 * x19), tsum (x7 * x19), tsum (x8 * x19)] + x9 in tfromList [tsum (x10 * x20), tsum (x11 * x20), tsum (x12 * x20), tsum (x13 * x20)] + x14"
  resetVarCounter
  let (artifact6nonLin, _) = revDtFun afcnn2TnonLin valsInit
  printGradient6Pretty renames artifact6nonLin
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in let x28 = tkonst 2 (tconst 1.0) - x27 in let x29 = x27 * x28 in let x30 = let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x34 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in let x28 = tkonst 2 (tconst 1.0) - x27 in let x29 = x27 * x28 in let x30 = let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x32 = exp (negate x31) in let x33 = tkonst 3 (tconst 1.0) + x32 in recip x33 in let x35 = tkonst 3 (tconst 1.0) - x34 in let x36 = x34 * x35 in let x37 = let x34 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in let x28 = tkonst 2 (tconst 1.0) - x27 in let x29 = x27 * x28 in let x30 = let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x32 = exp (negate x31) in let x33 = tkonst 3 (tconst 1.0) + x32 in recip x33 in x34 in let x38 = exp (tfromList [tsum (x10 * x37), tsum (x11 * x37), tsum (x12 * x37), tsum (x13 * x37)] + x14) in let x39 = tsum x38 in let x40 = tkonst 4 (recip x39) in let x41 = x38 * (tkonst 4 (negate (recip (x39 * x39)) * tsum (x38 * dret)) + x40 * dret) in let x42 = x41 ! [3] in let x43 = x41 ! [2] in let x44 = x41 ! [1] in let x45 = x41 ! [0] in let x46 = x10 * tkonst 4 x45 + x11 * tkonst 4 x44 + x12 * tkonst 4 x43 + x13 * tkonst 4 x42 in let x47 = x34 * (x31 * x46) in let x48 = x36 * x46 in let x49 = x48 ! [2] in let x50 = x48 ! [1] in let x51 = x48 ! [0] in let x52 = x6 * tkonst 3 x51 + x7 * tkonst 3 x50 + x8 * tkonst 3 x49 in let x53 = x27 * (x24 * x52) in let x54 = x29 * x52 in let x55 = x54 ! [1] in let x56 = x54 ! [0] in (tfromList [], x23 * tkonst 784 x56, x23 * tkonst 784 x55, x54, x30 * tkonst 2 x51, x30 * tkonst 2 x50, x30 * tkonst 2 x49, x48, x37 * tkonst 3 x45, x37 * tkonst 3 x44, x37 * tkonst 3 x43, x37 * tkonst 3 x42, x41)"
  printPrimal6Pretty renames artifact6nonLin
    @?= "\\s0 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in let x28 = tkonst 2 (tconst 1.0) - x27 in let x29 = x27 * x28 in let x30 = let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x34 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in let x28 = tkonst 2 (tconst 1.0) - x27 in let x29 = x27 * x28 in let x30 = let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x32 = exp (negate x31) in let x33 = tkonst 3 (tconst 1.0) + x32 in recip x33 in let x35 = tkonst 3 (tconst 1.0) - x34 in let x36 = x34 * x35 in let x37 = let x34 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in let x28 = tkonst 2 (tconst 1.0) - x27 in let x29 = x27 * x28 in let x30 = let x27 = let x23 = tkonst 784 (tconst 7.0) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x25 = exp (negate x24) in let x26 = tkonst 2 (tconst 1.0) + x25 in recip x26 in x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x32 = exp (negate x31) in let x33 = tkonst 3 (tconst 1.0) + x32 in recip x33 in x34 in let x38 = exp (tfromList [tsum (x10 * x37), tsum (x11 * x37), tsum (x12 * x37), tsum (x13 * x37)] + x14) in let x39 = tsum x38 in let x40 = tkonst 4 (recip x39) in x40 * x38"
  printGradient6Pretty renames (simplifyArtifact6 artifact6nonLin)
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x23 = tconstant (tkonst 784 (tconst 7.0)) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x25 = exp (negate x24) in let x26 = tconstant (tkonst 2 (tconst 1.0)) + x25 in recip x26 in let x28 = tconstant (tkonst 2 (tconst 1.0)) - x27 in let x29 = x27 * x28 in let x30 = x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x34 = let x32 = exp (negate x31) in let x33 = tconstant (tkonst 3 (tconst 1.0)) + x32 in recip x33 in let x35 = tconstant (tkonst 3 (tconst 1.0)) - x34 in let x36 = x34 * x35 in let x37 = x34 in let x38 = exp (tfromList [tsum (x10 * x37), tsum (x11 * x37), tsum (x12 * x37), tsum (x13 * x37)] + x14) in let x39 = tsum x38 in let x40 = tkonst 4 (recip x39) in let x41 = x38 * (tkonst 4 (negate (recip (x39 * x39)) * tsum (x38 * dret)) + x40 * dret) in let x42 = x41 ! [3] in let x43 = x41 ! [2] in let x44 = x41 ! [1] in let x45 = x41 ! [0] in let x46 = x10 * tkonst 4 x45 + x11 * tkonst 4 x44 + x12 * tkonst 4 x43 + x13 * tkonst 4 x42 in let x47 = x34 * (x31 * x46) in let x48 = x36 * x46 in let x49 = x48 ! [2] in let x50 = x48 ! [1] in let x51 = x48 ! [0] in let x52 = x6 * tkonst 3 x51 + x7 * tkonst 3 x50 + x8 * tkonst 3 x49 in let x53 = x27 * (x24 * x52) in let x54 = x29 * x52 in let x55 = x54 ! [1] in let x56 = x54 ! [0] in (tfromList [], x23 * tkonst 784 x56, x23 * tkonst 784 x55, x54, x30 * tkonst 2 x51, x30 * tkonst 2 x50, x30 * tkonst 2 x49, x48, x37 * tkonst 3 x45, x37 * tkonst 3 x44, x37 * tkonst 3 x43, x37 * tkonst 3 x42, x41)"
  printPrimal6Pretty renames (simplifyArtifact6 artifact6nonLin)
    @?= "\\s0 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 -> let x23 = tconstant (tkonst 784 (tconst 7.0)) in let x24 = tfromList [tsum (x3 * x23), tsum (x4 * x23)] + x5 in let x27 = let x25 = exp (negate x24) in let x26 = tconstant (tkonst 2 (tconst 1.0)) + x25 in recip x26 in let x28 = tconstant (tkonst 2 (tconst 1.0)) - x27 in let x29 = x27 * x28 in let x30 = x27 in let x31 = tfromList [tsum (x6 * x30), tsum (x7 * x30), tsum (x8 * x30)] + x9 in let x34 = let x32 = exp (negate x31) in let x33 = tconstant (tkonst 3 (tconst 1.0)) + x32 in recip x33 in let x35 = tconstant (tkonst 3 (tconst 1.0)) - x34 in let x36 = x34 * x35 in let x37 = x34 in let x38 = exp (tfromList [tsum (x10 * x37), tsum (x11 * x37), tsum (x12 * x37), tsum (x13 * x37)] + x14) in let x39 = tsum x38 in let x40 = tkonst 4 (recip x39) in x40 * x38"

testVT2OPP :: Assertion
testVT2OPP = do
  resetVarCounter
  let renames = IM.empty
      valsInit :: MnistFcnnRanked2.ADFcnnMnist2Parameters Double
      valsInit =
        ( (OR.fromList [2,1] [1, 2], OR.fromList [2] [1, 2])
        , (OR.fromList [3,1] [1, 2, 3], OR.fromList [3] [1, 2, 3])
        , (OR.fromList [4,1] [1, 2, 3, 4], OR.fromList [4] [1, 2, 3, 4]) )
      blackGlyph = AstKonst sizeMnistGlyphInt 7
      afcnn2T :: MnistFcnnRanked2.ADFcnnMnist2Parameters (Ast0 Double)
              -> TensorOf 1 (Ast0 Double)
      afcnn2T = MnistFcnnRanked2.afcnnMnist2 id id blackGlyph
      afcnn2TnonLin :: MnistFcnnRanked2.ADFcnnMnist2Parameters (Ast0 Double)
                    -> TensorOf 1 (Ast0 Double)
      afcnn2TnonLin = MnistFcnnRanked2.afcnnMnist2 logistic softMaxV blackGlyph
  resetVarCounter
  let (artifact6, _) = revDtFun afcnn2T valsInit
  printGradient6Simple renames artifact6
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 -> dlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x12 -> dlet (tkonst 3 (tsum (ttranspose [1,0] (x12 * x3)) + x4)) (\\x13 -> dlet (tkonst 4 (tsum (ttranspose [1,0] (x13 * x5)) + x6)) (\\x14 -> dlet (ttranspose [1,0] (tkonst 3 dret)) (\\x15 -> dlet (tsum (x7 * x15)) (\\x16 -> dlet (ttranspose [1,0] (tkonst 2 x16)) (\\x17 -> dlet (tsum (x5 * x17)) (\\x18 -> dlet (ttranspose [1,0] (tkonst 784 x18)) (\\x19 -> dmkDomains (fromList [dfromR (tfromList []), dfromR (x12 * x19), dfromR x18, dfromR (x13 * x17), dfromR x16, dfromR (x14 * x15), dfromR dret])))))))))"
  printPrimal6Simple renames artifact6
    @?= "\\s0 x3 x4 x5 x6 x7 x8 -> tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x12 -> tlet (tkonst 3 (tsum (ttranspose [1,0] (x12 * x3)) + x4)) (\\x13 -> tlet (tkonst 4 (tsum (ttranspose [1,0] (x13 * x5)) + x6)) (\\x14 -> tsum (ttranspose [1,0] (x14 * x7)) + x8)))"
  printGradient6Simple renames (simplifyArtifact6 artifact6)
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 -> dlet (tconstant (tkonst 2 (tkonst 784 (tconst 7.0)))) (\\x12 -> dlet (tkonst 3 (tsum (ttranspose [1,0] (x12 * x3)) + x4)) (\\x13 -> dlet (tkonst 4 (tsum (ttranspose [1,0] (x13 * x5)) + x6)) (\\x14 -> dlet (tgather [4,3] dret (\\[i20, i21] -> [i20])) (\\x15 -> dlet (tsum (x7 * x15)) (\\x16 -> dlet (tgather [1,2] x16 (\\[i22, i23] -> [i22])) (\\x17 -> dlet (tsum (x5 * x17)) (\\x18 -> dlet (tgather [1,784] x18 (\\[i24, i25] -> [i24])) (\\x19 -> dmkDomains (fromList [dfromR (tfromList []), dfromR (x12 * x19), dfromR x18, dfromR (x13 * x17), dfromR x16, dfromR (x14 * x15), dfromR dret])))))))))"
  printPrimal6Simple renames (simplifyArtifact6 artifact6)
    @?= "\\s0 x3 x4 x5 x6 x7 x8 -> tlet (tconstant (tkonst 2 (tkonst 784 (tconst 7.0)))) (\\x12 -> tlet (tkonst 3 (tsum (ttranspose [1,0] (x12 * x3)) + x4)) (\\x13 -> tlet (tkonst 4 (tsum (ttranspose [1,0] (x13 * x5)) + x6)) (\\x14 -> tsum (ttranspose [1,0] (x14 * x7)) + x8)))"
  resetVarCounter
  let (artifact6nonLin, _) = revDtFun afcnn2TnonLin valsInit
  printGradient6Simple renames artifact6nonLin
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 -> dlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> dlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> dlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> dlet (tkonst 2 (tconst 1.0) - x21) (\\x22 -> dlet (x21 * x22) (\\x23 -> dlet (tkonst 3 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> x21))) (\\x24 -> dlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> dlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> tlet (tkonst 2 (tconst 1.0) - x21) (\\x22 -> tlet (x21 * x22) (\\x23 -> tlet (tkonst 3 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> x21))) (\\x24 -> tlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> tlet (exp (negate x25)) (\\x26 -> tlet (tkonst 3 (tconst 1.0) + x26) (\\x27 -> recip x27)))))))))) (\\x28 -> dlet (tkonst 3 (tconst 1.0) - x28) (\\x29 -> dlet (x28 * x29) (\\x30 -> dlet (tkonst 4 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> tlet (tkonst 2 (tconst 1.0) - x21) (\\x22 -> tlet (x21 * x22) (\\x23 -> tlet (tkonst 3 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> x21))) (\\x24 -> tlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> tlet (exp (negate x25)) (\\x26 -> tlet (tkonst 3 (tconst 1.0) + x26) (\\x27 -> recip x27)))))))))) (\\x28 -> x28))) (\\x31 -> dlet (exp (tsum (ttranspose [1,0] (x31 * x7)) + x8)) (\\x32 -> dlet (tsum x32) (\\x33 -> dlet (tkonst 4 (recip x33)) (\\x34 -> dlet (x32 * (tkonst 4 (negate (recip (x33 * x33)) * tsum (x32 * dret)) + x34 * dret)) (\\x35 -> dlet (ttranspose [1,0] (tkonst 3 x35)) (\\x36 -> dlet (tsum (x7 * x36)) (\\x37 -> dlet (x28 * (x25 * x37)) (\\x38 -> dlet (x30 * x37) (\\x39 -> dlet (ttranspose [1,0] (tkonst 2 x39)) (\\x40 -> dlet (tsum (x5 * x40)) (\\x41 -> dlet (x21 * (x18 * x41)) (\\x42 -> dlet (x23 * x41) (\\x43 -> dlet (ttranspose [1,0] (tkonst 784 x43)) (\\x44 -> dmkDomains (fromList [dfromR (tfromList []), dfromR (x17 * x44), dfromR x43, dfromR (x24 * x40), dfromR x39, dfromR (x31 * x36), dfromR x35])))))))))))))))))))))))))"
  printPrimal6Simple renames artifact6nonLin
    @?= "\\s0 x3 x4 x5 x6 x7 x8 -> tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> tlet (tkonst 2 (tconst 1.0) - x21) (\\x22 -> tlet (x21 * x22) (\\x23 -> tlet (tkonst 3 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> x21))) (\\x24 -> tlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> tlet (tkonst 2 (tconst 1.0) - x21) (\\x22 -> tlet (x21 * x22) (\\x23 -> tlet (tkonst 3 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> x21))) (\\x24 -> tlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> tlet (exp (negate x25)) (\\x26 -> tlet (tkonst 3 (tconst 1.0) + x26) (\\x27 -> recip x27)))))))))) (\\x28 -> tlet (tkonst 3 (tconst 1.0) - x28) (\\x29 -> tlet (x28 * x29) (\\x30 -> tlet (tkonst 4 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> tlet (tkonst 2 (tconst 1.0) - x21) (\\x22 -> tlet (x21 * x22) (\\x23 -> tlet (tkonst 3 (tlet (tlet (tkonst 2 (tkonst 784 (tconst 7.0))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (exp (negate x18)) (\\x19 -> tlet (tkonst 2 (tconst 1.0) + x19) (\\x20 -> recip x20))))) (\\x21 -> x21))) (\\x24 -> tlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> tlet (exp (negate x25)) (\\x26 -> tlet (tkonst 3 (tconst 1.0) + x26) (\\x27 -> recip x27)))))))))) (\\x28 -> x28))) (\\x31 -> tlet (exp (tsum (ttranspose [1,0] (x31 * x7)) + x8)) (\\x32 -> tlet (tsum x32) (\\x33 -> tlet (tkonst 4 (recip x33)) (\\x34 -> x34 * x32))))))))))))))"
  printGradient6Simple renames (simplifyArtifact6 artifact6nonLin)
    @?= "\\s0 dret x3 x4 x5 x6 x7 x8 -> dlet (tconstant (tkonst 2 (tkonst 784 (tconst 7.0)))) (\\x17 -> dlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> dlet (tlet (exp (negate x18)) (\\x19 -> tlet (tconstant (tkonst 2 (tconst 1.0)) + x19) (\\x20 -> recip x20))) (\\x21 -> dlet (tconstant (tkonst 2 (tconst 1.0)) - x21) (\\x22 -> dlet (x21 * x22) (\\x23 -> dlet (tkonst 3 x21) (\\x24 -> dlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> dlet (tlet (exp (negate x25)) (\\x26 -> tlet (tconstant (tkonst 3 (tconst 1.0)) + x26) (\\x27 -> recip x27))) (\\x28 -> dlet (tconstant (tkonst 3 (tconst 1.0)) - x28) (\\x29 -> dlet (x28 * x29) (\\x30 -> dlet (tkonst 4 x28) (\\x31 -> dlet (exp (tsum (ttranspose [1,0] (x31 * x7)) + x8)) (\\x32 -> dlet (tsum x32) (\\x33 -> dlet (tkonst 4 (recip x33)) (\\x34 -> dlet (x32 * (tkonst 4 (negate (recip (x33 * x33)) * tsum (x32 * dret)) + x34 * dret)) (\\x35 -> dlet (tgather [4,3] x35 (\\[i45, i46] -> [i45])) (\\x36 -> dlet (tsum (x7 * x36)) (\\x37 -> dlet (x28 * (x25 * x37)) (\\x38 -> dlet (x30 * x37) (\\x39 -> dlet (tgather [3,2] x39 (\\[i47, i48] -> [i47])) (\\x40 -> dlet (tsum (x5 * x40)) (\\x41 -> dlet (x21 * (x18 * x41)) (\\x42 -> dlet (x23 * x41) (\\x43 -> dlet (tgather [2,784] x43 (\\[i49, i50] -> [i49])) (\\x44 -> dmkDomains (fromList [dfromR (tfromList []), dfromR (x17 * x44), dfromR x43, dfromR (x24 * x40), dfromR x39, dfromR (x31 * x36), dfromR x35])))))))))))))))))))))))))"
  printPrimal6Simple renames (simplifyArtifact6 artifact6nonLin)
    @?= "\\s0 x3 x4 x5 x6 x7 x8 -> tlet (tconstant (tkonst 2 (tkonst 784 (tconst 7.0)))) (\\x17 -> tlet (tsum (ttranspose [1,0] (x17 * x3)) + x4) (\\x18 -> tlet (tlet (exp (negate x18)) (\\x19 -> tlet (tconstant (tkonst 2 (tconst 1.0)) + x19) (\\x20 -> recip x20))) (\\x21 -> tlet (tconstant (tkonst 2 (tconst 1.0)) - x21) (\\x22 -> tlet (x21 * x22) (\\x23 -> tlet (tkonst 3 x21) (\\x24 -> tlet (tsum (ttranspose [1,0] (x24 * x5)) + x6) (\\x25 -> tlet (tlet (exp (negate x25)) (\\x26 -> tlet (tconstant (tkonst 3 (tconst 1.0)) + x26) (\\x27 -> recip x27))) (\\x28 -> tlet (tconstant (tkonst 3 (tconst 1.0)) - x28) (\\x29 -> tlet (x28 * x29) (\\x30 -> tlet (tkonst 4 x28) (\\x31 -> tlet (exp (tsum (ttranspose [1,0] (x31 * x7)) + x8)) (\\x32 -> tlet (tsum x32) (\\x33 -> tlet (tkonst 4 (recip x33)) (\\x34 -> x34 * x32))))))))))))))"

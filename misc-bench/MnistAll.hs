module Main (main) where

import Prelude

import Control.DeepSeq
import Criterion.Main

import qualified MnistBackpropTools
import qualified MnistMostlyHarmlessTools
import           MnistTools

main :: IO ()
main = do
  testData0 <- loadMnistData testGlyphsPath testLabelsPath  -- 10k total
  let !testData = deepseq testData0 testData0
  defaultMain
    [ bgroup "5"
        [ bgroup "ours"
            [ MnistMostlyHarmlessTools.mnistTrainBGroup2 testData 5
            ]
        , bgroup "backprop"
            [ MnistBackpropTools.backpropBgroupUnboxed testData 5
            ]
        ]
    , bgroup "50"
        [ bgroup "ours"
            [ MnistMostlyHarmlessTools.mnistTrainBGroup2 testData 50
            ]
        , bgroup "backprop"
            [ MnistBackpropTools.backpropBgroupUnboxed testData 50
            ]
        ]
    , bgroup "500"
        [ bgroup "ours"
            [ MnistMostlyHarmlessTools.mnistTrainBGroup2 testData 500
            ]
        , bgroup "backprop"
            [ MnistBackpropTools.backpropBgroupUnboxed testData 500
            ]
        ]
    ]

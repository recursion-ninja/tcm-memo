{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import Bio.Character.Exportable.Class
import Control.DeepSeq
import Criterion.Main
import Data.Foldable
import Data.TCM.Memoized.FFI
import Foreign.C.Types


newtype MyStruct = T [CULong]
    deriving (Show)


instance Exportable MyStruct where

    toExportableBuffer (T xs) =
        ExportableCharacterSequence
        { exportedElementCountSequence = 5
        , exportedElementWidthSequence = length xs
        , exportedBufferChunks         = xs
        }
    fromExportableBuffer   = T . exportedBufferChunks
    toExportableElements   = undefined
    fromExportableElements = undefined


instance NFData MyStruct where

    rnf (T xs) = foldl' (const rnf) () xs

  
main :: IO ()
main = defaultMain [
    bgroup "tcm" [ bench  "1" $ nf (f  1) 16
                 , bench  "2" $ nf (f  2) 16
                 , bench  "4" $ nf (f  4) 16
                 , bench  "8" $ nf (f  8) 16
                 , bench "16" $ nf (f 16) 16
                 ]
    ]
  where
    f x y = getMedianAndCost linearNormTCM (T [x]) (T [y])


linearNormTCM :: MemoizedCostMatrix
linearNormTCM = getMemoizedCostMatrix 5 linearNorm
  where
    linearNorm i j = max i j - min i j

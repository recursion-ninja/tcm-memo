{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import Bio.Character.Exportable.Class
import Control.DeepSeq
import Data.Foldable
import Data.TCM.Memoized.FFI
import Foreign.C.Types
import Test.QuickCheck


newtype MyStruct = T [CULong]
    deriving (Show)


instance Arbitrary MyStruct where

    arbitrary = T . pure <$> choose (1,31)


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
main = quickCheck prop
  where
    f = getMedianAndCost linearNormTCM

    prop :: MyStruct -> MyStruct -> Bool
    prop x y = rnf (f x y) == ()


linearNormTCM :: MemoizedCostMatrix
linearNormTCM = getMemoizedCostMatrix 5 linearNorm
  where
    linearNorm i j = max i j - min i j

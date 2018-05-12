{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import Bio.Character.Exportable.Class
import Control.DeepSeq
import Data.Bits
import Data.TCM.Memoized.FFI
import Foreign.C.Types
import Test.Tasty
import Test.Tasty.QuickCheck


data MyStruct = T Int Integer -- [CULong]
    deriving (Show)


newtype DNA = DNA MyStruct
    deriving (Show)


newtype Protein = Protein MyStruct
    deriving (Show)


newtype SmallTwoWords = SmallTwoWords MyStruct
    deriving (Show)


newtype LargeTwoWords = LargeTwoWords MyStruct
    deriving (Show)


newtype FourWords = FourWords MyStruct
    deriving (Show)


newtype Biggest = Biggest MyStruct
    deriving (Show)


instance Arbitrary DNA where

    arbitrary = DNA . T 5 <$> choose (1, 31)


instance Arbitrary Protein where

    arbitrary = Protein . T 22 . pred <$> choose (0, 1 `shiftL` 22)


instance Arbitrary SmallTwoWords where

    arbitrary = SmallTwoWords . T 80 . pred <$> choose (0, 1 `shiftL` 80)


instance Arbitrary LargeTwoWords where

    arbitrary = LargeTwoWords . T 80 . pred <$> choose (0, 1 `shiftL` 125)


instance Arbitrary FourWords where

    arbitrary = FourWords . T 250 . pred <$> choose (0, 1 `shiftL` 250)


instance Arbitrary Biggest where

    arbitrary = Biggest . T 360 . pred <$> choose (0, 1 `shiftL` 360)


instance Exportable MyStruct where

    toExportableElements   = undefined
    fromExportableElements = undefined
    toExportableBuffer (T d i) =
        ExportableCharacterSequence
        { exportedElementCountSequence = d
        , exportedElementWidthSequence = length packedElems
        , exportedBufferChunks         = packedElems
        }
      where
        allBitsValue = (1 `shift` wordWidth) - 1
        wordWidth    = finiteBitSize (undefined :: CULong)
        
        packedElems = go i []

        go :: Integer -> [CULong] -> [CULong]
        go x acc
          | x <= allBitsValue = reverse $ fromIntegral x : acc
          | otherwise = let (q,r) = force $ x `quotRem` toEnum wordWidth
                        in  go q $ fromIntegral r : acc

    fromExportableBuffer xs = let (d,i) = foldr f (0,0) $ exportedBufferChunks xs
                              in T d i
      where
        wordWidth = finiteBitSize (undefined :: CULong)
        f e (c,i) =
            let v = i + (fromIntegral e `shiftL` (wordWidth * c))
            in  (c+1,v)


instance NFData MyStruct where

    rnf (T d i) = rnf d `seq` rnf i


main :: IO ()
main = defaultMain $ testGroup "TCM I/O"
    [ testProperty "[  5]> DNA"         testDNA
    , testProperty "[ 22]> Protien"     testProtein
    , testProperty "[ 80]> Small2Words" testSmallTwoWords
    , testProperty "[125]> Large2Words" testLargeTwoWords
    , testProperty "[250]> FourWords"   testFourWords
    , testProperty "[360]> Biggest"     testBiggest
    ]
  where
    linearNormTCM_5   = getMemoizedCostMatrix   5 linearNorm
    linearNormTCM_22  = getMemoizedCostMatrix  22 linearNorm
    linearNormTCM_80  = getMemoizedCostMatrix  80 linearNorm
    linearNormTCM_125 = getMemoizedCostMatrix 125 linearNorm
    linearNormTCM_250 = getMemoizedCostMatrix 250 linearNorm
    linearNormTCM_360 = getMemoizedCostMatrix 360 linearNorm

    linearNorm i j = max i j - min i j
    
    testDNA :: DNA -> DNA -> Property
    testDNA (DNA x) (DNA y) = 
        rnf (getMedianAndCost linearNormTCM_5 x y) === ()
    
    testProtein :: Protein -> Protein -> Property
    testProtein (Protein x) (Protein y) = 
        rnf (getMedianAndCost linearNormTCM_22 x y) === ()

    testSmallTwoWords :: SmallTwoWords -> SmallTwoWords -> Property
    testSmallTwoWords (SmallTwoWords x) (SmallTwoWords y) = 
        rnf (getMedianAndCost linearNormTCM_80 x y) === ()

    testLargeTwoWords :: LargeTwoWords -> LargeTwoWords -> Property
    testLargeTwoWords (LargeTwoWords x) (LargeTwoWords y) = 
        rnf (getMedianAndCost linearNormTCM_125 x y) === ()

    testFourWords :: FourWords -> FourWords -> Property
    testFourWords (FourWords x) (FourWords y) = 
        rnf (getMedianAndCost linearNormTCM_250 x y) === ()

    testBiggest :: Biggest -> Biggest -> Property
    testBiggest (Biggest x) (Biggest y) = 
        rnf (getMedianAndCost linearNormTCM_360 x y) === ()


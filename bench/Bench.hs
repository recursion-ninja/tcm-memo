{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import Bio.Character.Exportable.Class
import Control.DeepSeq
import Criterion.Main
import Data.Bits
import Data.Foldable
import Data.Semigroup
import Data.TCM.Memoized.FFI
import Foreign.C.Types


data MyStruct = T Int Integer -- [CULong]
    deriving (Show)


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
main = defaultMain
    [ benchmarkDimension 5
    , benchmarkDimension 22
    , benchmarkDimension 64
    , benchmarkDimension 128
    , benchmarkDimension 256
    , benchmarkDimension 360
    ]


benchmarkDimension :: Int -> Benchmark
benchmarkDimension dimmension = bgroup ("tcm:"<> show dimmension)
    [ bench (labelA<>"<!>"<>labelB) $ nf (f lhs) rhs
    | (labelA, lhs) <- values
    , (labelB, rhs) <- values
    , lhs < rhs
    ]
  where
    values =
        [ ("first", firstBit)
        , ("last" , lastBit)
        , ("odd"  , interspersed)
        , ("even" , rotated)
        , ("twos" , twoPowers)
        ]

    firstBit = 1
 
    lastBit = 1 `shiftL` (dimmension - 1)
 
    interspersed = foldl' (.|.) 0 $ (shiftL 1) <$> take (dimmension `div` 2) [0,2..]

    rotated = interspersed `rotateR` 1

    twoPowers = sum . fmap (shiftL 1) . takeWhile (< dimmension) $ (pred . shiftL 1) <$> [1..]

    f x y = getMedianAndCost linearNormTCM (T dimmension x) (T dimmension y)

    linearNormTCM :: MemoizedCostMatrix
    linearNormTCM = force $ getMemoizedCostMatrix (toEnum dimmension) linearNorm
      where
        linearNorm i j = max i j - min i j


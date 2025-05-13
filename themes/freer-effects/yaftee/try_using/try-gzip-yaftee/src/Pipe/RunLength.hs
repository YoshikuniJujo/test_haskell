{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.RunLength where

import Control.Arrow
import Data.List qualified as L
import Data.Word
import Data.ByteString qualified as BS
import Data.Gzip.Calc

data R = Literal Word8 | LiteralBS BS.ByteString
	| LenDist Length Dist | EndOfInput deriving Show

type Length = Int
type Dist = Int

runLengthToLitLen :: R -> [Int]
runLengthToLitLen (Literal b) = [fromIntegral b]
runLengthToLitLen (LiteralBS bs) = fromIntegral <$> BS.unpack bs
runLengthToLitLen (LenDist ln _dst) = [fst $ lengthToCode ln]
runLengthToLitLen EndOfInput = [256]

toLitLenFreqs :: [R] -> [(Int, Int)]
toLitLenFreqs = ((head &&& length) <$>) . L.group . L.sort . (runLengthToLitLen =<<)

runLengthToDist :: R -> [Int]
runLengthToDist (Literal _) = []
runLengthToDist (LiteralBS _) = []
runLengthToDist (LenDist _ln dst) = [fst $ distToCode dst]
runLengthToDist EndOfInput = []

toDistFreqs :: [R] -> [(Int, Int)]
toDistFreqs = ((head &&& length) <$>) . L.group . L.sort . (runLengthToDist =<<)

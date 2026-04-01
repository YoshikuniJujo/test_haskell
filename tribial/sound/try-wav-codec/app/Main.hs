{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

main :: IO ()
main = do
	ewv <- stripRiff <$> BS.readFile "/home/tatsuya/tmp/aaaaa.wav"
	either error checkStriped ewv

checkStriped :: BS.ByteString -> IO ()
checkStriped bs = do
	let	(fmt, chs) = BS.splitAt 4 bs
		(fch, r1) = chunk chs
	print fmt
	print fch
	printSome . fst $ chunk r1

stripRiff :: BS.ByteString -> Either String BS.ByteString
stripRiff bs = case BS.splitAt 4 bs of
	("RIFF", r1) -> let
		(sz, r2) = first (
				foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)
				) 0 . BS.unpack )
			$ BS.splitAt 4 r1
		(pl, r3) = BS.splitAt sz r2 in
		Right pl
	_ -> Left "no RIFF magic number"

chunk :: BS.ByteString -> (Chunk, BS.ByteString)
chunk bs = let
	(fcc, r1) = BS.splitAt 4 bs
	(sz, r2) = first (
			foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)
			) 0 . BS.unpack )
		$ BS.splitAt 4 r1
	(pl, r3) = BS.splitAt sz r2 in
	(Chunk fcc pl, r3)

data Chunk = Chunk {
	chunkFourCC :: BS.ByteString,
	chunkPayload :: BS.ByteString }
	deriving Show

printSome :: Chunk -> IO ()
printSome = putStrLn . showSome

showSome :: Chunk -> String
showSome Chunk { chunkFourCC = fcc, chunkPayload = pl } =
	"(Chunk " ++ BSC.unpack fcc ++ " " ++ take 100 (BSC.unpack pl) ++ "...)"

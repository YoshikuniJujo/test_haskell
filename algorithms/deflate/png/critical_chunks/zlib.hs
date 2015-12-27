{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

import ReadIdat

data Cmf = Deflate Int | CmfRaw Word8 deriving Show

data FLvl = FLvl Word8 deriving Show

data FDct = FDct Adler32 deriving Show

data Adler32 = Adler32 Word32 deriving Show

header :: StateT BS.ByteString Maybe (Cmf, FLvl, Maybe FDct)
header = do
	(c, w) <- item 1 cmf
	(b, l) <- item 1 . flg $ fromIntegral w
	md <- if b
		then (Just <$>) . item 4
			$ ((Just . FDct . Adler32) .) . flip BS.foldl' 0 . curry
			$ uncurry (.|.) . ((`shiftL` 8) *** fromIntegral)
		else return Nothing
	return (c, l, md)

cmf :: BS.ByteString -> Maybe (Cmf, Word8)
cmf bs = case BS.uncons bs of
	Just (w, "")
		| w .&. 0x0f == 8 -> Just (Deflate $ 2 ^ (w `shiftR` 4 + 8), w)
		| otherwise -> Just (CmfRaw w, w)
	_ -> Nothing

flg :: Word16 -> BS.ByteString -> Maybe (Bool, FLvl)
flg c bs = case BS.uncons bs of
	Just (w, "") -> case (c `shiftL` 8 .|. fromIntegral w) `mod` 31 of
		0 -> Just (w `testBit` 5, FLvl $ w `shiftR` 6)
		_ -> Nothing
	_ -> Nothing

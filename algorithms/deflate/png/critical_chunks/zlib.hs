{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.State
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

import ReadIdat

data Cmf = Deflate Int | CmfRaw Word8 deriving Show

data FLevel = FLevel Word8 deriving Show

data FDict = FDict Adler32 deriving Show

data Adler32 = Adler32 Word32 deriving Show

cmf :: State BS.ByteString (Maybe (Cmf, Word8))
cmf = get >>= \s -> case BS.uncons s of
	Just (w, s') -> put s' >> (return $ Just (readCmf w, w))
	_ -> return Nothing

readCmf :: Word8 -> Cmf
readCmf w
	| w .&. 0x0f == 8 = Deflate $ 2 ^ (w `shiftR` 4 + 8)
	| otherwise = CmfRaw w

flg :: Word16 -> State BS.ByteString (Maybe (Bool, FLevel))
flg c = get >>= \s -> case BS.uncons s of
	Just (w, s') -> put s' >>
		case (c `shiftL` 8 .|. fromIntegral w) `mod` 31 of
			0 -> return $ Just
				(w `testBit` 5, FLevel $ w `shiftR` 6)
			_ -> return Nothing
	_ -> return Nothing

fdict :: Bool -> State BS.ByteString (Maybe FDict)
fdict False = return Nothing
fdict _ = get >>= \s -> let (d, s') =  BS.splitAt 4 s in do
	put s'
	return . Just . FDict . Adler32 $ toNum d

toNum :: (Bits n, Num n) => BS.ByteString -> n
toNum = tb zeroBits
	where
	tb r bs = case BS.uncons bs of
		Just (w, bs') -> tb (r `shiftL` 8 .|. fromIntegral w) bs'
		_ -> r

-- header :: State BS.ByteString (Maybe (Cmf, FLevel, Maybe FDict))

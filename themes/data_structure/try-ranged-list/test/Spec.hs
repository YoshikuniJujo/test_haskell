{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Stack
import GHC.TypeNats
import Control.Exception
import Data.List.Length
import Data.Bits
import Data.Bool
import Data.Word

data Bit = O | I deriving (Show, Enum)

toWord8 :: LengthL 8 Bit -> Word8
toWord8 = foldr (\b w -> w `shift` 1 .|. fromIntegral (fromEnum b)) 0

class FromBits n where fromBits :: (HasCallStack, Bits i) => i -> LengthL n Bit

instance FromBits 0 where
	fromBits i
		| i == zeroBits = NilL
		| otherwise = error "overflow"

instance {-# OVERLAPPABLE #-} FromBits (n - 1) => FromBits n where
	fromBits i = bool O I (testBit i 0) :. fromBits (i `shiftR` 1)

fromWord8 :: Word8 -> LengthL 8 Bit
fromWord8 = fromBits

fromWord8' :: Word8 -> LengthL 8 Bit
fromWord8' = unfoldr \n -> (bool O I $ testBit n 0, n `shiftR` 1)

main :: IO ()
main = do
	print . toWord8 $ O :. I :. I :. O :. O :. I :. O :. O :. NilL
	print $ fromWord8 38
	print $ fromWord8' 38
	print (fromBits (256 :: Word) :: LengthL 8 Bit) `catch` \(e :: ErrorCall) -> print e

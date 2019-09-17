{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

type family List x

type instance List () = Int
type instance List Bool = (Int, Integer)
type instance List Word8 = BS.ByteString
type instance List Double = [Double]

class Packable p where
	fromList :: [p] -> List p
	toList :: List p -> [p]

instance Packable () where
	fromList = length
	toList = (`replicate` ())

instance Packable Bool where
	fromList = \case
		[] -> (0, 0)
		b : bs -> (+ 1) *** (bool 0 1 b .|.) . (`shiftL` 1)
			$ fromList bs
	toList (l, n) | l <= 0 || n < 0 = []
	toList (l, n) = n `testBit` 0 : toList (l - 1, n `shiftR` 1)

instance Packable Word8 where
	fromList = BS.pack
	toList = BS.unpack

instance Packable Double where
	fromList = id
	toList = id

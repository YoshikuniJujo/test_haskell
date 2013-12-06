{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Int

class Same a b where
	convert :: a -> b
	reconvert :: b -> a

data Bit = Off | On deriving Show

instance Same Bool Bit where
	convert True = On
	convert _ = Off
	reconvert On = True
	reconvert _ = False

class Include a b where
	reduce :: a -> Maybe b
	enlarge :: b -> a

instance Include Integer Int where
	reduce i =
		case (i > toInteger (maxBound :: Int), i < toInteger (minBound :: Int)) of
		(False, False) -> Just $ fromInteger i
		_ -> Nothing
	enlarge = toInteger

instance Include Int Int16 where
	reduce i = case (i > mx, i < mn) of
		(False, False) -> Just $ fromIntegral i
		_ -> Nothing
		where
		mx = fromIntegral (maxBound :: Int16)
		mn = fromIntegral (minBound :: Int16)
	enlarge = fromIntegral

data Bool3 = False' | Unknown | True' deriving Show

instance Include Bool3 Bool where
	reduce False' = Just False
	reduce True' = Just True
	reduce _ = Nothing
	enlarge False = False'
	enlarge True = True'

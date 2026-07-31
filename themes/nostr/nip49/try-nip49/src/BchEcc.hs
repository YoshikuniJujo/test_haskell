{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BchEcc where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.Word
import Data.Char

import MyWords

gen :: [Word30]
gen = [	0x3b6a57b2,
	0x26508e6d,
	0x1ea119fa,
	0x3d4233dd,
	0x2a1462b3 ]

applyGen :: Word5 -> Word30 -> Word30
applyGen w5 w30 = let
	gen' = zipWith (\i g -> if testBit w5 i then (`xor` g) else id) [0 .. 4] gen in
	foldr ($) w30 gen'

shift5M :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe (Word5, Word30))
shift5M gt w30 = do
	mdt <- gt
	case mdt of
		Nothing -> pure Nothing
		Just dt -> let	bs = w30 `shiftR` 25
				w30' = w30 `shiftL` 5 .|. fromIntegral dt in
			pure $ Just (fromIntegral bs, w30')

class Pop5Bits a where pop5Bits :: a -> (Maybe Word5, a)

shift5 :: Pop5Bits a => Word30 -> a -> (Maybe (Word5, Word30), a)
shift5 w30 ws = shift5M gt w30 `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

stepM :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe Word30)
stepM gt w30 = do
	p <- shift5M gt w30
	pure $ uncurry applyGen <$> p

step :: Pop5Bits a => Word30 -> a -> (Maybe Word30, a)
step w30 ws = stepM gt w30 `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

stepsM :: Monad m => m (Maybe Word5) -> Word30 -> m Word30
stepsM gt w30 = stepM gt w30 >>= \case
	Nothing -> pure w30
	Just w30' -> stepsM gt w30'

steps :: Pop5Bits a => Word30 -> a -> (Word30, a)
steps w30 ws = stepsM gt w30 `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

newtype Word5List = Word5List [Word5] deriving Show

instance Pop5Bits Word5List where
	pop5Bits (Word5List []) = (Nothing, Word5List [])
	pop5Bits (Word5List (w : ws)) = (Just w, Word5List ws)

polymodM :: Monad m => m (Maybe Word5) -> m Word30
polymodM gt = do
	w30 <- stepsM gt 1
	pure $ fst (steps w30 $ Word5List [0, 0, 0, 0, 0, 0]) `xor` 1

polymod :: Pop5Bits a => a -> Word30
polymod ws = fst $ polymodM gt `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

polymodL :: [Word5] -> Word30
polymodL = polymod . Word5List

polymodNoTailM :: Monad m => m (Maybe Word5) -> m Word30
polymodNoTailM gt = stepsM gt 1

polymodNoTail :: Pop5Bits a => a -> Word30
polymodNoTail ws = fst $ polymodNoTailM (StateT $ Identity . pop5Bits) `runState` ws

polymodNoTailL :: [Word5] -> Word30
polymodNoTailL = polymodNoTail . Word5List

hrpToW5s :: String -> [Word5]
hrpToW5s hrp =
	(fromIntegral . (`shiftR` 5) . ord <$> hrp) ++ [0] ++
	(fromIntegral . (.&. 0x1f) . ord <$> hrp)

dataToW5, dataToW5DropTail :: String -> [Word5]
dataToW5 = (dict <$>)
dataToW5DropTail = (dict <$>) . dropRight 6

dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse

dictChars :: [Char]
dictChars = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

dict :: Char -> Word5
dict = fromIntegral . fromJust . (`L.elemIndex` dictChars)

undict :: Word5 -> Char
undict = (dictChars !!) . fromIntegral

word30ToB :: Word30 -> String
word30ToB = (undict <$>) . word30ToWord5List

hrpDataToW5s :: String -> Either String [Word5]
hrpDataToW5s hrpdt = do
	Separated { humanReadPart = hrp, dataPart = dt } <- sepHrpDt hrpdt
	pure $ hrpToW5s hrp ++ dataToW5 dt

checkedToHdrDat :: Checked -> Either String [Word8]
checkedToHdrDat (Checked hdr dt) = word5sToWord8s dt

hrpDataToHprBytes :: String -> Either String (String, [Word8])
hrpDataToHprBytes hrpdt = do
	Separated { humanReadPart = hrp, dataPart = dt } <- sepHrpDt hrpdt
	(hrp ,) <$> word5sToWord8s (dataToW5DropTail dt)

sepHrpDt :: String -> Either String Separated
sepHrpDt = (tupleToSeparated <$>)
	. either (Right . (init `first`))
		(const $ Left "Bech32: no separator 1") . spanRR (/= '1')

spanRR :: (a -> Bool) -> [a] -> Either ([a], [a]) [a]
spanRR _ [] = Right []
spanRR p (x : xs) = case (p x, spanRR p xs) of
	(_, Left (t, d)) -> Left (x : t, d)
	(False, Right d) -> Left ([x], d)
	(True, Right d) -> Right $ x : d

check :: String -> Separated -> Either String Checked
check hrp0 Separated { humanReadPart = hrp, dataPart = dp } =
	case polymodNoTailL $ hrpToW5s hrp ++ dp' of
		1	| hrp == hrp0 -> Right Checked {
				checkedHumanReadPart = hrp,
				checkedDataPart = take (length dp' - 6) dp' }
			| otherwise -> Left $ "HRP should be " ++ show hrp0
		_ -> Left "Bech32: Checksum should be 1"
	where dp' = dataToW5 dp

data Checked = Checked {
	checkedHumanReadPart :: String,
	checkedDataPart :: [Word5] }
	deriving (Show, Eq)

data Separated = Separated {
	humanReadPart :: String,
	dataPart :: String }
	deriving Show

tupleToSeparated = uncurry Separated

computeChecksum :: Checked -> [Word5]
computeChecksum Checked {
	checkedHumanReadPart = hrp,
	checkedDataPart = dp } = word30ToWord5List . polymodL $ hrpToW5s hrp ++ dp

checkedToBech32 :: Checked -> String
checkedToBech32 c@Checked {
	checkedHumanReadPart = hrp,
	checkedDataPart = dp } = hrp ++ "1" ++ ((dictChars !!) . fromIntegral <$> (dp ++ cs))
	where
	cs = computeChecksum c

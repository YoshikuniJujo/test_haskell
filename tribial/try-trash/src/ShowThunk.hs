{-# LANGUAGE UnboxedTuples, MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ShowThunk (isThunk, other, showLazyList, showTipe, showLazyListIO, showTips) where

import GHC.Prim
import GHC.Exts
import qualified GHC.Exts.Heap as H
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

-- import RtClosureInspect	

ptr :: a -> Ptr b
ptr x = let !(# iptr, _, _ #) = unpackClosure# x in Ptr iptr

data StgInfoTable = StgInfoTable {
	ptrs :: Word32,
	nptrs :: Word32,
	tipe :: Word32,
	strlen :: Word32
--	code :: [Word8]
	} deriving Show

instance Storable StgInfoTable where
	sizeOf = undefined
	alignment = undefined
	poke = undefined
	peek a = StgInfoTable
		<$> peek a0
		<*> peek a1
		<*> peek a2
		<*> peek a3
		where [a0, a1, a2, a3] = map (castPtr . (a `plusPtr`)) [0, 4, 8, 16]

{-

AP = 25
THUNK = 16
THUNK_1_0 = 17
THUNK_0_1 = 18
THUNK_2_0 = 19
THUNK_1_1 = 20
THUNK_0_2 = 21
THUNK_STATIC = 22
THUNK_SELECTOR = 23

BCO = 24
FUN_STATIC = 15

-}

-- foo :: [Int]
-- foo = reverse [1, 2, 3]

showTipe :: a -> IO Word32
showTipe x = tipe <$> (peek $ ptr x)

isThunk :: a -> IO Bool
-- isThunk x = (`elem` [25, 16, 17, 18, 19, 20, 21, 22, 23]) . tipe <$> (peek $ ptr x)
isThunk x = (`elem` [25, 16, 17, 18, 19, 20, 21, 22, 23, 24, 15]) . tipe <$> (peek $ ptr x)

other :: [Int]
other = 1 : 2 : 3 : reverse [4, 5, 6]

showTips :: [a] -> IO ()
showTips xs = do
	showTipe xs >>= print
	case xs of
		[] -> putStrLn "end"
		_ : xs' -> showTips xs'

showLazyList :: Show a => [a] -> String
showLazyList xs = unsafePerformIO $ do
--	b <- isThunk xs
--	b <- not <$> isCon xs
--	b <- not <$> isFullyEvaluated xs
	b <- not <$> myEvaluated xs
--	print . H.tipe . H.info =<< H.getClosureData xs
--	showTips xs
	if b then return "?" else case xs of
		[] -> return "[]"
		x : xs' -> return $ show x ++ " : " ++ showLazyList xs'

myEvaluated :: a -> IO Bool
myEvaluated x = do
	t <- H.tipe . H.info <$> H.getClosureData x
	return $ case t of
		H.BLACKHOLE -> True
		H.CONSTR -> True
		H.CONSTR_1_0 -> True
		H.CONSTR_0_1 -> True
		H.CONSTR_2_0 -> True
		H.CONSTR_1_1 -> True
		H.CONSTR_0_2 -> True
		_ -> False

showLazyListIO :: Show a => [a] -> IO String
showLazyListIO xs = do
	b <- isThunk xs
	if b then return "?" else case xs of
		[] -> return "[]"
		x : xs' -> ((show x ++ " : ") ++) <$> showLazyListIO xs'

{-

CONSTR = 1
CONSTR_1_0 = 2
CONSTR_0_1 = 3
CONSTR_2_0 = 4
CONSTR_1_1 = 5
CONSTR_0_2 = 6
CONSTR_STATIC = 7
CONSTR_NOCAF_STATIC = 8

UNDERFLOW_FRAME = 38

-}

-- isCon :: a -> IO Bool
-- isCon x = (`elem` ([1 .. 8] ++ [38])) . tipe <$> (peek $ ptr x)
-- isCon x = (`elem` [1 .. 8]) . tipe <$> (peek $ ptr x)

-- a = seq a 1

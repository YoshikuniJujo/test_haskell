module Main where

import Data.Int
import Data.Word
import Lib

main :: IO ()
main = print (fromIntegral (123 :: Word8) :: Int8)

-- RULES
-- Class op show (BUILTIN)
-- fromIntegral/a->Int8 (GHC.Int)
-- fromIntegral/Word8->a (GHC.Word)
-- Class op fromInteger (BUILTIN)
-- integerToWord (BUILTIN)
-- narrow8Word# (BUILTIN)
-- fromIntegral/Word->Int (GHC.Real)
-- word2Int# (BUILTIN)
-- narrow8Int# (BUILTIN)
--
-- "fromIntegral/a->Int8" fromIntegral = \x -> case fromIntegral x of I# x# -> I8# (narrow8Int# x#)
-- "fromIntegral/Wrod8->a" fromIntegral = \(W8# x#) -> fromIntegral (W# x#)
-- "fromIntegral/Word->Int" fromIntegral = \(W# x#) -> I# (word2Int# x#)
--
-- fromIntegral :: Word8 -> Int8
-- fromIntegral (W8# w#)
-- 	=> case fromIntegral (W8# w#) of I# x# -> I8# (narrow8Int# x#)
-- 	=> case fromIntegral (W# w#) of I# x# -> I8# (narrow8Int# x#)
-- 	=> case I# (word2Int# w#) of I# x# -> I8# (narrow8Int# x#)
-- 	=> I8# (narrow8Int# (word2Int# w#))

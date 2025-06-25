{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.FingerTree qualified as BSF
import System.Exit

main :: IO ()
main = do
	when (BSF.toStrict (foldr (BSF.:<) "" ((fromIntegral . ord) <$> (take 1000000 (cycle "abcde") :: String))) /= BS.concat (replicate 200000 "abcde")) $ exitWith (ExitFailure 1)
	when (BSF.toStrict (foldr (BSF.:<|) "" (replicate 10000 "abcde")) /= BS.concat (replicate 10000 "abcde")) $ exitWith (ExitFailure 1)
	putStrLn "Test suite not yet implemented"



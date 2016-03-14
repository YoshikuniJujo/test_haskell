{-# LANGUAGE MonadComprehensions #-}

import Data.Char

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs

main :: IO ()
main = [ concatMap capitalize [s1, s2, s3] |
	s1 <- getLine,
	s2 <- getLine,
	s3 <- getLine ] >>= putStrLn

module Main (main) where

import Data.Time
import Data.FTCQueue

import Lib

main :: IO ()
-- main = foo `apply` ()
main = bar `apply` ()

foo, bar :: Q IO () ()
foo = singleton . const $ putStrLn "Hello, world!"

bar = singleton (const getLine) |> addTime |> putStrLn

addTime :: String -> IO String
addTime msg = do
	ct <- getCurrentTime
	pure $ msg ++ " (" ++ show ct ++ ")"

apply :: Monad t => Q t a b -> a -> t b
q `apply` x = case viewl q of
	One f -> f x
	f :| q' -> f x >>= (q' `apply`)

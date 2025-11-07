# try-ftcqueue

## See

https://okmij.org/ftp/Haskell/zseq.pdf

## Sample Code

```Haskell
module Main (main) where

import Data.Time
import Data.FTCQueue

import Lib

main :: IO ()
main = foo `apply` ()

foo :: Q IO () ()
foo = singleton (const getLine) |> addTime |> putStrLn

addTime :: String -> IO String
addTime msg = do
	ct <- getCurrentTime
	pure $ msg ++ " (" ++ show ct ++ ")"

apply :: Monad t => Q t a b -> a -> t b
q `apply` x = case viewl q of
	One f -> f x
	f :| q' -> f x >>= (q' `apply`)
```

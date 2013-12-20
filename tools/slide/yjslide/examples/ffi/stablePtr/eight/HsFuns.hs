module HsFun where

import Foreign.StablePtr

foreign export ccall "hsfun" fun :: IO ()
foreign export ccall "eight" eight :: IO (StablePtr Int)
foreign export ccall "printSP" printSP :: StablePtr Int -> IO ()

fun :: IO ()
fun = putStrLn "Hoge"

eight :: IO (StablePtr Int)
eight = newStablePtr 8

printSP :: StablePtr Int -> IO ()
printSP p = deRefStablePtr p >>= print

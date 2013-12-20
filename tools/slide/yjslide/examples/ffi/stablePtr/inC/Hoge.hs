module Hoge where

import Foreign.StablePtr

foreign export ccall "hoge" hoge :: Int -> Int
foreign export ccall "tarou" tarou :: IO (StablePtr (String, Int))
foreign export ccall "print_tarou" printTarou :: StablePtr (String, Int) -> IO ()

hoge = (+ 3)

tarou :: IO (StablePtr (String, Int))
tarou = newStablePtr ("Tarou", 33)

printTarou :: StablePtr (String, Int) -> IO ()
printTarou p = deRefStablePtr p >>= print

{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Int

foreign import ccall "add3" c_add3 :: #{type int} -> #{type int}

main :: IO ()
main = do
	putStrLn "Hello, world!"
	print $ c_add3 8

#def int add3(int i) { return i + 3; }

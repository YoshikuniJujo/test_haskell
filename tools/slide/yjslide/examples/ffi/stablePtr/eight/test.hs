import Foreign.C.Types

foreign import ccall "fun1" c_fun1 :: CInt -> CInt
foreign import ccall "useHSFun" c_useHSFun :: IO ()
foreign import ccall "printEight" c_printEight :: IO ()

main :: IO ()
main = do
	putStrLn "Hello"
	print $ c_fun1 88
	c_useHSFun
	c_printEight

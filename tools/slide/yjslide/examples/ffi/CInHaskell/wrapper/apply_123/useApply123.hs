import Foreign.Ptr

foreign import ccall "wrapper" wrap :: (Int -> Int) -> IO (FunPtr (Int -> Int))
foreign import ccall "apply_123.h print_apply_123" c_printApply123 ::
	FunPtr (Int -> Int) -> IO ()

main :: IO ()
main = do
	c_printApply123 =<< wrap (\x -> x * x)

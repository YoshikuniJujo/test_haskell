import Foreign.Ptr

foreign import ccall "wrapper" wrap :: (IO ()) -> IO (FunPtr (IO ()))
foreign import ccall "three_times.h three_times" c_threeTimes ::
	FunPtr (IO ()) -> IO ()

main :: IO ()
main = do
	wrap (putStrLn "Hello") >>= c_threeTimes

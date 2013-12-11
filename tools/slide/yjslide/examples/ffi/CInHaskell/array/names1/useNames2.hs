import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad

foreign import ccall "names.h get_size" c_getSize :: IO CInt
foreign import ccall "names.h get_names" c_getNames :: IO (Ptr CString)

getSize :: IO Int
getSize = do
	ci <- c_getSize
	case safeConvertIntegral ci of
		Just i -> return i
		_ -> error "conversion error"

main :: IO ()
main = do
	len <- getSize
	pns <- c_getNames
	forM_ [0 .. len - 1] $ \i -> do
		cs <- peekElemOff pns i
		peekCString cs >>= putStrLn

safeConvertIntegral :: (Integral a, Integral b, Bounded b) => a -> Maybe b
safeConvertIntegral x = let
	r = fromIntegral x
	mx = fromIntegral $ maxBound `asTypeOf` r
	mn = fromIntegral $ minBound `asTypeOf` r in
	if x <= mx && x >= mn then Just r else Nothing

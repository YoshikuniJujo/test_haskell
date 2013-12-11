import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Storable

foreign import ccall "names.h get_size" c_getSize :: IO CInt
foreign import ccall "names.h get_names" c_getNames :: IO (Ptr CString)

getSize :: IO Int
getSize = do
	ci <- c_getSize
	case safeConvertIntegral ci of
		Just i -> return i
		_ -> error "conversion error"

getNames :: IO [String]
getNames = do
	i <- getSize
	c_getNames >>= myPeekArray i >>= mapM peekCString

main :: IO ()
main = getNames >>= mapM_ putStrLn

myPeekArray :: Storable a => Int -> Ptr a -> IO [a]
myPeekArray 0 _ = return []
myPeekArray n p = do
	e <- peekElemOff p (n - 1)
	es <- myPeekArray (n - 1) p
	return $ e : es

safeConvertIntegral :: (Integral a, Integral b, Bounded b) => a -> Maybe b
safeConvertIntegral x = let
	r = fromIntegral x
	mx = fromIntegral $ maxBound `asTypeOf` r
	mn = fromIntegral $ minBound `asTypeOf` r in
	if x <= mx && x >= mn then Just r else Nothing

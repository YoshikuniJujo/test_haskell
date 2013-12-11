import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "counter" c_counter :: IO (Ptr CInt)

safeConvertIntegral :: (Integral a, Integral b, Bounded b) => a -> Maybe b
safeConvertIntegral x = let
	r = fromIntegral x
	mx = fromIntegral $ maxBound `asTypeOf` r
	mn = fromIntegral $ minBound `asTypeOf` r in
	if x <= mx && x >= mn then Just r else Nothing

counter :: IO Int
counter = do
	pci <- c_counter
	ci <- peek pci
	case safeConvertIntegral ci of
		Just i -> return i
		_ -> error "conversion (CInt -> Int) error"

main :: IO ()
main = do
	_ <- counter
	x <- counter
	_ <- counter
	y <- counter
	print x
	print y

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

foreign import ccall "message.h sellect_message" c_sellectMessage :: Ptr CInt -> IO ()
foreign import ccall "message.h message" c_message :: IO ()

message :: Int -> IO ()
message i = case safeConvertIntegral i of
	Just ci -> alloca $ \pci -> do
			poke pci ci
			c_sellectMessage pci
			c_message
	_ -> error "conversion error"

main :: IO ()
main = message 2

safeConvertIntegral :: (Integral a, Integral b, Bounded b) => a -> Maybe b
safeConvertIntegral x = let
	r = fromIntegral x
	mx = fromIntegral $ maxBound `asTypeOf` r
	mn = fromIntegral $ minBound `asTypeOf` r in
	if x <= mx && x >= mn then Just r else Nothing

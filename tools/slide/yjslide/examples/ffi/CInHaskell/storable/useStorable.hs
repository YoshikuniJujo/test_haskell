import Foreign.C.Types
import Foreign.Ptr
import Storable
import Foreign.Storable
import Foreign.Marshal

foreign import ccall "printMaybeDouble" c_printMaybeDouble ::
	Ptr (Maybe CDouble) -> IO ()

foreign import ccall "printEitherShortDouble" c_printEitherShortDouble ::
	Ptr (Either CShort CDouble) -> IO ()

foreign import ccall "printShape" c_printShape :: Ptr Shape -> IO ()

main :: IO ()
main = do
	alloca $ \ptr -> do
		poke ptr (Just 8.0 :: Maybe CDouble)
		c_printMaybeDouble ptr
		poke ptr (Nothing :: Maybe CDouble)
		c_printMaybeDouble ptr
	alloca $ \ptr -> do
		poke ptr (Right 9.8 :: Either CShort CDouble)
		c_printEitherShortDouble ptr
		poke ptr (Left 842 :: Either CShort CDouble)
		c_printEitherShortDouble ptr
	alloca $ \ptr -> do
		poke ptr $ Circle 82.5 
		c_printShape ptr
		poke ptr $ Rectangle 33 89
		c_printShape ptr

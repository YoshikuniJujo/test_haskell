import Foreign.C.Types
import Foreign.C.String

foreign import ccall "sayHelloTo" c_sayHelloTo :: CString -> IO ()

main :: IO ()
main = withCString "Yoshikuni" $ \cstr -> do
	c_sayHelloTo cstr
{-
main = do
	cstr <- newCString "Yoshikuni"
	c_sayHelloTo cstr
	-}

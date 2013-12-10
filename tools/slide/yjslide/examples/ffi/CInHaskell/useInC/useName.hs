import Foreign.C.String
import Foreign.Ptr
import Control.Applicative
import Control.Exception

newtype Name = Name (Ptr Name)

foreign import ccall "mkName" c_mkName :: CString -> CString -> IO (Ptr Name)
foreign import ccall "printName" c_printName :: Ptr Name -> IO ()
foreign import ccall "freeName" c_freeName :: Ptr Name -> IO ()

mkName :: String -> String -> IO Name
mkName fn ln = withCString fn $ \cfn -> withCString ln $ \cln ->
	Name <$> c_mkName cfn cln

printName, freeName :: Name -> IO ()
printName (Name pn) = c_printName pn
freeName (Name pn) = c_freeName pn

main :: IO ()
{-
main = do
	n <- mkName "Yoshikuni" "Jujo"
	printName n
	freeName n
	-}
main = do
	bracket (mkName "Yoshikuni" "Jujo") freeName printName
	bracket (mkName "YoshikuniTarouYoshikuni" "Jujo") freeName printName

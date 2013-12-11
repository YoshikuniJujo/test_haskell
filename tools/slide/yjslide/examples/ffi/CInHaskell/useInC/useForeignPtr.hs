import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Applicative
import Control.Exception

data Name = Name (ForeignPtr Name)

foreign import ccall "name.h mkName" c_mkName :: CString -> CString -> IO (Ptr Name)
foreign import ccall "name.h printName" c_printName :: Ptr Name -> IO ()
foreign import ccall "name.h &freeName" c_freeName :: FinalizerPtr Name

mkName :: String -> String -> IO Name
mkName fn ln = withCString fn $ \cfn -> withCString ln $ \cln ->
	Name <$> (c_mkName cfn cln >>= newForeignPtr c_freeName)

printName :: Name -> IO ()
printName (Name pn) = withForeignPtr pn c_printName

main :: IO ()
main = do
	n <- mkName "Yoshikuni" "Jujo"
	printName n

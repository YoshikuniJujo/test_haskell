{-# LANGUAGE ForeignFunctionInterface, ExistentialQuantification, DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "../c/script.h run_script" c_run_script :: CString -> IO CInt

data SomeMyException = forall e . Exception e => SomeMyException e
	deriving Typeable

instance Show SomeMyException where
	show (SomeMyException e) = show e

instance Exception SomeMyException

myExceptionToException :: Exception e => e -> SomeException
myExceptionToException = toException . SomeMyException

myExceptionFromException :: Exception e => SomeException -> Maybe e
myExceptionFromException x = do
	SomeMyException a <- fromException x
	cast a

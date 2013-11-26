import Control.Monad
import Control.Exception
import System.IO.Error
import GHC.IO.Exception

{-
getNEInfo :: (IOError -> a) -> IOError -> Maybe a
getNEInfo info e = do
	guard $ isDoesNotExistError e
	return $ info e
	-}

getNEInfo :: (IOError -> a) -> IO (Either a String)
getNEInfo info = tryJust
	(\e -> guard (isDoesNotExistError e) >> return (info e)) $
	readFile "notExist.txt"

getPEInfo :: (IOError -> a) -> IO (Either a String)
getPEInfo info = tryJust
	(\e -> guard (isPermissionError e) >> return (info e)) $
	readFile "/etc/shadow"

getAIUInfo :: (IOError -> a) -> IO (Either a String)
getAIUInfo info = tryJust
	(\e -> guard (isAlreadyInUseError e) >> return (info e)) $
	readFile "test.txt"

isInvalidArgument :: IOError -> Bool
isInvalidArgument IOError { ioe_type = InvalidArgument } = True
isInvalidArgument _ = False

getIAInfo :: (IOError -> a) -> IO (Either a String)
getIAInfo info = tryJust
	(\e -> guard (isInvalidArgument e) >> return (info e)) $
	readFile "bad.txt" >>= evaluate

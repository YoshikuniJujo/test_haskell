import Control.Monad
import Control.Exception
import System.IO.Error

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

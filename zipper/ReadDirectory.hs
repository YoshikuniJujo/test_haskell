module ReadDirectory (
	FSZipper,
	Name,
	getDir,
	emptyDir,
	fsUp, fsTo, fsRename, fsNewFile,
	fsGetDirContents, fsGetContents
) where

import FileSystem

import System.Directory
import System.FilePath
import Control.Applicative

emptyDir :: Name -> FSItem
emptyDir = flip Folder []

getDir :: Name -> IO FSItem
getDir fp = do
	fe <- doesFileExist fp
	de <- doesDirectoryExist fp
	case (fe, de) of
		(True, False) -> do
			cnt <- readFile fp
			return $ File (last $ splitPath fp) cnt
		(False, True) -> Folder (last $ splitPath fp) <$>
			(mapM (getDir . ((fp ++ "/") ++))
				. filter (`notElem` [".", ".."])
				=<< getDirectoryContents fp)
		_ -> error "bad"

-- writeDir :: FSItem -> IO ()

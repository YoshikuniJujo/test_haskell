import Control.Applicative
import Data.List
import System.Directory
import System.Environment
import System.Console.GetOpt

main :: IO ()
main = do
	args <- getArgs
	either putStrLn (uncurry copy) $
		option args >>= getIOFiles

data IOFile = Infile FilePath | Outfile FilePath
	deriving Show

isInfile, isOutfile :: IOFile -> Bool
isInfile (Infile _) = True; isInfile _ = False
isOutfile (Outfile _) = True; isOutfile _ = False

copy :: IOFile -> IOFile -> IO ()
copy (Infile i) (Outfile o) = copyFile i o
copy _ _ = error "You must copy from Infile to Outfile."

option :: [String] -> Either String [IOFile]
option args = case getOpt RequireOrder [
		Option ['i'] ["infile"]
			(ReqArg Infile "file path")
			"input file",
		Option ['o'] ["outfile"]
			(ReqArg Outfile "file path")
			"output file" ]
	args of
	(ss, [], []) -> Right ss
	(_, as, []) -> Left $
		"Naked arguments: " ++ unwords as
	(_, _, es) -> Left $ concat es

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither msg _ = Left msg

getIOFiles :: [IOFile] -> Either String (IOFile, IOFile)
getIOFiles iofs = (,)
	<$> maybeToEither
		"No input files" (find isInfile iofs)
	<*> maybeToEither
		"No output files " (find isOutfile iofs)

import Data.List
import Data.Char
import System.IO
import System.Directory
import System.Environment
import System.Cmd

main :: IO ()
main = do
	args <- getArgs
	let oc = case args of
		["only-count"] -> True
		_ -> False
	getDirectoryContents "." >>= mapM_ (showPages oc) . sort . filter isLecture

showPages :: Bool -> String -> IO ()
showPages oc fn = do
	if oc then return () else do
		putStr $ getNumber fn ++ ": "
		hFlush stdout
	system $ mkCmd fn
	return ()

mkCmd :: String -> String
mkCmd fn = "runhaskell -Wall " ++ fn ++ " count-pages"

isLecture :: String -> Bool
isLecture fn = case splitFileName fn of
	("lecture", n, ".hs") -> all isDigit n
	_ -> False

getNumber :: String -> String
getNumber fn = let (_, n, _) = splitFileName fn in n

splitFileName :: String -> (String, String, String)
splitFileName fn = (l, n, s)
	where
	(l, r) = splitAt 7 fn
	(n, s) = splitAt 2 r

module Main where

import System.Environment

main :: IO ()
main = do
	fn : _ <- getArgs
	cnt <- readFile fn
	print . (readImportPre <$>) $ lines cnt
	writeFile (fn ++ "_new") . unlines . (trLine <$>) $ lines cnt

data QualifiedImport = QualifiedImport String String deriving Show

trLine :: String -> String
trLine src = maybe src showImportPost $ readImportPre src

readImportPre :: String -> Maybe QualifiedImport
readImportPre src = case words src of
	"import" : "qualified" : foo : "as" : foos ->
		Just $ QualifiedImport foo (unwords foos)
	_ -> Nothing

showImportPost :: QualifiedImport -> String
showImportPost (QualifiedImport nm psa) =
	unwords ["import", nm, "qualified as", psa]

import Control.Applicative
import Data.List
import Data.Char
import System.Environment

main :: IO ()
main = do
	fp : m : _ <- getArgs
	cnt <- readFile fp
	let	consts = map (takeWhile ((||) <$> isUpper <*> (== '_'))
				.  head . tail)
			. filter ((== "#define") . head)
			. filter (not . null) . map words $ lines cnt
		vars = map (heading toLower) consts
		tdec = intercalate ", " vars ++ " :: CInt"
		dec = zipWith mkDec vars consts
	writeFile (m ++ ".hsc") $
		"#include " ++ show fp ++ "\n\n" ++
		"module " ++ m ++ " (" ++ intercalate ", " vars ++ ") where\n\n" ++
		"import Foreign.C.Types\n\n" ++
		tdec ++ "\n" ++ unlines dec

heading :: (a -> a) -> [a] -> [a]
heading f (x : xs) = f x : xs
heading _ _ = error "empty list"

mkDec :: String -> String -> String
mkDec v c = v ++ " = #const " ++ c

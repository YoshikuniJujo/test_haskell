import Control.Applicative
import Data.Maybe

main :: IO ()
main = do
	t <- readFile "templates/bsd3.txt"
	{-
	mapM_ putStrLn . take 10 . lines .
		(\(Right s) -> s) $ transpose [
			("YEAR", "2015"),
			("OWNER", "Yoshikuni Jujo") ] t
			-}
	print $ transpose [("YEAR", "2015"), ("OWNER", "Yoshikuni")] t

transpose ::
	[(String, String)] -> String -> Either String String
transpose _ "" = return ""
transpose tbl ('<' : cs) = case span (/= '>') cs of
	(k, '>' : r) -> maybe
		(fail $ "No such key: " ++ k)
		((<$> transpose tbl r) . (++)) $ lookup k tbl
	_ -> fail "There are not '>'"
transpose tbl (c : cs) = (c :) <$> transpose tbl cs

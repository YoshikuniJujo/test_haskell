main :: IO ()
main = interact $ unlines . map (writeHtml . readBBCode1) . lines

data BBCode = BBCode String String String deriving Show

readBBCode1 :: String -> BBCode
readBBCode1 ('[' : s) = case span (/= ']') s of
	(t, ']' : r) -> case span (/= '[') r of
		(txt, '[' : r') -> let
			(tg, v) = readTag t in
			BBCode tg v txt
		_ -> error "bad"
	_ -> error "bad"

readTag :: String -> (String, String)
readTag s = case span (/= '=') s of
	(tg, '=' : v) -> (tg, v)
	_ -> (s, "")

writeHtml :: BBCode -> String
writeHtml (BBCode "b" "" txt) = "<strong>" ++ txt ++ "<strong>"
writeHtml (BBCode "i" "" txt) = "<em>" ++ txt ++ "<em>"
writeHtml (BBCode "u" "" txt) = "<span class=u>" ++ txt ++ "<span>"
writeHtml (BBCode "url" url txt) =
	"<a href=\"" ++ url ++ "\">" ++ txt ++ "</a>"
writeHtml (BBCode "img" url txt) =
	"<img src=\"" ++ url ++ "\" alt=\"" ++ txt ++ "\">"

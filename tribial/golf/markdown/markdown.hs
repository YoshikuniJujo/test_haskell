import System.IO.Unsafe
import Data.Char

main :: IO ()
main = interact $ showHtml . processP . readMarkdown . lines

test1, test2, test3, test4 :: [String]
test1 = lines . unsafePerformIO $ readFile "test1.md"
test2 = lines . unsafePerformIO $ readFile "test2.md"
test3 = lines . unsafePerformIO $ readFile "test3.md"
test4 = lines . unsafePerformIO $ readFile "test4.md"

data Markdown
	= H1 String
	| H2 String
	| P [Markdown]
	| Raw String
	| Em String
	| Strong String
	| Ul [Markdown]
	| Li Markdown
	| Ol [Markdown]
	| OLi String
	| Hr
	| Url String String
	| Image String String
	deriving Show

readMarkdown :: [String] -> [Markdown]
readMarkdown [] = []
readMarkdown ("" : t) = readMarkdown t
readMarkdown (('#' : ' ' : s) : t) = H1 (head $ words s) : readMarkdown t
readMarkdown (('*' : '*' : s) : t) = Strong (init $ init s) : readMarkdown t
readMarkdown (('*' : s) : t) = Em (init s) : readMarkdown t
readMarkdown ((' ' : '-' : s) : t) =
	Li (head $ readMarkdown [dropWhile (== ' ') s]) : readMarkdown t
readMarkdown (('-' : _) : t) = Hr : readMarkdown t
readMarkdown (s@(i : _) : t) | isDigit i =
	OLi (dropWhile isSpace . tail $ dropWhile (/= '.') s) : readMarkdown t
readMarkdown (h : ('-' : _) : t) = H2 h : readMarkdown t
readMarkdown (('[' : s) : t) =
	let (txt, url) = readUrls s in Url txt url : readMarkdown t
readMarkdown (('!' : '[' : s) : t) =
	let (txt, url) = readUrls s in Image txt url : readMarkdown t
readMarkdown (w : t) = Raw w : readMarkdown t

readUrls :: String -> (String, String)
readUrls s = case span (/= ']') s of
	(txt, ']' : '(' : url) -> (txt, init url)
	_ -> error "bad"

isRaw, isLi, isOLi :: Markdown -> Bool
isRaw (Raw _) = True
isRaw (Strong _) = True
isRaw (Em _) = True
isRaw (Url _ _) = True
isRaw (Image _ _) = True
isRaw _ = False
isLi (Li _) = True
isLi _ = False
isOLi (OLi _) = True
isOLi _ = False

processP :: [Markdown] -> [Markdown]
processP [] = []
processP (h1@(H1 _) : ms) = h1 : processP ms
processP (h2@(H2 _) : ms) = h2 : processP ms
processP (Hr : ms) = Hr : processP ms
processP ms@(Li _ : _) = let (rs, ms') = span isLi ms in Ul rs : processP ms'
processP ms@(OLi _ : _) = let (rs, ms') = span isOLi ms in Ol rs : processP ms'
processP ms = let (rs, ms') = span isRaw ms in P rs : processP ms'

showHtml :: [Markdown] -> String
showHtml [] = ""
showHtml (H1 h1 : md) = "<h1>" ++ h1 ++ "</h1>" ++ showHtml md
showHtml (H2 h2 : md) = "<h2>" ++ h2 ++ "</h2>" ++ showHtml md
showHtml (P md : mds) = "<p>" ++ showHtml md ++ "</p>" ++ showHtml mds
showHtml (Strong st : md) = "<strong>" ++ st ++ "</strong>" ++ showHtml md
showHtml (Em em : md) = "<em>" ++ em ++ "</em>" ++ showHtml md
showHtml (Ul md : mds) = "<ul>" ++ showHtml md ++ "</ul>" ++ showHtml mds
showHtml (Li md : mds) = "<li>" ++ showHtml [md] ++ "</li>" ++ showHtml mds
showHtml (Ol md : mds) = "<ol>" ++ showHtml md ++ "</ol>" ++ showHtml mds
showHtml (OLi md : mds) = "<li>" ++ md ++ "</li>" ++ showHtml mds
showHtml (Hr : mds) = "<hr>" ++ showHtml mds
showHtml (Raw s : mds) = s ++ showHtml mds
showHtml (Url txt url : mds) =
	"<a href=\"" ++ url ++ "\">" ++ txt ++ "</a><br>" ++ showHtml mds
showHtml (Image txt url : mds) =
	"<img src=\"" ++ url ++ "\" alt=\"" ++ txt ++ "\">" ++ showHtml mds

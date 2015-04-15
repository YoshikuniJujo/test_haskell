import Data.List

main :: IO ()
main = interact $ unlines
	. (\(w : ls) -> cutoutBT w . cutoutTB w $ map (cutoutRL w . cutoutLR w) ls)
	. lines

cutoutLR, cutoutRL :: String -> String -> String
cutoutLR _ "" = ""
cutoutLR w str@(c : cs)
	| w `isPrefixOf` str = map (const ' ') w ++ drop (length w) str
	| otherwise = c : cutoutLR w cs
cutoutRL w = reverse . cutoutLR w . reverse

cutoutTB, cutoutBT :: String -> [String] -> [String]
cutoutTB w = transpose . map (cutoutLR w) . transpose
cutoutBT w = transpose . map (cutoutRL w) . transpose

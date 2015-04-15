import Control.Applicative
import Data.Maybe
import Data.List

main :: IO ()
main = interact $ unlines . winning . lines

sample :: [String]
sample = [
	"X_O",
	"XOO",
	"___" ]

winning :: [String] -> [String]
winning brd = head $ mapMaybe ($ brd)
	[winningHr, winningVt, winningDg, winningDg2, Just]

winningHr :: [String] -> Maybe [String]
winningHr [] = Nothing
winningHr (ln : lns) =
	((ln :) <$> winningHr lns) `maybe` (Just . (: lns)) $ winningHr1 ln

winningHr1 :: String -> Maybe String
winningHr1 ln
	| ("XX", "_") == partition (== 'X') ln = Just "XXX"
	| otherwise = Nothing

winningVt :: [String] -> Maybe [String]
winningVt [[], [], []] = Nothing
winningVt [a : as, b : bs, c : cs]
	| ("XX", "_") ==
		partition (== 'X') [a, b, c] = Just $ map ('X' :) [as, bs, cs]
	| otherwise = zipWith (:) [a, b, c] <$> winningVt [as, bs, cs]

winningDg, winningDg2 :: [String] -> Maybe [String]
winningDg [a : as, b0 : b : bs, c0 : c1 : [c]]
	| ("XX", "_") == partition (== 'X') [a, b, c] =
		Just ['X' : as, b0 : 'X' : bs, c0 : c1 : "X"]
	| otherwise = Nothing
winningDg2 = (reverse <$>) . winningDg . reverse

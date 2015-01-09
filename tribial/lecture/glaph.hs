import System.Environment
import Control.Applicative

main :: IO ()
main = do
	fp : _ <- getArgs
	ns <- map read . lines <$> readFile fp
	putStr $ mkGlaph ns

mkGlaph :: [Int] -> String
mkGlaph ns = unlines $ map (flip replicate '*') ns

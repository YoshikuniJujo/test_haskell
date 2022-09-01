{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sample.GetOpt where

import System.Environment
import System.Console.GetOpt

getOptions :: IO (Maybe BufMem)
getOptions = do
	args <- getArgs
	let	(opts, noopts, emsgs) = getOpt RequireOrder options args
	case (emsgs, noopts) of
		([], []) -> do
			let	opt' = processOptions opts
			print opt'
			pure $ Just opt'
		_ -> do	putStrLn `mapM_` emsgs
			putStrLn "Unsuitable args:"
			putStrLn `mapM_` noopts
			pure Nothing

options :: [OptDescr Option]
options = [
	Option ['b'] ["buffer"] (ReqArg
			(\case "1" -> Buffer1; "3" -> Buffer3; _ -> Nonsense)
			"Number of Buffers")
		"Set Number of Buffers",
	Option ['m'] ["memory"] (ReqArg
			(\case "1" -> Memory1; "3" -> Buffer3; _ -> Nonsense)
			"Number of Memories")
		"Set Number of Memories" ]

data Option = Buffer1 | Buffer3 | Memory1 | Memory3 | Nonsense deriving (Show, Eq, Ord)
data BufMem = Buffer1Memory1 | Buffer3Memory1 | Buffer3Memory3 deriving Show

processOptions :: [Option] -> BufMem
processOptions opts = case (b1, m1) of
	(False, False) -> Buffer3Memory3
	(False, True) -> Buffer3Memory1
	(True, True) -> Buffer1Memory1
	_ -> Buffer3Memory3
	where b1 = Buffer1 `elem` opts; m1 = Memory1 `elem` opts

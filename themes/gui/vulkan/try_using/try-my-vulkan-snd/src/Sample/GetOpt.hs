{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sample.GetOpt where

import System.Environment
import System.Console.GetOpt

getOptions :: IO (Maybe Opts)
getOptions = do
	args <- getArgs
	let	(opts, noopts, emsgs) = getOpt RequireOrder options args
	case (emsgs, noopts, getImagePath opts, getTiling opts) of
		([], [], Just fp, Just tlng) -> do
			let	bm = getBufMem opts
			print bm
			print fp
			pure . Just $ Opts bm fp tlng
		(_, _, Just _, _) -> do
			putStrLn $ usageInfo "Usage:" options
			putStrLn `mapM_` emsgs
			putStrLn "Unsuitable args:"
			putStrLn `mapM_` noopts
			pure Nothing
		(_, _, Nothing, _) -> do
			putStrLn "Set Image file path"
			putStrLn $ usageInfo "Usage:" options
			pure Nothing

options :: [OptDescr Option]
options = [
	Option ['b'] ["buffer"] (ReqArg
			(\case "1" -> Buffer1; "3" -> Buffer3; _ -> Nonsense)
			"[Number of Buffers]")
		"Set Number of Buffers",
	Option ['m'] ["memory"] (ReqArg
			(\case "1" -> Memory1; "3" -> Buffer3; _ -> Nonsense)
			"[Number of Memories]")
		"Set Number of Memories",
	Option ['i'] ["image-file"] (ReqArg ImagePath "[Image file path]")
		"Set Image file path",
	Option ['t'] ["tiling"] (ReqArg tiling "[optimal or linear]")
		"Set Tiling" ]

data Option =
	Buffer1 | Buffer3 | Memory1 | Memory3 | ImagePath FilePath | OptionTiling Tiling | Nonsense
	deriving (Show, Eq, Ord)

data Opts = Opts { optsBuffMem :: OptBffMm, optsImagePath :: FilePath, optsTiling :: Tiling }
	deriving Show

data OptBffMm = Buffer1Memory1 | Buffer3Memory1 | Buffer3Memory3 deriving Show

data Tiling = Optimal | Linear deriving (Show, Eq, Ord)

tiling :: String -> Option
tiling "optimal" = OptionTiling Optimal
tiling "linear" = OptionTiling Linear
tiling _ = Nonsense

getBufMem :: [Option] -> OptBffMm
getBufMem opts = case (b1, m1) of
	(False, False) -> Buffer3Memory3
	(False, True) -> Buffer3Memory1
	(True, True) -> Buffer1Memory1
	_ -> Buffer3Memory3
	where b1 = Buffer1 `elem` opts; m1 = Memory1 `elem` opts

getImagePath :: [Option] -> Maybe FilePath
getImagePath [] = Nothing
getImagePath (ImagePath fp : _) = Just fp
getImagePath (_ : os) = getImagePath os

getTiling :: [Option] -> Maybe Tiling
getTiling [] = Nothing
getTiling (OptionTiling t : _) = Just t
getTiling (_ : os) = getTiling os

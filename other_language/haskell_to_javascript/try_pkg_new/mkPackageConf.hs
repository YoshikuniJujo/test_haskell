{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Maybe
import System.Environment

import Hason
import Hason.Eval
import MkPackageConf.RealMain

main :: IO ()
main = do
	[pkgfp] <- getArgs
	hsn <- either error id . eval <$> readFile pkgfp
	realMain . fromJust $ hasonToMetaData hsn

type Name = String
type Vsn = String
type Module = String

hasonToMetaData :: Hason -> Maybe MetaData
hasonToMetaData h = do
	nm <- toName =<< lookup (KStr "name") h
	vsn <- toVersion =<< lookup (KStr "version") h
	ems <- toModules =<< lookup (KStr "exposed-modules") h
	dps <- toNameVsns =<< lookup (KStr "depends") h
	pure $ MetaData nm vsn ems dps

toName :: HasonValue -> Maybe Name
toName = \case Str nm -> Just nm; _ -> Nothing

toVersion :: HasonValue -> Maybe Vsn
toVersion = \case Str vsn -> Just vsn; _ -> Nothing

toModules :: HasonValue -> Maybe [Module]
toModules = \case
	Seq ms -> (\case Str mn -> Just mn; _ -> Nothing) `mapM` ms
	_ -> Nothing

toNameVsns :: HasonValue -> Maybe [(Name, Vsn)]
toNameVsns = \case
	Seq ds -> mapM toNameVsn ds
	_ -> Nothing

toNameVsn :: HasonValue -> Maybe (Name, Vsn)
toNameVsn = \case
	Dct p -> do
		Str n <- lookup (KStr "name") p
		Str v <- lookup (KStr "version") p
		pure (n, v)
	_ -> Nothing

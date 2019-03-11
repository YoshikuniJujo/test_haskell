{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.IO.Class
import Data.Foldable
import System.Environment
import GmoPg hiding (Amount)

import Lib
import Parts
import Tools

main :: IO ()
main = do
	[t] <- getArgs
	ct <- getZonedTimeOr $ case t of "now" -> Nothing; _ -> Just $ read t
	env <- getGmoPgEnv
	withConnection $ \conn -> do
		updateNeighborType conn ct
		mfs <- zip [3 ..] <$> getMonthlyFee conn
		for_ mfs $ \(pid, (nid, ntid, mf)) ->
			addMonthlyFee conn ct (Just pid) nid ntid mf
		as <- getAmount conn
		rslt <- (`runGmoPgM` env) . for_ as $
			\(nid, amnt) -> do
				liftIO $ do
					print nid
					print amnt
				settle Monthly conn ct nid amnt
		either print return rslt

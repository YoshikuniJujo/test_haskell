{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.DrawDownloadImage where

import Control.Monad as M
import Control.Monad.ST
import Data.String
import Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as V
import Codec.Picture
import System.Random
import Network.HTTP.Simple

import Field

import Check.DownloadImage
import Check.Field.DrawImage

drawDownloadImage :: Field -> Request -> IO ()
drawDownloadImage f rq = downloadImage rq >>= \case
	Left em -> error em
	Right img -> do
		let	w = fromIntegral $ imageWidth img
			h = fromIntegral $ imageHeight img
			dt = V.modify swap02s $ imageData img
		V.unsafeWith (V.unsafeCast dt) \d -> do
			i <- createImageSimple f d w h
			putImage f i 100 100 w h

swap02s :: V.Storable a => V.MVector s a -> ST s ()
swap02s v = M.zipWithM_ (MV.swap v) [0, 4 .. MV.length v] [2, 6 .. MV.length v]

githubAvatar :: Int -> Request
githubAvatar n = fromString $ "https://avatars1.githubusercontent.com/u/" ++ show n ++ "?v=4"

randomAvatar :: IO Request
randomAvatar = githubAvatar <$> randomRIO (1, 500)

sample :: IO ()
sample = do
	f <- openField "foobar" []
	drawDownloadImage f =<< randomAvatar
	flushField f
	void getLine
	closeField f

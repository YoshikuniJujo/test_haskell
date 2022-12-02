{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time
import Data.Hason
import Data.CairoContext
import Data.Color
import System.Environment
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Cairo
import Text

translate' :: (Int, NominalDiffTime) -> (CDouble, CDouble)
translate' = tr . (fromIntegral *** realToFrac)
	where tr (x, y) = (transX x, transY y)

transX, transY :: CDouble -> CDouble
transX x = log x * 150 + 120
transY y = 768 - (y - 0.110) * 5000

readDict :: String -> M.Map T.Text Hason
readDict s = case read s of
	Dct d -> M.fromList d
	_ -> error "not dictionary"

readResult :: M.Map T.Text Hason -> Result1
readResult m = Result1 {
	machineId = toText $ m M.! "machine-id",
	listSize = fromIntegral . toI $ m M.! "N",
	result = readResultResult $ m M.! "result"
	}

readResultResult :: Hason -> [(Int, NominalDiffTime)]
readResultResult = \case
	L l -> dictToTime1 <$> l
	_ -> error "not list"

dictToTime1 :: Hason -> (Int, NominalDiffTime)
dictToTime1 = \case
	Dct d -> case (lookup "M" d, lookup "time" d) of
		(Just (I m), Just (DT t)) -> (fromIntegral m, t)
		_ -> error "no `M' or `time'"
	_ -> error "not dict"

toText :: Hason -> T.Text
toText = \case T t -> t; _ -> error "not text"

toI :: Hason -> Integer
toI = \case I i -> i; _ -> error "not integer"

data Result1 = Result1 {
	machineId :: T.Text,
	listSize :: Int,
	result :: [(Int, NominalDiffTime)] }
	deriving Show

filterMachine :: [Result1] -> IO [Result1]
filterMachine rs = do
	mid <- (T.pack . init) <$> readFile "/etc/machine-id"
	pure $ filter ((== mid) . machineId) rs

drawResult1 :: CairoT s RealWorld -> Result1 -> IO ()
drawResult1 cr rslt = graph cr $ translate' <$> result rslt

main :: IO ()
main = withCairo "quicksort-m.png" 1024 768 \cr -> do
	as <- getArgs

	cairoSetLineWidth cr 0.5
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
	cairoMoveTo cr (transX 9) 200
	cairoLineTo cr (transX 9) 700
	cairoStroke cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoMoveTo cr (transX 1) 715
	cairoLineTo cr (transX 256) 715
	cairoStroke cr
	for_ [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024] \i -> do
		cairoMoveTo cr (transX i) 715
		cairoLineTo cr (transX i) 700
		cairoStroke cr
		putText cr (Size 10) (transX i - 5) 730
			. T.pack . show @Int $ round i
	cairoMoveTo cr 60 (transY 0.15)
	cairoLineTo cr 60 (transY 0.25)
	cairoStroke cr
	for_ [0.15, 0.2, 0.25] \s -> do
		cairoMoveTo cr 60 (transY s)
		cairoLineTo cr 70 (transY s)
		cairoStroke cr
		putText cr (Size 10) 25 (transY s - 9) . T.pack $ show s

	cairoSetLineWidth cr 0.3
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.3 0.3
	rs <- readFile `mapM` as
	(drawResult1 cr `mapM_`) =<< filterMachine (readResult . readDict <$> rs)
	cairoStroke cr

graph :: PrimMonad m => CairoT s (PrimState m) -> [(CDouble, CDouble)] -> m ()
graph _ [] = pure ()
graph cr ((x0, y0) : xys) = do
	cairoMoveTo cr x0 y0
	for_ xys \(x, y) -> cairoLineTo cr x y
	cairoStroke cr

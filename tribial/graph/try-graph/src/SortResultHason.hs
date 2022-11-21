{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SortResultHason where

import Control.Arrow
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time
import Data.Hason

data All = All {
	machine :: T.Text,
	header :: [T.Text],
	dat :: [[(Integer, NominalDiffTime)]] } deriving Show

readAll :: String -> All
readAll src = All {
	machine = getMachine dct,
	header = getHeader dct,
	dat = getResult dct }
	where dct = dctToMap $ read src

dctToMap :: Hason -> M.Map T.Text Hason
dctToMap (Dct dct) = M.fromList dct
dctToMap _ = error "not Dct"

getMachine :: M.Map T.Text Hason -> T.Text
getMachine dct = getText $ dct M.! "machine-id"

getText :: Hason -> T.Text
getText = \case T t -> t; _ -> error "not text"

getList :: Hason -> [Hason]
getList = \case L l -> l; _ -> error "not list"

getHeader :: M.Map T.Text Hason -> [T.Text]
getHeader dct = getText <$> getList (dct M.! "header")

getResultDct :: M.Map T.Text Hason -> [M.Map T.Text Hason]
getResultDct dct = dctToMap <$> getList (dct M.! "result")

getInteger :: Hason -> Integer
getInteger = \case I i -> i; _ -> error "not integer"

getDiffTime :: Hason -> NominalDiffTime
getDiffTime = \case DT dt -> dt; _ -> error "not diff time"

resultToTuples :: M.Map T.Text Hason -> [(Integer, NominalDiffTime)]
resultToTuples = uncurry (map . (,)) .
	(getInteger . (M.! "N") &&& (getDiffTime <$>) . getList . (M.! "time"))

getResult :: M.Map T.Text Hason -> [[(Integer, NominalDiffTime)]]
getResult = L.transpose . (resultToTuples <$>) . getResultDct

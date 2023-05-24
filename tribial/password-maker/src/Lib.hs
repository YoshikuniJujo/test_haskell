{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (randomWords, getWords, getWordsFrom) where

import Data.Yaml
import System.Random
import Paths_password_maker

import qualified Data.Vector as V
import qualified Data.Text as T

import qualified Data.Aeson.KeyMap as K

randomWords :: IO [T.Text]
randomWords = do
	ws <- getWords
	g <- getStdGen
	return . map (ws !!) $ randomRs (0, length ws - 1) g

getWords :: IO [T.Text]
getWords = getWordsFrom =<< getDataFileName "words.yaml"

getWordsFrom :: FilePath -> IO [T.Text]
getWordsFrom fp = stringsFromObject <$> decodeFileThrow @_ @Object fp

stringsFromObject :: Object -> [T.Text]
stringsFromObject o = stringsFromValue =<< K.elems o

stringsFromValue :: Value -> [T.Text]
stringsFromValue (Object o) = stringsFromObject o
stringsFromValue (Array a) = stringsFromValue =<< V.toList a
stringsFromValue (String t) = [t]
stringsFromValue _ = []

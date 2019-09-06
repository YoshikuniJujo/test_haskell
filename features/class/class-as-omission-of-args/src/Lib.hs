{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

tenTimes :: (a -> a) -> a -> a
tenTimes f = (!! 10) . iterate f

tenTimes' :: TenTimes a => a -> a
tenTimes' = (!! 10) . iterate tenTimesFun

class TenTimes a where
	tenTimesFun :: a -> a

newtype TenTimesAddOne n = TenTimesAddOne { getTenTimesAddOne :: n }
	deriving Show

instance Num n => TenTimes (TenTimesAddOne n) where
	tenTimesFun = TenTimesAddOne . (+ 1) . getTenTimesAddOne

newtype TenTimesDouble n = TenTimesDouble { getTenTimesDouble :: n }
	deriving Show

instance Num n => TenTimes (TenTimesDouble n) where
	tenTimesFun = TenTimesDouble . (* 2) . getTenTimesDouble

newtype Bucciarati = Bucciarati { getBucciarati :: String }

instance TenTimes Bucciarati where
	tenTimesFun = Bucciarati . ("アリ" ++) . getBucciarati

newtype Narancia = Narancia { getNarancia :: String }

instance TenTimes Narancia where
	tenTimesFun = Narancia . ("ボラ" ++) . getNarancia

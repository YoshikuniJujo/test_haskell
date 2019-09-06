{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import System.Random

prefectures :: [String]
prefectures = [
	"北海",
	"青森",
	"秋田",
	"岩手",
	"山形",
	"宮城",
	"新潟",
	"福島",
	"石川",
	"富山",
	"長野",
	"群馬",
	"栃木",
	"茨城",
	"福井",
	"岐阜",
	"山梨",
	"埼玉",
	"東京",
	"神奈川",
	"千葉",
	"京都",
	"滋賀",
	"愛知",
	"静岡",
	"兵庫",
	"大阪",
	"奈良",
	"三重",
	"和歌山",
	"鳥取",
	"岡山",
	"島根",
	"広島",
	"山口",
	"香川",
	"徳島",
	"愛媛",
	"高知",
	"福岡",
	"佐賀",
	"長崎",
	"熊本",
	"大分",
	"宮崎",
	"鹿児島",
	"沖縄"
	]

prefectureTypes :: String
prefectureTypes = "都道府県"

randomsFromList :: RandomGen g => [a] -> g -> [a]
randomsFromList lst g = (lst !!) <$> randomRs (0, length lst - 1) g

fakePrefecture :: forall g . RandomGen g => g -> String
fakePrefecture g =
	take (head $ randomsFromList (length <$> prefectures) g)
		(randomsFromList (concat prefectures) g') ++
	take 1 (randomsFromList prefectureTypes g'')
	where
	(g', s) = split g :: (g, g)
	(g'', _) = split s :: (g, g)

fakePrefectureIO :: IO String
fakePrefectureIO = fakePrefecture <$> newStdGen

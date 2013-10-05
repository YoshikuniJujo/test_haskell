module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第7回 型クラス"

pages :: [Page]
pages = [
	titlePage, prelude,
	simpleExam1, simpleExam2, eqShowSummary,
--	duckTyping, color, vegetable, fruit, fortune
	naDatabase
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型クラスはHaskellのなかで一番好きな機能", \t -> do
	itext t 1 "- 誰が?", \t -> do
	itext t 1 "- 僕が", \t -> do
	text t "* なんとなく気持ちがいい", \t -> do
	itext t 1 "- 何が?", \t -> do
	itext t 1 "- インターフェースをさきに定義"
	itext t 1 "- 実装はあと", \t -> do
	text t "* Haskellで多重定義(オーバーロード)を使うための枠組"
 ]

simpleExam1 :: Page
simpleExam1 = [\t -> do
	writeTopTitle t "簡単な例", \t -> do
	text t "type Name = String"
	text t "data Animal = Dog Name | Cat Name"
	text t "", \t -> do
	text t "Animalを(==)で比較したい"
	text t "", \t -> do
	text t "instance Eq Animal where"
	itext t 1 "Dog n1 == Dog n2 = n1 == n2"
	itext t 1 "Cat n1 == Cat n2 = n1 == n2"
	itext t 1 "_ == _ = False"
	text t "", \t -> do
	text t "Dog \"taro\" == Dog \"taro\" => True"
	text t "Dog \"taro\" == Dog \"hanako\" => False"
	text t "Dog \"taro\" == Cat \"taro\" => False"
 ]

simpleExam2 :: Page
simpleExam2 = [\t -> do
	writeTopTitle t "簡単な例"
	text t "Animalを表示したい"
	text t "", \t -> do
	text t "instance Show Animal where"
	itext t 1 "show (Dog n) = \"Dog \" ++ show n"
	itext t 1 "show (Cat n) = \"Cat \" ++ show n"
	text t "", \t -> do
	text t "show (Dog \"taro\") => \"Dog \\\"taro\\\"\""
	text t "show (Cat \"hanako\") => \"Cat \\\"hanako\\\"\""
 ]

eqShowSummary :: Page
eqShowSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* EqやShowははじめから定義されている型クラス", \t -> do
	text t "* 型クラスにはderiving節で簡単に定義できるものも", \t -> do
	itext t 1 "Eq, Ord, Enum, Ix, Bounded, Read, Show"
	itext t 1 "これらはたとえば"
	itext t 1 "data ... = ... deriving (Eq, Show)"
	itext t 1 "とするだけで「よきにはから」ってくれる", \t -> do
	text t "* 型クラスは定義ずみのものだけではない"
	text t "", \t -> do
	semititle t "自分で定義することができる"
 ]

duckTyping :: Page
duckTyping = [\t -> do
	writeTopTitle t "あひる"
	text t "", \t -> do
	text t "「あひるのように歩きあひるのように鳴くのなら、"
	itext t 1 "それはあひるである」"
	text t "", \t -> do
	text t "class DuckLike a where"
	itext t 1 "duckWalk :: a -> String"
	itext t 1 "quack :: a -> String"
	text t "", \t -> do
	semititle t "このページのことは忘れてください"
 ]

color :: Page
color = [\t -> do
	writeTopTitle t "色々な色"
	text t "色を持つという性質を表すクラス"
	text t "", \t -> do
	text t "data Color = Red | Yellow | Green"
	text t ""
	text t "class Colored a where"
	itext t 1 "getColor :: a -> Color"
 ]

vegetable :: Page
vegetable = [\t -> do
	writeTopTitle t "色々な野菜"
	text t "", \t -> do
	text t "data Vegetable = Tomato | Paprika | Cabage"
	text t ""
	text t "instance Colored Vegetable where"
	itext t 1 "getColor Tomato = Red"
	itext t 1 "getColor Paprika = Yellow"
	itext t 1 "getColor Cabage = Green"
 ]

fruit :: Page
fruit = [\t -> do
	writeTopTitle t "色々な果物"
	text t "", \t -> do
	text t "data Fruit = Apple | Banana | Kiwi"
	text t ""
	text t "instance Colored Fruit where"
	itext t 1 "getColor Apple = Red"
	itext t 1 "getColor Banana = Yellow"
	itext t 1 "getColor Kiwi = Green"
 ]

fortune :: Page
fortune = [\t -> do
	writeTopTitle t "占い"
	text t "", \t -> do
	text t "fortune :: Colored a => a -> IO ()"
	text t "fortune x = putStrLn $ case getColor x of"
	itext t 1 "Red -> \"情熱的な人ですね。\""
	itext t 1 "Yellow -> \"楽天的って言われませんか?\""
	itext t 1 "Green -> \"年のわりに落ち着いてますね。\""
	text t "", \t -> do
	text t "fortune Apple => 情熱的な人ですね。", \t -> do
	text t "fortune Kiwi => 年のわりに落ち着いてますね。"
 ]

naDatabase :: Page
naDatabase = [\t -> do
	writeTopTitle t "名前から年齢を求める"
	text t "", \t -> do
	text t "type Name = String"
	text t "type Age = Int"
	text t ""
	text t "class NADatabase db where"
	itext t 1 "getAge :: Name -> db -> Age"
	text t "", \t -> do
	text t "tellAge :: NADatabase => db -> Name -> db -> IO ()"
	text t "tellAge n db = putStrLn $ n ++ \"さんは\" ++"
	itext t 1 "show getAge db ++ \"歳です\""
 ]

import Lecture

subtitle :: String
subtitle = "第21回 型の階層"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, prelude2,
	life, life2
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellで型の階層を作る方法を見ていこう", \t -> do
	text t "* 前回学んだ存在型とTypeableのcastを使う", \t -> do
	text t "* やや複雑だが非常に巧妙だ", \t -> do
	text t "* オブジェクト指向プログラミングへの回帰", \t -> do
	text t "* オブジェクト指向的に「どうしても」書きたいときに使う", \t -> do
	text t "* upcastやdowncastができる", \t -> do
	text t "* castできない型への変換は明示的にNothingになる", \t -> do
	text t "* 動きとしては以下のようになる", \t -> do
	itext t 1 "- 最上位の型への変換", \t -> do
	itext t 1 "- 最上位の型からの変換"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* オブジェクト指向的な例を挙げるところまではやらない", \t -> do
	text t "* 型の階層を作り、階層間でcastできるところまでを示す"
 ]

life :: Page
life = [\t -> do
	writeTopTitle t "生物の例"
	text t "", \t -> do
	text t "* 生物型を最上位の型とする", \t -> do
	text t "* 生物型の下に動物型と植物型があり", \t -> do
	text t "* 動物型の下に犬型と猫型があり", \t -> do
	text t "* 植物型の下に樹型と草型がある", \t -> do
	text t "* また生物型の直下に菌型も作ろう"
	text t "", \t -> do
	text t "* そういう例を作っていこう"
 ]

life2 :: Page
life2 = [\t -> do
	writeTopTitle t "SomeLife"
	text t "", \t -> do
	text t "* まずは最上位の型を作成する"
 ]

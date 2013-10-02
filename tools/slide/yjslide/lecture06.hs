module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第6回 入出力"

pages :: [Page]
pages = [
	titlePage, term, prelude1, prelude2, prelude2_5, prelude3,
	functionIO1, functionIO2,
	lazyList1, lazyList2,
	ioMachine1, ioMachine2, ioMachine3, ioMachine4, ioMachine5,
	ioMachine6, ioMachine7, ioMachine8, ioMachine9,
	ioMonad1, ioMonad2, ioMonad3
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

term :: Page
term = [\t -> do
	writeTopTitle t "用語について"
	text t "関数の入出力とプログラムの入出力がまぎらわしいので"
	text t "", \t -> do
	itext t 1 "関数の入力、出力、入出力", \t -> do
	dvArrowShort t
	itext t 1 "そのまま入力、出力、入出力"
	text t "", \t -> do
	itext t 1 "プログラムの入力、出力、入出力", \t -> do
	dvArrowShort t
	itext t 1 "インプット、アウトプット、I/O"
	text t ""
	text t "とする"
 ]

prelude1 :: Page
prelude1 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	semititle t "* HaskellではIO monadという仕組みを使う", \t -> do
	semititle t "* 理解しづらいことで有名", \t -> do
	itext t 1 "まぎらわしさがある"
	itext t 1 "説明のしかたがまずい", \t -> do
	dvArrowShort t
	text t "「わかりやすく説明してみようじゃないか」という野望"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t ""
	text t "まずはmonadを説明し、次にIO monadを説明する", \t -> do
	xmark t "まずはmonadを説明し、次にIO monadを説明する"
	arrowIText t 1 "monadは「実のところ」IO monadの本質ではない"
	text t "", \t -> do
	text t "むしろ"
	text t ""
	semititle t "「I/Oの仕組みを作ったところ、"
	semititle t "それがmonadという大きな枠組にはまりこんだ」"
	itext t 5 ""
	itext t 5 "が正解"
 ]

prelude2_5 :: Page
prelude2_5 = [\t -> do
	writeTopTitle t "はじめに"
	semititle t "説明の戦略", \t -> do
	semititle t "* I/Oを実現させた漢たちの架空の歴史をたどる", \t -> do
	semititle t "* 実際の歴史がどうだったかは知らない", \t -> do
	semititle t "* 今の形に至るまでの思考経路を想像", \t -> do
	dvArrowShort t
	semititle t "研究者達の(架空の)思考を追体験する", \t -> do
	dvArrowShort t
	semititle t "「なるほど!」と思えるかもしれない"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "問題提起"
	text t ""
	semititle t "「純粋関数型言語でI/Oはどうする?」", \t -> do
	text t "- 他の言語では副作用という形でI/Oを実現している", \t -> do
	text t "- しかし、副作用を許すと参照透過性が確保できない", \t -> do
	text t "- 遅延性があるのでI/Oがいつ実行されるか予想しづらい", \t -> do
	dvArrowShort t
	semititle t "特別な仕組みが必要", \t -> do
	itext t 1 "- 関数の入出力を使う", \t -> do
	itext t 1 "- 遅延リスト", \t -> do
	itext t 1 "- I/Oを行う機械または装置というメタファ"
 ]

functionIO1 :: Page
functionIO1 = [\t -> do
	writeTopTitle t "関数の入出力を使うという解"
	text t "", \t -> do
	semititle t "* String -> String型の関数を用意する", \t -> do
	semititle t "* インプットをその関数に対する入力とする", \t -> do
	semititle t "* その関数からの出力をアウトプットとする", \t -> do
	dvArrowShort t
	text t "わかりやすい!"
	preLine t
	itext t 3 "「フィルタ」が書ける!"
	text t ""
	text t "でも", \t -> do
	semititle t "* インプットとアウトプットはそれぞれ1つずつ", \t -> do
	semititle t "* 単純なフィルタしか作れない"
 ]

functionIO2 :: Page
functionIO2 = [\t -> do
	writeTopTitle t "関数の入出力を使うという解"
	text t "", \t -> do
	semititle t "* 現在はこのやりかたは使われていない", \t -> do
	semititle t "* 現在の枠組のなかで同じことをすることはできる", \t -> do
	semititle t "* そのための関数「interact」が用意されている", \t -> do
	itext t 1 "例: インプットを大文字化してアウトプットする"
	itext t 2 "interact $ map toUpper"
 ]

lazyList1 :: Page
lazyList1 = [\t -> do
	writeTopTitle t "遅延リストという解"
	text t "", \t -> do
	semititle t "* 問題となるのはアウトプットよりもインプット", \t -> do
	semititle t "* インプットを遅延リストとして用意する", \t -> do
	semititle t "* アウトプットは先行評価を強制すれば良い", \t -> do
	semititle t "* ファイルや標準入力にそれぞれひとつのリスト", \t -> do
	dvArrowShort t
	text t "複数のインプットに対応可!"
	text t ""
	text t "でも", \t -> do
	semititle t "入力のタイミングは制御できない"
 ]

lazyList2 :: Page
lazyList2 = [\t -> do
	writeTopTitle t "遅延リストという解"
	text t "", \t -> do
	semititle t "* 現在はこのやりかたは(ry"
	semititle t "* 現在の枠組のなかで同じことを(ry", \t -> do
	semititle t "* getContentsやreadFileなどの関数群を用意", \t -> do
	itext t 1 "例: do { inp <- getContents;"
	itext t 2.5 "file1 <- readFile \"file1\";"
	itext t 2.5 "putStr inp;"
	itext t 2.5 "putStr file1; }"
 ]

ioMachine1 :: Page
ioMachine1 = [\t -> do
	writeTopTitle t "I/Oマシンという解"
	text t "", \t -> do
	semititle t "* 副作用としてI/Oを行うというやりかたでは"
	itext t 1 "関数展開する部分とI/Oの部分がひとつ", \t -> do
	semititle t "* I/Oマシンというやりかたでは"
	itext t 1 "関数展開する部分とI/Oの部分とを分けている", \t -> do
	semititle t "* メタファとしては"
	itext t 1 "- 機械を組み立ててスイッチを入れる、または"
	itext t 1 "- 命令書きを組み立てて機械に渡す"
	itext t 5 "といった感じ"
 ]

ioMachine2 :: Page
ioMachine2 = [\t -> do
	writeTopTitle t "I/Oマシンという解", \t -> do
	semititle t "* IOMachineという型を持つ値を考える", \t -> do
	semititle t "* IOMachineを出力する関数をつくる", \t -> do
	semititle t "* 出力されたIOMachineのスイッチを入れる", \t -> do
	semititle t "* スイッチ処理は関数展開の部分とは独立", \t -> do
	semititle t "* 具体的には対話環境で評価またはmainに束縛"
	itext t 1 "例: main = someIO"
	text t "", \t -> do
	semititle t "mainはIOMachine型であり"
	semititle t "処理系によってスイッチを入れられる"
 ]

ioMachine3 :: Page
ioMachine3 = [\t -> do
	writeTopTitle t "I/Oマシンの型", \t -> do
	semititle t "* 組み立てるための機構が必要", \t -> do
	semititle t "* アウトプットのみならば単純につなげば良い", \t -> do
	itext t 1 "例: doSome >> doAnother", \t -> do
	semititle t "* インプットについて考えると難しくなる"
	itext t 1 "- インプットを行う機械の出力を"
	itext t 2 "アウトプットをする機械の入力につなぐ", \t -> do
	itext t 1 "- つなぐ際には型を合わせる必要がある", \t -> do
	itext t 1 "- 単純化のためIOMachineに入出力の両方を持たせる", \t -> do
	dvArrowShort t
	semititle t "I/Oマシンの型はIOMachine i oとなる"
	itext t 1 "(スペースの関係で今後はIOMcn i oとする)"
 ]

ioMachine4 :: Page
ioMachine4 = [\t -> do
	writeTopTitle t "I/Oマシンの組み立て"
	text t "", \t -> do
	semititle t "* マシン1の出力をマシン2の入力につなぐ関数"
	itext t 1 "pipe :: IOMcn a b -> IOMcn b c -> IOMcn a c"
	itext t 1 "これでインプットするマシンから"
	itext t 2 "アウトプットするマシンに値を渡せる", \t -> do
	semititle t "* 問題はまだ半分しか解決していない"
	itext t 1 "インプットをそのままアウトプットに渡せるだけ"
	itext t 1 "インプットを処理したものをアウトプットにしたい", \t -> do
	semititle t "* マシンへの入力に何らかの処理をする関数"
	itext t 1 "trans :: (a -> b) -> IOMcn b c -> IOMcn a c"
 ]

ioMachine5 :: Page
ioMachine5 = [\t -> do
	writeTopTitle t "IOMcnの世界"
	text t "", \t -> do
	semititle t "* IOMcnの世界を見てみよう", \t -> do
	itext t 1 "getLine :: IOMcn () String"
	itext t 1 "putLine :: IOMcn String () の2つがあるとき"
	text t "「インプットをreverseしてアウトプット」は", \t -> do
	dvArrowShort t
	itext t 1 "getLine `pipe` trans reverse putLine"
	text t "となる"
	text t "", \t -> do
	semititle t "これでI/Oに関しては何でもできる"
 ]

ioMachine6 :: Page
ioMachine6 = [\t -> do
	writeTopTitle t "IOMcnの世界の一歩先へ"
	text t "I/Oを実現した漢達の挑戦は終わらなかった"
	text t "", \t -> do
	text t "ここで、(>>>) = flip (.)を考える"
	itext t 1 "(* 2) >>> (+ 3)は2倍して3を足すということ"
	text t "", \t -> do
	text t "IOMcn i oの代わりにi -> IO oを使えば"
	itext t 1 "transの代わりに(>>>)が使えるじゃないか!"
	itext t 1 "(>>>) :: (a -> b) -> (b -> c) -> a -> c"
	itext t 1 "trans :: (a -> b) -> (b -> IO c) -> a -> IO c", \t -> do
	dvArrowShort t
	text t "transという組み込みの関数をひとつ減らせる!"
 ]

ioMachine7 :: Page
ioMachine7 = [\t -> do
	writeTopTitle t "pipeはどうなる?"
	text t "", \t -> do
	text t "pipe :: IOMcn a b -> IOMcn b c -> IOMcn a c"
	dvArrowShort t
	text t "pipe :: (a -> IO b) -> (b -> IO c) -> a -> IO c"
	text t "", \t -> do
	text t "これってもっと単純にできないの?", \t -> do
	arrowIText t 1 "できます!"
	text t "", \t -> do
	text t "(>>=) :: IO b -> (b -> IO c) -> IO c を定義しておけば"
	text t "pipe m1 m2 = \\x -> m1 x >>= m2 とできる"
 ]

ioMachine8 :: Page
ioMachine8 = [\t -> do
	writeTopTitle t ">>= だ!"
	text t "", \t -> do
	text t "(>>=) :: IO a -> (a -> IO b) -> IO b"
	text t "", \t -> do
	text t "おいっ! これって monad じゃね?(monadについては後述)"
	text t "monadだ!"
	preLine t
	itext t 3 "monadだ!"
	itext t 1 "「monadだ!!」"
	text t "m a -> (a -> m b) -> m b はmonadっっ"
	text t ""
	text t "monad には return :: a -> m a が必要"
	itext t 1 "(そういうことになっている)" 
	text t "return :: a -> IO a も作っておこう"
 ]

ioMachine9 :: Page
ioMachine9 = [\t -> do
	writeTopTitle t "結末"
	text t "", \t -> do
	text t "こうして"
	semititle t "I/OにはIO monadが使われるようになりましたとさ"
	text t ""
	text t ""
	semititle t "ということでこのあとは", \t -> do
	dvArrow t
	semititle t "IO monadについて見ていこう"
 ]

ioMonad1 :: Page
ioMonad1 = [\t -> do
	writeTopTitle t "IO monad"
	text t "", \t -> do
	semititle t "* monadについて詳しくは第8回あたりにやる予定"
	itext t 1 "(あるいはもっと後に)", \t -> do
	semititle t "* とりあえず今は以下の関数だけで良い"
	itext t 1 "return :: a -> m a"
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b"
	semititle t "* 上記の関数をIO monadに限定すると"
	itext t 1 "return :: a -> IO a"
	itext t 1 "(>>=) :: IO a -> (a -> IO b) -> IO b"
 ]

ioMonad2 :: Page
ioMonad2 = [\t -> do
	writeTopTitle t "return"
	text t "", \t -> do
	semititle t "* returnって何?", \t -> do
	semititle t "* 何もせずにその値を返す機械を作る関数", \t -> do
	semititle t "* 例えばreturn 8は何もせずに8を出力する機械", \t -> do
	dvArrow t
	semititle t "return \"Hello, world!\" >>= putStrLn"
	text t "やっと、これができた!"
 ]

ioMonad3 :: Page
ioMonad3 = [\t -> do
	writeTopTitle t "monad則 その1"
	text t "", \t -> do
	text t "* monad則というものが3つある", \t -> do
	text t "* monadを使う人がびっくりしないための規則", \t -> do
	text t "* そのうちのひとつを以下に示す", \t -> do
	semititle t "monad則1: return x >>= f == f x", \t -> do
	text t "* IO monadもこれを満たす"
	text t "よって"
	semititle t "return \"Hello, world!\" >>= putStrLn"
	dvArrowShort t
	semititle t "putStrLn \"Hello, world!\""
	text t "とできる"
 ]

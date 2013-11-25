import Lecture

subtitle :: String
subtitle = "第11回 ファイル入出力"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	useInteract, useInteract2, useInteract3, useInteract4, useInteract5,
	useInteractSummary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ファイル入出力について学ぶ", \t -> do
	text t "* いくつかのパラダイムが混在している", \t -> do
	text t "* そのため意外な落とし穴がある", \t -> do
	text t "* より先進的な機構については第53回「iteratee」で"
 ]

useInteract :: Page
useInteract = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* Unix系のOSではフィルタという考えかたがある", \t -> do
	itext t 1 "- 標準入力から入力を受け取り", \t -> do
	itext t 1 "- 何らかの加工を行い", \t -> do
	itext t 1 "- 標準出力に書き出す", \t -> do
	itext t 1 "- 入力と出力は並行して行われる", \t -> do
	text t "* 標準入力はデフォルトではキーボードからのインプット", \t -> do
	text t "* 標準出力はデフォルトではモニタへの出力", \t -> do
	text t "* 標準入出力はファイルにつなぎかえることができる", \t -> do
	text t "* 標準入出力は他のコマンドにつなぎかえることができる"
 ]

useInteract2 :: Page
useInteract2 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* フィルタは(String -> String)型の関数とみなせる", \t -> do
	arrowIText t 1 "関数型言語との相性が良い", \t -> do
	text t "* (String -> String)型の関数からフィルタを作る関数", \t -> do
	itext t 1 "interact :: (String -> String) -> IO ()", \t -> do
	text t "* Unix系OSで簡単なフィルタを作るのならばこれが使える"
 ]

useInteract3 :: Page
useInteract3 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* 最も単純な例", \t -> do
	itext t 1 "% cat id.hs"
	itext t 2 "main :: IO ()"
	itext t 2 "main = interact id", \t -> do
	itext t 1 "% runghc id.hs"
	itext t 1 "hello"
	itext t 1 "hello"
	itext t 1 "world"
	itext t 1 "world", \t -> do
	text t "* 標準入力からの入力をそのまま標準出力に書き出している", \t -> do
	text t "* 入力と出力が交互に行われていることに注目"
 ]

useInteract4 :: Page
useInteract4 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* 入力値をreverseして出力", \t -> do
	itext t 1 "% cat rev.hs"
	itext t 2 "main :: IO ()"
	itext t 2 "main = interact reverse", \t -> do
	itext t 1 "% runghc rev.hs"
	itext t 1 "hello"
	itext t 1 "world", \t -> do
	itext t 1 "[Ctrl-D]"
	itext t 1 "dlrow"
	itext t 1 "olleh%", \t -> do
	text t "* Ctrl-Dで入力を終了するまで出力されない"
 ]

useInteract5 :: Page
useInteract5 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* 出力に十分なだけの入力を受けとった時点で出力", \t -> do
	arrowIText t 1 "Haskellの遅延評価という性質を十二分に利用", \t -> do
	text t "* 最終的に(String -> String)という形を作れば良い", \t -> do
	arrowIText t 1 "問題解決に集中できる", \t -> do
	text t "* ほとんどのコードを純粋な関数に保つことができる", \t -> do
	arrowIText t 1 "IOに関する余計な複雑さを排除できる", \t -> do
	text t "* リダイレクトやパイプを使うことで柔軟な使いかたが", \t -> do
	dvArrowShort t
	text t "フィルタで十分な問題に対しては最高のスタイル"
 ]

useInteractSummary :: Page
useInteractSummary = [\t -> do
	writeTopTitle t "フィルタを作る(まとめ)"
	text t "", \t -> do
	text t "* 標準入力から標準出力へのフィルタという考えかた", \t -> do
	text t "* Unixで使う部品を作るときの理想的なスタイル", \t -> do
	text t "* コードの純粋さを保ちたいときの最適解", \t -> do
	text t "* 日常のプログラミングではフィルタで十分なことは多い", \t -> do
	itext t 1 "- ファイルを読み書きするにはリダイレクトを使う", \t -> do
	text t "* IOモナドというパラダイムより手前にあるやりかた", \t -> do
	text t "* 積極的に使おう"
 ]

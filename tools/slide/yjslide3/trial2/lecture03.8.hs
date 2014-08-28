import Lecture

subtitle :: String
subtitle = "トライアル 第3.8回 自習課題"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	funAscending, funAscending2, funAscending12,
	refer
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 演習では様々な多相関数を作成した", \t -> do
	text t "* プログラムのなかの共通する枠組みをくくり出した", \t -> do
	text t "* 自習課題では同様の関数をもうひとつ作ることにする"
 ]

funAscending :: Page
funAscending = [\t -> do
	writeTopTitle t "ascending"
	text t "", \t -> do
	text t "* 3つ引数を取り、その引数が昇順かどうかを確認する関数", \t -> do
	text t "* 3つに重複は許さないものとする", \t -> do
	text t "* そのような関数は以下のようになる", \t -> do
	itext t 1 "ascending :: Int -> Int -> Int -> Bool", \t -> do
	itext t 1 "ascending x y z = x < y && y < z", \t -> do
	text t "* ~/lectures/lecture01/に移動しstudy.hsに書き込む"
 ]

ascending :: Int -> Int -> Int -> Bool
ascending x y z = x < y && y < z

funAscending2 :: Page
funAscending2 = [\t -> do
	writeTopTitle t "ascending"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "% ghci study.hs", \t -> do
	itext t 1 "*Main> ascending 3 8 10", \t -> do
	itext t 1 $ show $ ascending 3 8 10, \t -> do
	itext t 1 "*Main> ascending 2 9 7", \t -> do
	itext t 1 $ show $ ascending 2 9 7, \t -> do
	itext t 1 "*Main> ascending 4 4 9", \t -> do
	itext t 1 $ show $ ascending 4 4 9
 ]

funAscending12 :: Page
funAscending12 = [\t -> do
	writeTopTitle t "ascending12"
	text t "", \t -> do
	text t "* 同様のことを12で割った余りについて行うとする", \t -> do
	text t "* 以下のようになるだろう", \t -> do
	itext t 0 "ascending12 :: Int -> Int -> Int -> Bool", \t -> do
	itext t 0 "ascending12 x y z ="
	itext t 1 "ascending (x `mod` 12) (y `mod` 12) (z `mod` 12)", \t -> do
	text t "* 「3つの引数すべてに同じ変換をする」という構造がある", \t -> do
	text t "* この構造を抽出した関数on3について考えよう", \t -> do
	text t "* on3を使ってascending12を定義すると以下のようになる", \t -> do
	itext t 1 "ascending12 x y z = on3 ascending (`mod` 12) x y z", \t -> do
	text t "* 自習課題. on3を定義せよ"
 ]

refer :: Page
refer = [\t -> do
	writeTopTitle t "参考"
	text t "", \t -> do
	text t "* Windowsにghcをインストールするには以下のexeを実行", \t -> do
	text t "http://www.haskell.org/platform/download/2013.2.0.0/"
	itext t 1 "HaskellPlatform-2013.2.0.0-setup.exe"
 ]

import Lecture

subtitle :: String
subtitle = "第37回 不変配列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	dataArray,
	randomAccess, randomAccess2, randomAccess3, randomAccess4,
	randomAccess5,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* リストは基本的に一度だけ頭から読むのに向いた構造", \t -> do
	text t "* 保存して何度もランダムアクセスするのには向かない", \t -> do
	itext t 1 "- 小さいリストなら問題ない", \t -> do
	itext t 1 "- リストが大きくなると問題となる", \t -> do
	text t "* 今回学ぶ配列は以下のようなときに使う", \t -> do
	itext t 1 "- 基本的に一度作ったら変化しない", \t -> do
	itext t 1 "- 何度もランダムアクセスされる", \t -> do
	text t "* 値の変化を伴う配列については次回扱う"
 ]

dataArray :: Page
dataArray = [\t -> do
	writeTopTitle t "作りかた"
	text t "", \t -> do
	text t "* Data.ArrayモジュールのArray型を見ていこう", \t -> do
	itext t 1 "Array i e", \t -> do
	itext t 1 "- インデックスと値を指定する", \t -> do
	itext t 1 "- インデックスはIxクラスのインスタンス", \t -> do
	itext t 1 "- 整数値や整数値のタプルはIxのインスタンス", \t -> do
	itext t 1 "- Ixのインスタンスにすればインデックスとなる", \t -> do
	text t "* 配列を作成するには範囲とリストをわたす", \t -> do
	itext t 1 "array :: Ix i => (i, i) -> [(i, e)] -> Array i e", \t -> do
	itext t 1 "> array (3, 5) [(3, \"3\"), (4, \"4\"), (5, \"5\")]"
 ]

randomAccess :: Page
randomAccess = [\t -> do
	writeTopTitle t "ランダムアクセス"
	text t "", \t -> do
	text t "* ランダムアクセスとは?", \t -> do
	itext t 1 "- シーケンシャルアクセスの対義語", \t -> do
	itext t 1 "- 並びを前から順にではなく途中の値を取り出すこと", \t -> do
	text t "* リストを使った場合と配列を使った場合で効率を比較する", \t -> do
	text t "* timesDoの定義は前回と同じ"
 ]

randomAccess2 :: Page
randomAccess2 = [\t -> do
	writeTopTitle t "ランダムアクセス"
	text t "", \t -> do
	text t "* リストの場合", \t -> do
	itext t 1 "randomAccess :: [a] -> Int -> IO a"
	itext t 1 "randomAccess xs len = (xs !!) <$>"
	itext t 4 "randomRIO (0, len - 1)"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "let lst = map show [0 .. 10 ^ 7 - 1]"
	itext t 2 "(10 ^ 3) `timesDo`"
	itext t 3 "(randomAccess lst (10 ^ 7) >>= putStr)"
 ]

randomAccess3 :: Page
randomAccess3 = [\t -> do
	writeTopTitle t "ランダムアクセス"
	text t "", \t -> do
	text t "* かかった時間は", \t -> do
	itext t 1 "- 36.69秒のうちrandomAccessが98.8%", \t -> do
	itext t 1 "- ランダム値の生成を含む時間が36.25秒"
 ]

randomAccess4 :: Page
randomAccess4 = [\t -> do
	writeTopTitle t "ランダムアクセス", \t -> do
	text t "* 配列の場合", \t -> do
	itext t 1 "randomAccess :: Array Int a -> Int -> IO a"
	itext t 1 "randomAccess xs len = (xs !) <$>"
	itext t 4 "randomRIO (0, len - 1)"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "let"
	preLine t
	itext t 3 "lst = map show [0 .. 10 ^ 7 - 1]"
	itext t 3 "arr = array (0, 10 ^ 7 - 1)"
	itext t 4 "(zip [0 .. 10 ^ 7 - 1] lst)"
	itext t 2 "(10 ^ 3) `timesDo`"
	itext t 3 "(randomAccess arr (10 ^ 7) >>= putStr)"
	itext t 2 "putChar '\\n'"
 ]

randomAccess5 :: Page
randomAccess5 = [\t -> do
	writeTopTitle t "ランダムアクセス"
	text t "", \t -> do
	text t "* かかった時間は", \t -> do
	itext t 1 "- 1.18秒のうちrandomAccessが0.1%", \t -> do
	itext t 1 "- ランダム値の生成を含む時間が118マイクロ秒", \t -> do
	itext t 1 "- リストを使った場合は36.25秒なので", \t -> do
	itext t 1 "- 速度は30万倍となる"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 大きなリストで(!!)によるランダムアクセスの多用", \t -> do
	itext t 1 "- 一度配列を作っておくと大幅に効率が向上する", \t -> do
	text t "* 何でもかんでも配列にすれば良いわけではない", \t -> do
	text t "* シーケンシャルアクセスがメインであれば", \t -> do
	itext t 1 "- そのままリストを使おう"
 ]

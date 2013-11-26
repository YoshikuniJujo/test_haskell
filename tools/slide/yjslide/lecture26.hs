import Lecture

subtitle :: String
subtitle = "第26回 プロファイリング"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	costProf, costProf2, costProf3, costProf4, costProf5,
	memoryProf,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 最適化は後回しにすることが大事", \t -> do
	text t "* はやすぎる最適化は諸悪の根源", \t -> do
	text t "* O記法での差が出る最適化ははやめに考慮する価値がある", \t -> do
	text t "* それ以外の最適化は実際に速度的な問題が生じてから", \t -> do
	text t "* 最適化する前に必ずプロファイリングをとる", \t -> do
	itext t 1 "- ボトルネックにしぼって最適化する", \t -> do
	text t "* Haskellはコードから実際の動きが推測しづらい", \t -> do
	arrowIText t 1 "プロファイリングの重要性が高い", \t -> do
	text t "* プロファイリングについて学んでいこう"
 ]

costProf :: Page
costProf = [\t -> do
	writeTopTitle t "コスト集約点"
	text t "", \t -> do
	text t "% cat fib.hs"
	text t "fib :: Int -> Int"
	text t "fib 0 = 0"
	text t "fib 1 = 1"
	text t "fib n = fib (n - 1) + fib (n - 2)"
	text t ""
	text t "main :: IO ()"
	text t "main = print $ fib 40", \t -> do
	text t "% ghc -prof -fprof-auto fib.hs", \t -> do
	text t "% ./fib +RTS -p -RTS"
	text t "102334155"
 ]

costProf2 :: Page
costProf2 = [\t -> do
	writeTopTitle t "コスト集約点"
	text t "", \t -> do
	text t "* -prof -fprof-autoを作ってコンパイルする", \t -> do
	text t "* +RTSから-RTSのなかにランタイムシステムオプションを", \t -> do
	itext t 1 "- プロファイリングを取るには-pを設定する", \t -> do
	text t "* fib.profが出力される", \t -> do
	itext t 1 "- total time", \t -> do
	itext t 2 "プログラムの実行にかかった時間", \t -> do
	itext t 1 "- total alloc", \t -> do
	itext t 2 "メモリの確保量", \t -> do
	itext t 1 "- コストの高い集約点の表示", \t -> do
	itext t 1 "- すべての集約点を呼び出し木の形で時間と確保量を"
 ]

costProf3 :: Page
costProf3 = [\t -> do
	writeTopTitle t "コスト集約点"
	text t "", \t -> do
	text t "* それぞれのコスト集約点について", \t -> do
	itext t 1 "- 進入回数", \t -> do
	itext t 1 "- 下のコスト集約点を含まない", \t -> do
	itext t 2 "時間の割合", \t -> do
	itext t 2 "確保量の割合", \t -> do
	itext t 1 "- 下のコスト集約点を含む", \t -> do
	itext t 2 "時間の割合", \t -> do
	itext t 2 "確保量の割合", \t -> do
	text t "* コスト集約点は", \t -> do
	itext t 1 "- すべての関数", \t -> do
	itext t 1 "- トップレベルの式はモジュールごとにまとめてCAF"
 ]

costProf4 :: Page
costProf4 = [\t -> do
	writeTopTitle t "コスト集約点"
	text t "", \t -> do
	text t "* コスト集約点はオプションで変更できる", \t -> do
	itext t 1 "- -fprof-auto", \t -> do
	itext t 2 "すべての関数をコスト集約点とする", \t -> do
	itext t 1 "- -fprof-auto-top", \t -> do
	itext t 2 "トップレベルの関数をコスト集約点とする", \t -> do
	itext t 1 "- -fprof-auto-exported", \t -> do
	itext t 2 "エクスポートされた関数をコスト集約点とする", \t -> do
	itext t 1 "- -fprof-auto-calls", \t -> do
	itext t 2 "すべての関数呼び出しをコスト集約点とする", \t -> do
	itext t 1 "- -fprof-cafs", \t -> do
	itext t 2 "CAFを個々の集約点に分ける"
 ]

costProf5 :: Page
costProf5 = [\t -> do
	writeTopTitle t "コスト集約点"
	text t "", \t -> do
	text t "* コスト集約点を手動で挿入することができる", \t -> do
	itext t 1 "{-# SCC \"name\" #-} <expression>", \t -> do
	text t "* Haskellの識別子として有効な名前ならば", \t -> do
	itext t 1 "{-# SCC my_function #-} <expression>"
 ]

memoryProf :: Page
memoryProf = [\t -> do
	writeTopTitle t "メモリ使用状況"
	text t "", \t -> do
	text t "% ./fib +RTS -h -RTS", \t -> do
	text t "% hp2ps -c fib.hp", \t -> do
	text t "% ps2pdf fib.ps", \t -> do
	text t "* メモリ使用量のグラフがfib.pdfに作られる"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* プロファイリングの取りかたについて学んだ", \t -> do
	text t "* ボトルネックを知ることができる", \t -> do
	text t "* 時間ごとのメモリ使用量のグラフを得ることもできる"
 ]

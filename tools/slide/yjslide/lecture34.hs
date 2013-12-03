import Lecture

subtitle :: String
subtitle = "第34回 配列としての文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, useLazyList, useLazyList2,
	tempAndSubst, tempAndSubst2, tempAndSubst3
--	misunderstand, makeBigFile
--	profileUseString, profileUseByteString, profileUseByteStringLazy,
--	profileUseString2
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでは文字列は文字のリストである", \t -> do
	text t "* 多くの場合、これで十分である", \t -> do
	text t "* 文字列を一度だけ読み込み一度だけ出力するような場合", \t -> do
	itext t 1 "- 文字列は生成され使われGCされていく", \t -> do
	itext t 1 "- GCのぶんの効率の低下はあるが", \t -> do
	itext t 1 "- 大きな問題はない", \t -> do
	text t "* しかし、文字列は前から順に一度だけ読まれるだけか?", \t -> do
	text t "* 2回以上読まれたり、ランダムアクセスされたり、する", \t -> do
	text t "* このような場合、著しい", \t -> do
	itext t 1 "- 空間効率の低下や", \t -> do
	itext t 1 "- 時間効率の低下が生じる"
 ]

useLazyList :: Page
useLazyList = [\t -> do
	writeTopTitle t "遅延リストの適所"
	text t "", \t -> do
	text t "* Stringは遅延リスト", \t -> do
	itext t 1 "- 遅延リストはランダムアクセスには向かない", \t -> do
	itext t 1 "- 保存しておいて何度も使うという用途には向かない", \t -> do
	text t "* 遅延リストはストリーム", \t -> do
	itext t 1 "- 頭から順に食べていく関数に渡す", \t -> do
	itext t 1 "- そのような関数に次々と渡していく", \t -> do
	itext t 1 "- そういう用途に向いたデータ構造", \t -> do
	text t "* 何かを保存しておいて後から使うという用途には向かない", \t -> do
	text t "* 小さいデータなら使いやすさが効率低下よりも勝つが", \t -> do
	text t "* 大きいデータを保存するという用途には向かない"
 ]

useLazyList2 :: Page
useLazyList2 = [\t -> do
	writeTopTitle t "遅延リストの適所"
	text t "", \t -> do
	text t "* 遅延リストは言わば「くりかえし」の実体化", \t -> do
	text t "* 「くりかえし」という「動作」を「もの」で表現した", \t -> do
	text t "* だから、できるだけ「幽霊」のように扱うべき", \t -> do
	text t "* 見るまでは存在せず、見たあとは消えていく", \t -> do
	text t "* データのうけわたしのための一時的な構造である", \t -> do
	text t "* 一度「見た」リストをもう一度「見る」と効率が低下"
 ]

tempAndSubst :: Page
tempAndSubst = [\t -> do
	writeTopTitle t "文字列の2つの側面"
	text t "", \t -> do
	text t "* 文字列は文字のうけわたしのための構造", \t -> do
	text t "- でもある", \t -> do
	itext t 1 "ファイルを読み込んで何か処理して出力する", \t -> do
	itext t 1 "ユーザーの入力に対して何か反応させる", \t -> do
	itext t 1 "等々", \t -> do
	text t "* 文字列は保存されて何度も使われるもの", \t -> do
	text t "- でもある", \t -> do
	itext t 1 "テキストエディタなどでの編集", \t -> do
	itext t 1 "等々", \t -> do
	text t "* 前者を「一時文字列」、後者を「保存文字列」と呼ぼう"
 ]

tempAndSubst2 :: Page
tempAndSubst2 = [\t -> do
	writeTopTitle t "文字列の2つの側面"
	text t "", \t -> do
	text t "* 他の言語では「一時」と「保存」の区別は意識しない", \t -> do
	text t "* Haskellでは「くりかえし」を遅延リストで実体化", \t -> do
	text t "* リストは言わば「くりかえし」のために温められる構造", \t -> do
	itext t 1 "- 温めたら冷める前にはやく食べてしまう必要がある", \t -> do
	text t "* 保存のためには別の構造が必要になってくる"
 ]

tempAndSubst3 :: Page
tempAndSubst3 = [\t -> do
	writeTopTitle t "文字列の2つの側面"
	text t "", \t -> do
	text t "* 「文字列」が「一時」なのか「保存」なのか意識する", \t -> do
	text t "* 「一時」であればStringを使う", \t -> do
	text t "* 「保存」であればByteStringを使う"
 ]

misunderstand :: Page
misunderstand = [\t -> do
	writeTopTitle t "誤解"
	text t "", \t -> do
	text t "* 誤解しやすい点なので先に注意を喚起しておく", \t -> do
	text t "* StringをByteStringに換えれば効率が上がる", \t -> do
	itext t 1 "- わけではない", \t -> do
	itext t 1 "- 場合によっては無駄な空間効率の低下が生じる", \t -> do
	text t "* Stringを頭から消費して捨てていくタイプのプログラムで", \t -> do
	itext t 1 "- 単純にByteStringを使うと空間効率は低下する", \t -> do
	text t "* 実際にプロファイルを取ってみてみよう"
 ]

makeBigFile :: Page
makeBigFile = [\t -> do
	writeTopTitle t "ByteStringで空間効率の低下"
 ]

profileUseString :: Page
profileUseString = [\t -> do
	writeTopTitle t "String"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useString.png")
 ]

profileUseByteString :: Page
profileUseByteString = [\t -> do
	writeTopTitle t "ByteString"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useByteString.png")
 ]

profileUseByteStringLazy :: Page
profileUseByteStringLazy = [\t -> do
	writeTopTitle t "lazy ByteString"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useByteStringLazy.png")
 ]

profileUseString2 :: Page
profileUseString2 = [\t -> do
	writeTopTitle t "Stringに2回アクセス"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useString2.png")
 ]

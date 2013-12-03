import Lecture

subtitle :: String
subtitle = "第34回 配列としての文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, advantage,
	disadvantageProfile, profileUseString,
	disadvantageProfile2, profileUseByteString,
	disadvantageProfileSummary,
	usage, usage2
	
-- prelude, useLazyList, useLazyList2,
--	tempAndSubst, tempAndSubst2, tempAndSubst3
--	misunderstand, makeBigFile
--	profileUseString, profileUseByteString, profileUseByteStringLazy,
--	profileUseString2
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回は「リストとしての文字列」を見た", \t -> do
	itext t 1 "- 1度だけのシーケンシャルなアクセスが得意", \t -> do
	text t "* 今回は「配列としての文字列」を見ていこう", \t -> do
	text t "* 配列としての文字列にはByteStringとTextがある", \t -> do
	text t "* Textは「Charの配列」と考えられる", \t -> do
	itext t 1 "- つまりユニコード文字を保存している", \t -> do
	text t "* ByteStringは「Word8の配列」と考えられる", \t -> do
	itext t 1 "- つまり表現できるのは8ビット文字のみ"
 ]

advantage :: Page
advantage = [\t -> do
	writeTopTitle t "ByteStringを使う利点と欠点"
	text t "", \t -> do
	text t "* 配列なので要素へのランダムアクセスがO(1)でできる", \t -> do
	text t "* 内部表現として32bitのCharではなく8bitの値を使う", \t -> do
	text t "* リストと違い次のリストへのポインタを保持しなくてよい", \t -> do
	text t "* その一方、ユニコード文字を扱うのは難しい", \t -> do
	text t "* Haskellの豊富なリスト関数が使えない", \t -> do
	text t "* リストとは違い遅延しないので", \t -> do
	itext t 1 "- すべての文字が一度にメモリ上に確保される", \t -> do
	text t "* Stringでは文字の追加がO(1)だがByteStringではO(n)", \t -> do
	itext t 1 "- 文字を追加するような処理には向かない"
 ]

disadvantageProfile :: Page
disadvantageProfile = [\t -> do
	writeTopTitle t "メモリの使用量の比較"
	text t "", \t -> do
	text t "* StringとByteStringで以下の比較をする", \t -> do
	itext t 1 "- ファイル内容の表示時のメモリの使用量", \t -> do
	text t "* Stringを使った場合", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile \"big.txt\""
	itext t 2 "putStrLn cnt", \t -> do
	text t "* メモリ使用状況は次のようになる"
 ]

profileUseString :: Page
profileUseString = [\t -> do
	writeTopTitle t "メモリの使用量の比較"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useString.png"), \t -> do
	text t "* 9.6MBのファイルを表示するのに", \t -> do
	text t "* 60kBから80kBのあいだで一定のメモリの使用量"
 ]

disadvantageProfile2 :: Page
disadvantageProfile2 = [\t -> do
	writeTopTitle t "メモリの使用量の比較"
	text t "", \t -> do
	text t "* ByteStringを使った場合", \t -> do
	itext t 1 "import qualified Data.ByteString.Char8 as BS"
	itext t 1 ""
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- BS.readFile \"big.txt\""
	itext t 2 "BS.putStrLn cnt"
	text t "* メモリの使用状況は次のようになる"
 ]

profileUseByteString :: Page
profileUseByteString = [\t -> do
	writeTopTitle t "メモリの使用量の比較"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useByteString.png"), \t -> do
	text t "* 10MBほどで一定したメモリ使用量", \t -> do
	text t "* 9.6MBのファイルを読んだので予想通りの動作"
 ]

disadvantageProfileSummary :: Page
disadvantageProfileSummary = [\t -> do
	writeTopTitle t "メモリの使用量の比較(まとめ)"
	text t "", \t -> do
	text t "* Stringを使用したほうは80kBのメモリ使用量", \t -> do
	text t "* ByteStringを使用したほうは10MBのメモリ使用量", \t -> do
	text t "* StringをByteStringに置き換えても", \t -> do
	itext t 1 "- 必ずしも効率が向上するとはかぎらない", \t -> do
	text t "* 「何をしたいのか」を明確にする", \t -> do
	itext t 1 "- 複数回のアクセスやランダムアクセスをしなければ", \t -> do
	itext t 1 "- Stringを使ったほうが良い"
 ]

usage :: Page
usage = [\t -> do
	writeTopTitle t "ByteStringの使いかた"
	text t "", \t -> do
	text t "* 同じByteStringに対して2つの見かたがある", \t -> do
	itext t 1 "- Word8の配列と見る", \t -> do
	itext t 1 "- Charの配列と見る", \t -> do
	text t "* ByteStringの本質からするとひとつめの見かたが正しい", \t -> do
	text t "* 後者は使用の際の利便性を考慮した見かた", \t -> do
	text t "* 2つの見かたに対してそれぞれ別のモジュールがある", \t -> do
	itext t 1 "- Data.ByteString", \t -> do
	itext t 1 "- Data.ByteString.Char8"
 ]

usage2 :: Page
usage2 = [\t -> do
	writeTopTitle t "ByteStringの使いかた"
	text t ""
 ]

prelude_ :: Page
prelude_ = [\t -> do
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

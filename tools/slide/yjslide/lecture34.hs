import Lecture

subtitle :: String
subtitle = "第34回 配列としての文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, advantage,
	disadvantageProfile, profileUseString,
	disadvantageProfile2, profileUseByteString,
	disadvantageProfileSummary,
	usage, usage2, usage3, usage4, usage5, usage6,
	randomAccess, randomAccess2, randomAccess3, randomAccess4,
	randomAccess5,
	aboutText,
	summary
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
	text t "", \t -> do
	text t "* Data.ByteStringにはPreludeと同じ名前の関数が多数", \t -> do
	text t "* 以下のように修飾名を使うようにする", \t -> do
	itext t 1 "import qualified Data.ByteString as BS", \t -> do
	text t "* リスト関数と同じ名前の関数が多数存在", \t -> do
	itext t 1 "- リストにおけるaをWord8に", \t -> do
	itext t 1 "- リストにおける[a]をByteStringに", \t -> do
	itext t 1 "- そうするとほぼ同じような動作をする", \t -> do
	text t "* Stringに対するIOと同じ名前の関数もある", \t -> do
	itext t 1 "- ByteStringに対してほぼ同じことをする"
 ]

usage3 :: Page
usage3 = [\t -> do
	writeTopTitle t "ByteStringの使いかた"
	text t "", \t -> do
	text t "* Word8のリストとの相互変換", \t -> do
	itext t 1 "pack :: [Word8] -> ByteString -- O(n)"
	itext t 1 "unpack :: ByteString -> [Word8] -- O(n)", \t -> do
	text t "* リストのコンストラクタとパターンマッチ相当の関数", \t -> do
	itext t 1 "cons :: Word8 -> ByteString -> ByteString -- O(n)"
	itext t 1 "uncons :: ByteString -> Maybe (Word8, ByteString)"
	itext t 5 "-- O(1)", \t -> do
	text t "* consがO(n)であることに注意", \t -> do
	itext t 1 "- リストの(:)はO(1)", \t -> do
	itext t 1 "- consでByteStringを構成するのは避けたほうが良い"
 ]

usage4 :: Page
usage4 = [\t -> do
	writeTopTitle t "ByteStringの使いかた"
	text t "", \t -> do
	text t "* ランダムアクセス関数", \t -> do
	itext t 1 "index :: ByteString -> Int -> Word8 -- O(1)", \t -> do
	text t "* indexがO(1)であることに注意", \t -> do
	itext t 1 "- リストの(!!)はO(n)", \t -> do
	text t "* 入出力関数", \t -> do
	itext t 1 "readFile :: FilePath -> IO ByteString"
	itext t 1 "writeFile :: FilePath -> ByteString -> IO ()"
	itext t 1 "appendFile :: FilePath -> ByteString -> IO ()"
	itext t 1 "- 他にもStringの同名の入出力関数と同様のものが"
 ]

usage5 :: Page
usage5 = [\t -> do
	writeTopTitle t "ByteStringの使いかた"
	text t "", \t -> do
	text t "* Data.ByteString.Char8モジュールも同様", \t -> do
	text t "* Word8ではなくCharを扱う関数が定義されている", \t -> do
	text t "* CharはByteStringに保存の際に8bitに切りつめられる"
 ]

usage6 :: Page
usage6 = [\t -> do
	writeTopTitle t "ByteStringの使いどころ"
	text t "", \t -> do
	text t "* メモリにおさまる程度の大きめのファイルを読み込む", \t -> do
	text t "* 読み込んだデータは主に読み込みに使う", \t -> do
	text t "* 読み込んだデータは何度でも、ランダムアクセスできる", \t -> do
	text t "* データを変更しようとすると効率が低下する", \t -> do
	itext t 1 "- 文字列全体をコピーする必要がある", \t -> do
	text t "* 前回のランダムアクセスなどは得意分野"
 ]

randomAccess :: Page
randomAccess = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* ランダムアクセスについてStringとByteStringを比較する", \t -> do
	text t "* 共通に使うくりかえし関数を定義しておく", \t -> do
	itext t 1 "timesDo :: Int -> IO () -> IO ()"
	itext t 1 "0 `timesDo` _ = return ()"
	itext t 1 "n `timesDo` io = io >> (n - 1) `timesDo` io"
 ]

randomAccess2 :: Page
randomAccess2 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* Stringを使う場合", \t -> do
	itext t 1 "randomAccess :: String -> Int -> IO Char"
	itext t 1 "randomAccess str len = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "return $ str !! i"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile \"big.txt\""
	itext t 2 "1000 `timesDo`"
	itext t 2.5 "(randomAccess cnt (10 ^ 7) >>= putChar)"
	itext t 2 "putChar '\\n'"
 ]

randomAccess3 :: Page
randomAccess3 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* このときの時間は", \t -> do
	itext t 1 "- 25.32秒のうちrandomAccessが98.5%", \t -> do
	itext t 1 "- よってランダム値の生成も含めた時間が24.94秒"
 ]

randomAccess4 :: Page
randomAccess4 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* ByteStringを使った場合", \t -> do
	itext t 1 "randomAccess :: BSC.ByteString -> Int -> IO Char"
	itext t 1 "randomAccess str len = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "return $ BSC.index str i"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- BSC.readFile \"big.txt\""
	itext t 2 "1000 `timesDo`"
	itext t 2.5 "(randomAccess cnt (10 ^ 7) >>= putChar)"
	itext t 2 "putChar '\\n'"
 ]

randomAccess5 :: Page
randomAccess5 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* かかった時間は", \t -> do
	itext t 1 "- 0.01秒でrandomAccessが20.0%", \t -> do
	itext t 1 "- よって0.002秒", \t -> do
	text t "* Stringを使ったとき(24.94秒)とくらべると", \t -> do
	itext t 1 "- 1万倍以上の時間効率の向上"
 ]

aboutText :: Page
aboutText = [\t -> do
	writeTopTitle t "Text"
	text t "", \t -> do
	text t "* ByteStringはword8の配列なので", \t -> do
	itext t 1 "> BSC.pack \"あいうえお\""
	itext t 1 "\"BDFHJ\"", \t -> do
	text t "* マルチバイト文字はうまく扱えない", \t -> do
	text t "* Textなら", \t -> do
	itext t 1 "> T.pack \"あいうえお\""
	itext t 1 "\"\12354\12356\12358\12360\12362\"", \t -> do
	itext t 1 "> T.putStr it"
	itext t 1 "あいうえお", \t -> do
	text t "* その他使いかたはByteStringと同様", \t -> do
	itext t 1 "- IO関係がData.Text.IOにあるなど小さな違いはある"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Stringには苦手な分野がある", \t -> do
	itext t 1 "- 2回以上読み込む必要があるとき", \t -> do
	itext t 1 "- ランダムアクセスが必要なとき", \t -> do
	text t "* そのような時にByteStringやTextが使える", \t -> do
	itext t 1 "- ただし、データの変更を伴わない場合に限る", \t -> do
	text t "* ByteStringはバイナリファイルやASCIIファイルに使う", \t -> do
	text t "* マルチバイト文字が必要な場合はTextを使う"
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

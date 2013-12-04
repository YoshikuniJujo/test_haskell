import Lecture

subtitle :: String
subtitle = "第35回 配列の遅延リストとしての文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	readFileProfile, readFileProfileUseString, readFileProfileUseByteString,
	readFileProfileLazy, -- readFileProfile2,
	usage, usage2, randomAccess
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* リストとしての文字列", \t -> do
	itext t 1 "- ストリームとして使うのに最適", \t -> do
	itext t 1 "- 要素を頭につけ加える効率は良い", \t -> do
	itext t 1 "- ランダムアクセスが苦手", \t -> do
	text t "* 配列としての文字列", \t -> do
	itext t 1 "- ランダムアクセスが得意", \t -> do
	itext t 1 "- 要素の追加の効率が悪い", \t -> do
	itext t 1 "- 全体を一度にメモリに読み込む", \t -> do
	text t "* その中間くらいの文字列が欲しい", \t -> do
	itext t 1 "- それが配列のリストとしての文字列"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 遅延ByteStringが用意されている", \t -> do
	itext t 1 "- 配列のリストとしての文字列", \t -> do
	itext t 1 "- Stringと正格ByteStringの間くらいの性質", \t -> do
	itext t 1 "- バランスが良い", \t -> do
	itext t 1 "- 深く考えなくても効率が上昇する可能性が高い", \t -> do
	itext t 1 "- デフォルトでは32kBのかたまりのリストとなる", \t -> do
	itext t 1 "- つまり、32kBずつメモリに読み込んでくれる"
 ]

readFileProfile :: Page
readFileProfile = [\t -> do
	writeTopTitle t "メモリの使用量の比較"
	text t "", \t -> do
	text t "* 前回のStringとByteStringの比較を再掲する"
	itext t 1 "- ファイル内容の表示時のメモリの使用量"
 ]

readFileProfileUseString :: Page
readFileProfileUseString = [\t -> do
	writeTopTitle t "メモリの使用量(String)"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useString.png"), \t -> do
	text t "* 9.6MBのファイルを表示するのに", \t -> do
	text t "* 60kBから80kBのあいだでほぼ一定のメモリの使用量"
 ]

readFileProfileUseByteString :: Page
readFileProfileUseByteString = [\t -> do
	writeTopTitle t "メモリの使用量(正格ByteString)"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useByteString.png"), \t -> do
	text t "* 9.6MBのファイルを読み込んだので", \t -> do
	text t "* 10MBほどで一定したメモリ使用量"
 ]

readFileProfileLazy :: Page
readFileProfileLazy = [\t -> do
	writeTopTitle t "メモリの使用量(遅延ByteString)"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/useByteStringLazy.png"), \t -> do
	text t "* 80kBほどで一定したメモリの使用量", \t -> do
	text t "* 一度に全てのメモリを読み込んでいないことがわかる"
 ]

readFileProfile2 :: Page
readFileProfile2 = [\t -> do
	writeTopTitle t "メモリの使用量"
--	text t "", \t -> do
 ]

usage :: Page
usage = [\t -> do
	writeTopTitle t "使いかた"
	text t "", \t -> do
	text t "* 正格ByteStringとの相互変換関数がある", \t -> do
	itext t 1 "fromStrict :: BS.ByteString -> BSL.ByteString"
	itext t 1 "toStrict :: BSL.ByteString -> BS.ByteString", \t -> do
	text t "* 正格ByteStringのリストとの相互変換関数がある", \t -> do
	itext t 1 "fromChunks :: [BS.ByteString] -> BSL.ByteString"
	itext t 1 "toChunks :: BSL.ByteString -> [BS.ByteString]", \t -> do
	text t "* 文字を前に追加するのはO(1)", \t -> do
	itext t 1 "cons :: Word8 -> ByteString -> ByteString", \t -> do
	text t "* indexはO(n)だがStringと比べると32k倍速い", \t -> do
	itext t 1 "index :: ByteString -> Int64 -> Word8"
 ]

usage2 :: Page
usage2 = [\t -> do
	writeTopTitle t "使いかた"
	text t "", \t -> do
	text t "* その他、使いかたは正格版と同様"
 ]

randomAccess :: Page
randomAccess = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* ランダムアクセスの効率を比較してみよう"
 ]

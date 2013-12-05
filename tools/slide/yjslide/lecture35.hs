import Lecture

subtitle :: String
subtitle = "第35回 配列の遅延リストとしての文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	readFileProfile, readFileProfileUseString, readFileProfileUseByteString,
	readFileProfileLazy, -- readFileProfile2,
	usage, usage2,
	randomAccess, randomAccess2, randomAccess3, randomAccess4, randomAccess5,
	randomAccess6,
	summary
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
	text t "* 前回のStringとByteStringの比較を再掲する", \t -> do
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
	text t "* ランダムアクセスの効率を比較してみよう", \t -> do
	text t "* 前回のテストプログラムだと差が出ない", \t -> do
	itext t 1 "- 9.6MBの文字列に1000回アクセス", \t -> do
	itext t 1 "- 正格版、遅延版ともに0.01秒", \t -> do
	text t "* ファイルを大きくしアクセス回数も増やす", \t -> do
	itext t 1 "- 96MBの文字列に100万回アクセス", \t -> do
	text t "* timesDoの定義は前回と同じ"
 ]

randomAccess2 :: Page
randomAccess2 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* 正格ByteStringだと", \t -> do
	itext t 1 "randomAccess :: BSC.ByteString -> Int -> IO Char"
	itext t 1 "randomAccess str len = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "return $ BSC.index str i"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- BSC.readFile \"moreBig.txt\""
	itext t 2 "(10 ^ 6) `timesDo`"
	itext t 2.5 "(randomAccess cnt (10 ^ 8) >>= putChar)"
	itext t 2 "putChar '\\n'"
 ]

randomAccess3 :: Page
randomAccess3 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* かかった時間は", \t -> do
	itext t 1 "- 2.06秒のうち54.8%がrandomAccess", \t -> do
	itext t 1 "- ランダム値の生成を含めた時間は1.13秒"
 ]

randomAccess4 :: Page
randomAccess4 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* 遅延ByteStringだと", \t -> do
	itext t 1 "randomAccess :: BSLC.ByteString -> Int -> IO Char"
	itext t 1 "randomAccess str len = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "return $ BSLC.index str i"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- BSLC.readFile \"moreBig.txt\""
	itext t 2 "(10 ^ 6) `timesDo`"
	itext t 2.5 "(randomAccess cnt (10 ^ 8) >>= putChar)"
	itext t 2 "putChar '\\n'"
 ]

randomAccess5 :: Page
randomAccess5 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率", \t -> do
	text t "* かかった時間は", \t -> do
	itext t 1 "- 18.58秒のうち93.4%がrandomAccess", \t -> do
	itext t 1 "- ランダム値の生成を含む時間は17.35秒", \t -> do
	itext t 1 "- 正格版の1.13秒と比較すると15倍の時間", \t -> do
	text t "* ファイルのサイズを2倍にすると", \t -> do
	itext t 1 "- 37.27秒のうち96.5%がrandomAccess", \t -> do
	itext t 1 "- よって35.97秒", \t -> do
	itext t 1 "- 2倍になっている", \t -> do
	text t "* 正格版でも同様にファイルサイズを2倍にする", \t -> do
	itext t 1 "- 2.32秒のうち58.1%がrandomAccess", \t -> do
	itext t 1 "- よって1.35秒", \t -> do
	itext t 1 "- 本来は変化しないはずだが1.2倍ほどになっている", \t -> do
	itext t 1 "- 多少の影響はあるということか"
 ]

randomAccess6 :: Page
randomAccess6 = [\t -> do
	writeTopTitle t "ランダムアクセスの効率"
	text t "", \t -> do
	text t "* ランダムアクセスでは", \t -> do
	itext t 1 "- Stringよりは効率がずっと良い", \t -> do
	itext t 1 "- 32000倍ほど良いはず", \t -> do
	itext t 1 "- それでもO(n)になっている"
 ]

aboutText :: Page
aboutText = [\t -> do
	writeTopTitle t "遅延Text"
	text t "", \t -> do
	text t "* 正格版と同様に遅延版にもTextがある", \t -> do
	itext t 1 "- 正格版と同様の関係", \t -> do
	itext t 1 "- マルチバイト文字を使うときはこちら"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 配列のリストとしての文字列について学んだ", \t -> do
	text t "* リストとしての文字列と配列としての文字列のあいだ", \t -> do
	text t "* あまり深く考えずにStringから置き換えかとしても", \t -> do
	itext t 1 "- 効率が向上することが多い", \t -> do
	text t "* ASCIIやバイナリファイルであればまずは4倍の空間効率", \t -> do
	text t "* また、文字のまとまりを扱うことでほぼ2倍程度の空間効率", \t -> do
	text t "* ランダムアクセスについてもリストの長さが32k分の1", \t -> do
	itext t 1 "- 32000倍の効率が予想される"
 ]

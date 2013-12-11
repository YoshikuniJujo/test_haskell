import Lecture

subtitle :: String
subtitle = "第43回 C言語とのインターフェース2"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	convertIntegral, convertIntegral2, convertIntegral3,
	getSimpleValue, getSimpleValue2, getSimpleValue3
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回、Ptr型を介したCの関数とのやりとりについて学んだ", \t -> do
	itext t 1 "- 値の中身をいじるのはCの関数のみ", \t -> do
	itext t 1 "- HaskellはCの関数からPtr型の値を受け取り", \t -> do
	itext t 1 "- それをCの関数にそのまま渡していた", \t -> do
	text t "* 今回の講義では中身の値に関して", \t -> do
	itext t 1 "- Haskell側で設定しC言語側に渡す", \t -> do
	itext t 1 "- C言語側から受け取った値をHaskellで利用する", \t -> do
	text t "* そのやりかたを学んでいこう"
 ]

convertIntegral :: Page
convertIntegral = [\t -> do
	writeTopTitle t "数値の安全な変換"
	text t "", \t -> do
	text t "* たいていにおいてCのintとHaskellのIntは同じ大きさ", \t -> do
	text t "* しかしHaskell 2010における定義は", \t -> do
	itext t 1 "- Intは[-2^29 .. 2^29-1]の範囲以上を表せれば良い", \t -> do
	text t "* C言語のintにおいては決められているのは", \t -> do
	itext t 1 "sizeof(short) <= sizeof(int) <= sizeof(long)", \t -> do
	text t "* よってどちらの変換によってもオーバーフローが生じ得る", \t -> do
	text t "* fromIntegralは何も気にしない", \t -> do
	itext t 1 "> fromIntegral (10 ^ 10 :: Integer) :: Int"
	itext t 1 "1410065408", \t -> do
	text t "* 本当はきちんと対応する必要がある"
 ]

convertIntegral2 :: Page
convertIntegral2 = [\t -> do
	writeTopTitle t "数値の安全な変換"
	text t "", \t -> do
	text t "* 今回の講義では以下のようにする", \t -> do
	itext t 1 "- オーバーフローはerrorとする", \t -> do
	itext t 1 "- 理想的な解決法ではないが、無視するよりはまし", \t -> do
	text t "* 安全な変換関数を作っておく", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "safeConvertIntegral ::"
	itext t 2 "(Integral a, Integral b, Bounded b) =>"
	itext t 3 "a -> Maybe b"
 ]

convertIntegral3 :: Page
convertIntegral3 = [\t -> do
	writeTopTitle t "数値の安全な変換"
	text t "", \t -> do
	text t "* 本体は以下のようになる", \t -> do
	itext t 1 "safeConvertIntegral x = let"
	itext t 2 "r = fromIntegral x"
	itext t 2 "mx = fromIntegral $ maxBound `asTypeOf` r"
	itext t 2 "mn = fromIntegral $ minBound `asTypeOf` r in"
	itext t 2 "if x <= mx && x >= mn"
	itext t 3 "then Just r else Nothing", \t -> do
	text t "* これを使いNothingのときには明示的にerrorを呼ぶ"
 ]

getSimpleValue :: Page
getSimpleValue = [\t -> do
	writeTopTitle t "単純な値を受け取る"
	text t "", \t -> do
	text t "* まずは単純な値の受け取りを学ぶ", \t -> do
	text t "* 結果を整数のポインタとして返すCの関数を考える", \t -> do
	itext t 1 "% cat counter.c"
	itext t 1 "int *counter(void) {"
	itext t 2 "static int c = 0;"
	itext t 2 "c++;"
	itext t 2 "return &c; }"
 ]

getSimpleValue2 :: Page
getSimpleValue2 = [\t -> do
	writeTopTitle t "単純な値を受け取る"
	text t "", \t -> do
	text t "* これを使うコードは以下のようになる", \t -> do
	itext t 1 "% cat useCounter.hs"
	itext t 1 "import Foreign.C.Types"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "import Foreign.Storable"
	itext t 2 "(続く)"
 ]

getSimpleValue3 :: Page
getSimpleValue3 = [\t -> do
	writeTopTitle t "単純な値を受け取る", \t -> do
	itext t 0 "foreign import ccall \"counter\""
	itext t 1 "c_counter :: IO (Ptr CInt)"
	itext t 0 "counter :: IO Int"
	itext t 0 "counter = do"
	preLine t
	itext t 2.5 "pci <- c_counter"
	itext t 2.5 "ci <- peek pci"
	itext t 2.5 "case safeConvertIntegral ci of"
	itext t 3.5 "Just i -> return i"
	itext t 3.5 "_ -> error \"conversion error\""
	itext t 0 "main :: IO ()"
	itext t 0 "main = do"
	preLine t
	itext t 2 "_ <- counter"
	itext t 2 "x <- counter"
	itext t 2 "print x"
 ]

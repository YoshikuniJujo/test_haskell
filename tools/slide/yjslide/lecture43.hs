import Lecture

subtitle :: String
subtitle = "第43回 C言語とのインターフェース2"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	convertIntegral, convertIntegral2, convertIntegral3,
	getSimpleValue, getSimpleValue2, getSimpleValue3, getSimpleValue4,
	sendSimpleValue, sendSimpleValue2, sendSimpleValue3, sendSimpleValue4,
	sendSimpleValue5, sendSimpleValue6, sendSimpleValue7,
	simpleValueSummary,
	array, array2, array3, array4, array5
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
	itext t 2 "a -> Maybe b"
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

getSimpleValue4 :: Page
getSimpleValue4 = [\t -> do
	writeTopTitle t "単純な値を受け取る"
	text t "", \t -> do
	text t "* ここに注目", \t -> do
	itext t 1 "ci <- peek pci", \t -> do
	text t "* Ptr型から値を取り出す関数", \t -> do
	itext t 1 "peek :: Storable a => Ptr a -> IO a", \t -> do
	itext t 1 "- Storableクラスについては後ほど説明する", \t -> do
	itext t 1 "- 数値、文字、Bool値はこのクラスのインスタンス"
 ]

sendSimpleValue :: Page
sendSimpleValue = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	text t "* Haskell側でポインタの指す値を設定して送る", \t -> do
	text t "* 与えられた数値によって挨拶を変える関数の例", \t -> do
	text t "* まずはヘッダファイルを作成する", \t -> do
	itext t 1 "% cat message.h"
	itext t 1 "typedef enum Message {"
	itext t 2 "Hello, GoodBye, GoodNight, Hi } Message;"
	itext t 1 "void sellect_message(Message *);"
	itext t 1 "void message(void);"
 ]

sendSimpleValue2 :: Page
sendSimpleValue2 = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	text t "* cのコード", \t -> do
	itext t 1 "#include <stdio.h>"
	itext t 1 "#include \"message.h\""
	itext t 1 "char *message_list[] = {"
	itext t 2 "\"Hello, world!\","
	itext t 2 "\"Good-bye, world!\","
	itext t 2 "\"Good night, world!\","
	itext t 2 "\"Hi, world!\" };"
	itext t 2 "(続く)"
 ]

sendSimpleValue3 :: Page
sendSimpleValue3 = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	itext t 0 "static Message *message_type;"
	itext t 0 "void sellect_message(Message *n) {"
	itext t 1 "*message_type = n; }"
	itext t 0 "void message(void) {"
	itext t 1 "printf(\"%s\\n\", message_list[*message_type]); }"
 ]

sendSimpleValue4 :: Page
sendSimpleValue4 = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	text t "* 比較のためCのmain関数を載せる", \t -> do
	itext t 1 "#include <stdlib.h>"
	itext t 1 "#include \"message.h\""
	itext t 1 "int main(int argc, char *argv[]) {"
	itext t 2 "enum Message *msg ="
	itext t 3 "calloc(1, sizeof(enum Message));"
	itext t 2 "sellect_message(msg);"
	itext t 2 "*msg = GoodNight;"
	itext t 2 "message();"
	itext t 2 "free(msg);"
	itext t 2 "return (); }"
 ]

sendSimpleValue5 :: Page
sendSimpleValue5 = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	text t "* Haskellでは", \t -> do
	itext t 1 "import Foreign.C.Type"
	itext t 1 "import Foreign.Marshal"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "foreign import ccall \"message.h sellect_message\""
	itext t 2 "c_sellectMessage :: Ptr CInt -> IO ()"
	itext t 1 "foreign import ccall \"message.h message\""
	itext t 2 "c_message :: IO ()"
	itext t 2 "(続く)"
 ]

sendSimpleValue6 :: Page
sendSimpleValue6 = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	itext t 1 "message :: Int -> IO ()"
	itext t 1 "message i = case safeConvertIntegral i of"
	itext t 2 "Just ci -> alloca $ \\pci -> do"
	itext t 3 "poke pci ci"
	itext t 3 "c_sellectMessage pci"
	itext t 3 "c_message"
	itext t 2 "_ -> error \"conversion error\""
	itext t 1 "main :: IO ()"
	itext t 1 "main = message 2"
 ]

sendSimpleValue7 :: Page
sendSimpleValue7 = [\t -> do
	writeTopTitle t "単純な値を送る"
	text t "", \t -> do
	text t "* Cと同様なやりかたをするならmallocが使える", \t -> do
	itext t 1 "malloc :: Storable a => IO (Ptr a)", \t -> do
	text t "* しかし、その場合明示的なfreeが必要になる", \t -> do
	text t "* より安全なやりかたはallocaを使う方法", \t -> do
	itext t 1 "alloca :: Storable a => (Ptr a -> IO b) -> IO b", \t -> do
	text t "* allocaの典型的な使いかた", \t -> do
	itext t 1 "alloca $ \\ptr -> ...", \t -> do
	itext t 1 "- 確保されたメモリのアドレスがptrに束縛される", \t -> do
	text t "* 確保されたメモリに値を入れるのにはpokeを使う", \t -> do
	itext t 1 "poke :: Storable a => Ptr a -> a -> IO ()"
 ]

simpleValueSummary :: Page
simpleValueSummary = [\t -> do
	writeTopTitle t "単純な値へのポインタ(まとめ)"
	text t "", \t -> do
	text t "* Ptr aからaの値を得るのはpeek", \t -> do
	itext t 1 "peek :: Storable a => Ptr a -> IO a", \t -> do
	text t "* Ptr aに値を書き込むにはpoke", \t -> do
	itext t 1 "poke :: Storable a => Ptr a -> a -> IO ()", \t -> do
	text t "* メモリのアロケーションにはallocaを使う", \t -> do
	itext t 1 "alloca :: (Ptr a -> IO b) -> IO b"
 ]

array :: Page
array = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* Ptr aをaの配列へのポインタと考えることができる", \t -> do
	text t "* 気温の例", \t -> do
	itext t 1 "- 1月から12月までの月毎の平均気温のリスト", \t -> do
	itext t 1 "- 気温は10倍した整数で表現"
 ]

array2 :: Page
array2 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* ヘッダファイル", \t -> do
	itext t 1 "% cat temp.h"
	itext t 1 "int *get_temp(void);", \t -> do
	text t "* Cのコード", \t -> do
	itext t 1 "% cat temp.c"
	itext t 1 "#include \"temp.h\""
	itext t 1 "int temps[] = {"
	itext t 2 "24, 32, 67, 127, 182, 210,"
	itext t 2 "261, 286, 248, 173, 101, 47 };"
	itext t 1 "int *get_temps(void) { return temps; }"
 ]

array3 :: Page
array3 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* 比較のためのCのmain関数", \t -> do
	itext t 1 "#include <stdio.h>"
	itext t 1 "#include \"temp.h\""
	itext t 1 "int main (int argc, char *argv[]) {"
	itext t 2 "int i, *ts = get_temps();"
	itext t 2 "for (i = 0; i < 12; i++) {"
	itext t 3 "printf(\"%2d: %d\\n\", i + 1, ts[i]); }"
	itext t 2 "return 0; }"
 ]

array4 :: Page
array4 = [\t -> do
	writeTopTitle t "配列へのポインタ", \t -> do
	text t "* Haskellから使う例", \t -> do
	itext t 0 "import Foreign.C.Types"
	itext t 0 "import Foreign.Ptr"
	itext t 0 "import Foreign.Storable"
	itext t 0 "import Control.Monad"
	itext t 0 "foreign import ccall \"temp.h get_temps\""
	itext t 1 "c_getTemps :: Ptr CInt"
	itext t 0 "main :: IO ()"
	itext t 0 "main = do"
	itext t 1 "forM_ [0 .. 11] $ \\i -> do"
	itext t 2 "tmp <- peekElemOff c_getTemps i"
	itext t 2 "putStrLn $ show (i + 1) ++ \": \" ++ show tmp"
 ]

array5 :: Page
array5 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* オフセットを指定して値を取り出す", \t -> do
	itext t 1 "peekElemOff :: Storable a => Ptr a -> Int -> IO a", \t -> do
	text t "* この関数を使えばPtr aを配列として扱うことができる"
 ]

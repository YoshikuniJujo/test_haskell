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
	array, array2, array3, array4, array5, array6, array7, array8, array9,
	array10, array11,
	arraySummary,
	structure, structure2, structure3, structure4, structure5, structure6,
	structure7, structure8, structure9, structure10, structure11, structure12,
	structure13, structure14,
	structureSummary,
	storable, storable2, storable3, storable4, storable5, storable6,
	storable7, storable8, storable9, storable10, storable11, storable12,
	storable13, storable14, storable15, storable16,
	storableSummary, summary
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
	itext t 1 "message_type = n; }"
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
	text t "* この関数を使えばPtr aを配列として", \t -> do
	itext t 1 "- そのひとつひとつの要素を取り出すことができる"
 ]

array6 :: Page
array6 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* 次はHaskell側で作った配列をCの関数に渡す例", \t -> do
	text t "* ターミナルにグラフを書く例", \t -> do
	text t "* ヘッダファイル", \t -> do
	itext t 1 "% cat graph.h"
	itext t 1 "void mk_graph(int *, int);"
 ]

array7 :: Page
array7 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* Cのコード", \t -> do
	itext t 1 "% cat graph.c"
	itext t 1 "# include <stdio.h>"
	itext t 1 "# include \"graph.h\""
	itext t 1 "void mk_graph(int *dat, int n) {"
	itext t 2 "int i, j;"
	itext t 2 "for (i = 0; i < n; i++) {"
	itext t 3 "for (j = 0; j < dat[i]; j ++)"
	itext t 4 "printf(\"*\");"
	itext t 3 "printf(\"\\n\"); }"
	itext t 1 "}"
 ]

array8 :: Page
array8 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* 比較のためのCのmain関数", \t -> do
	itext t 1 "#include \"graph.h\""
	itext t 1 "int main(int argc, char *argv[]) {"
	itext t 2 "int sample[] = {"
	itext t 3 "2, 3, 4, 13, 18, 21,"
	itext t 3 "26, 29, 25, 17, 10, 5 };"
	itext t 2 "mk_graph(sample, 12);"
	itext t 2 "return 0;"
	itext t 1 "}"
 ]

array9 :: Page
array9 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* Haskellから使うには", \t -> do
	itext t 1 "import Foreign.C.Types"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "import Foreign.Storable"
	itext t 1 "import Foreign.Marshal.Array"
	itext t 1 "import Control.Monad"
	itext t 1 "foreign import ccall \"mk_graph\""
	itext t 2 "c_mkGraph :: Ptr CInt -> CInt -> IO ()"
	itext t 2 "(続く)"
 ]

array10 :: Page
array10 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	itext t 0 "main :: IO ()"
	itext t 0 "main = allocaArray 12 $ \\ptr -> do"
	itext t 1 "forM_ (zip [0 ..] ["
	itext t 3 "2, 3, 7, 13, 18, 21,"
	itext t 3 "26, 29, 25, 17, 10, 5]) $ \\(i, d) ->"
	itext t 2 "pokeElemOff ptr i d"
	itext t 1 "c_mkGraph ptr 12"
 ]

array11 :: Page
array11 = [\t -> do
	writeTopTitle t "配列へのポインタ"
	text t "", \t -> do
	text t "* 配列の要素数分のメモリを確保して", \t -> do
	text t "* それぞれの要素を書き込んだうえで", \t -> do
	text t "* ポインタの値をCの関数に送っている"
 ]

arraySummary :: Page
arraySummary = [\t -> do
	writeTopTitle t "配列へのポインタ(まとめ)"
	text t "", \t -> do
	text t "* 配列の特定の場所の値を読み込む", \t -> do
	itext t 1 "peekElemOff :: Storable a => Ptr a -> Int -> IO a", \t -> do
	text t "* 配列の特定の場所への書き込み", \t -> do
	itext t 1 "pokeElemOff :: Storable a =>"
	itext t 2 "Ptr a -> Int -> a -> IO ()", \t -> do
	text t "* 要素の数分だけのメモリを確保する", \t -> do
	itext t 1 "allocaArray :: Storable a =>"
	itext t 2 "Int -> (Ptr a -> IO b) -> IO b"
 ]

structure :: Page
structure = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* 構造体へのポインタを扱うことができる", \t -> do
	text t "* ただし、その構造体の低レベルな構造の知識が必要", \t -> do
	itext t 1 "- パディング等についても知っている必要がある", \t -> do
	itext t 1 "- できるコードは処理系依存になってしまう", \t -> do
	text t "* これを解決するためにはhsc2hsが必要", \t -> do
	itext t 1 "- hsc2hsについては次々回の講義で扱う", \t -> do
	text t "* 今回はhsc2hsが作成するコードを理解するために", \t -> do
	itext t 1 "- 低レベルな詳細をコードに入れてしまおう", \t -> do
	itext t 1 "- 可搬性の低いコードとなる", \t -> do
	text t "* より理想的なコードはhsc2hsを学ぶときに"
 ]

structure2 :: Page
structure2 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* 人の年齢、身長、体重を収納する構造体", \t -> do
	text t "* 太郎さんのデータを取得する例", \t -> do
	text t "* ヘッダファイル", \t -> do
	itext t 1 "% cat human.h"
	itext t 1 "typedef struct human {"
	itext t 2 "int age;"
	itext t 2 "double height;"
	itext t 2 "double weight;"
	itext t 1 "} human;"
	itext t 1 "human *get_tarou(void);"
 ]

structure3 :: Page
structure3 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* Cのコード", \t -> do
	itext t 1 "% cat human.c"
	itext t 1 "#include \"human.h\""
	itext t 1 "static human tarou = { 35, 182.5, 75 };"
	itext t 1 "human *get_tarou(void) {"
	itext t 2 "return &tarou;"
	itext t 1 "}"
 ]

structure4 :: Page
structure4 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* 比較のためのCのmain関数", \t -> do
	itext t 1 "#include <stdio.h>"
	itext t 1 "#include \"human.h\""
	itext t 1 "int main(int argc, char *argv[]) {"
	itext t 2 "human *t = get_tarou();"
	itext t 2 "printf(\"太郎: %d歳 %3.1fcm %2.1fkg\\n\","
	itext t 3 "t->age, t->height, t->weight);"
	itext t 2 "return 0;"
	itext t 1 "}"
 ]

structure5 :: Page
structure5 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* Haskellから使う", \t -> do
	itext t 1 "import Foreign.C.Types"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "import Foreign.Storable"
	itext t 1 "data Human"
	itext t 1 "foreign import ccall \"human.h get_tarou\""
	itext t 2 "c_getTarou :: Ptr Human"
	itext t 2 "(続く)"
 ]

structure6 :: Page
structure6 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	itext t 0 "main :: IO ()"
	itext t 0 "main = do"
	itext t 1 "let t = c_getTarou"
	itext t 1 "age <- peekByteOff t 0 :: IO CInt"
	itext t 1 "height <- peekByteOff t 4 :: IO CDouble"
	itext t 1 "weight <- peekByteOff t 12 :: IO CDouble"
	itext t 1 "putStrLn $ \"太郎 \" ++ show age ++ \"歳 \" ++"
	itext t 2 "show height ++ \"cm \" ++ show weight ++ \"kg\""
 ]

structure7 :: Page
structure7 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* 与えられたポインタから指定のバイト数進み値を取得する", \t -> do
	itext t 1 "peekByteOff :: Storable a => Ptr b -> Int -> IO a", \t -> do
	text t "* 構造体におけるそれぞれの要素の位置を", \t -> do
	itext t 1 "- ハードコーディングしてしまっている", \t -> do
	itext t 1 "- C言語からの情報を得られないのでしかたない", \t -> do
	itext t 1 "- 後述のhsc2hsを使えばこれを避けることができる"
 ]

structure8 :: Page
structure8 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* 構造体の値をHaskell側で作成してわたす場合", \t -> do
	text t "* トランプのスートとランクを表示する例"
 ]

structure9 :: Page
structure9 = [\t -> do
	writeTopTitle t "構造体へのポインタ", \t -> do
	text t "* ヘッダファイル", \t -> do
	itext t 1 "% cat trump.h"
	itext t 1 "typedef enum SUIT {"
	itext t 2 "Spade, Heart, Diamond, Club } SUIT;"
	itext t 1 "typedef struct card {"
	itext t 2 "SUIT suit;"
	itext t 2 "int rank;"
	itext t 1 "} card;"
	itext t 1 "void print_card(card *);", \t -> do
	text t "* Cのコード", \t -> do
	itext t 1 "% cat trump.c"
 ]

structure10 :: Page
structure10 = [\t -> do
	writeTopTitle t "構造体へのポインタ", \t -> do
	itext t 1 "#include <stdio.h>"
	itext t 1 "#include <string.h>"
	itext t 1 "#include \"trump.h\""
	itext t 1 "void print_card(card *c) {"
	itext t 2 "char suit_name[10], rank_name[10];"
	itext t 2 "switch (c-> suit) {"
	itext t 2 "case Spade:"
	preLine t
	itext t 4 "strcpy(suit_name, \"Spade\");"
	itext t 4 "break;"
	itext t 2 "case Heart:"
	itext t 2 "..."
	itext t 2 "default:"
	preLine t
	itext t 4 "strcpy(suit_name, \"\");"
	itext t 2 "}"
 ]

structure11 :: Page
structure11 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	itext t 1 "switch (c->rank) {"
	itext t 1 "case 1:"
	preLine t
	itext t 3 "strcpy(rank_name, \"Ace\");"
	itext t 3 "break;"
	itext t 1 "case 11:"
	itext t 1 "..."
	itext t 1 "default:"
	preLine t
	itext t 3 "sprintf(rank_name, \"%d\", c->rank);"
	itext t 1 "}"
	itext t 1 "printf(\"%s %s\\n\", suit_name, rank_name);"
	itext t 0 "}"
 ]

structure12 :: Page
structure12 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* 比較のためのCのmain関数", \t -> do
	itext t 1 "#include \"trump.h\""
	itext t 1 "int main(int argc, char *argv[]) {"
	itext t 2 "card c = { Diamond, 11 };"
	itext t 2 "print_card(&c):"
	itext t 2 "return 0;"
	itext t 1 "}"
 ]

structure13 :: Page
structure13 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	text t "* Haskellから使う"
	itext t 1 "import Foreign.C.Types"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "import Foreign.Marshal"
	itext t 1 "import Foreign.Storable"
	itext t 1 "data Card"
	itext t 1 "foreign import ccall \"trump.h print_card\""
	itext t 2 "c_printCard :: Ptr Card -> IO ()"
 ]

structure14 :: Page
structure14 = [\t -> do
	writeTopTitle t "構造体へのポインタ"
	text t "", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = allocaBytes 4 $ \\pc -> do"
	itext t 2 "pokeByteOff pc 0 (2 :: CInt)"
	itext t 2 "pokeByteOff pc 4 (11 :: CInt)"
	itext t 2 "c_printCard pc", \t -> do
	text t "* allocaBytesで構造体の大きさ分のメモリを確保", \t -> do
	text t "* pokeByteOffでそれぞれのスロットに値を代入", \t -> do
	text t "* マジックナンバーだらけ", \t -> do
	itext t 1 "- 列挙体の値、構造体の大きさ、スロットのオフセット", \t -> do
	itext t 1 "- hsc2hsを学ぶまではしかたない"
 ]

structureSummary :: Page
structureSummary = [\t -> do
	writeTopTitle t "構造体へのポインタ(まとめ)"
	text t "", \t -> do
	text t "* 構造体は今のところ低レベルの詳細が必要", \t -> do
	itext t 1 "- 構造体の大きさ、スロットのオフセット", \t -> do
	text t "* スロットを指定して値を取得", \t -> do
	itext t 1 "peekByteOff :: Storable a => Ptr b -> Int -> IO a", \t -> do
	text t "* スロットを指定して値を代入", \t -> do
	itext t 1 "pokeByteOff :: Storable a =>"
	itext t 2 "Ptr b -> Int -> a -> IO ()", \t -> do
	text t "* 大きさを指定してメモリを確保", \t -> do
	itext t 1 "allocaBytes :: Int -> (Ptr a -> IO b) -> IO b"
 ]

storable :: Page
storable = [\t -> do
	writeTopTitle t "Storable", \t -> do
	text t "* peekやpokeで読み出し、書き込みができるのは", \t -> do
	itext t 1 "- Storableクラスのインスタンス", \t -> do
	text t "* Storableクラスの定義は以下のようになっている", \t -> do
	itext t 1 "class Storable a where"
	itext t 2 "sizeOf :: a -> Int"
	itext t 2 "alignment :: a -> Int"
	itext t 2 "peekElemOff :: Ptr a -> Int -> IO a"
	itext t 2 "pokeElemOff :: Ptr a -> Int -> a -> IO ()"
	itext t 2 "peekByteOff :: Ptr a -> Int -> IO a"
	itext t 2 "pokeByteOff :: Ptr a -> Int -> a -> IO ()"
	itext t 2 "peek :: Ptr a -> IO a"
	itext t 2 "poke :: Ptr a -> a -> IO ()"
 ]

storable2 :: Page
storable2 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* クラスメソッドをすべて定義する必要はない", \t -> do
	text t "* 定義する必要があるのは", \t -> do
	itext t 1 "- sizeOf, alignmentは定義する必要がある", \t -> do
	itext t 1 "- peek, peekElemOff, peekByteOffのどれかひとつ", \t -> do
	itext t 1 "- poke, pokeElemOff, pokeByteOffのどれかひとつ", \t -> do
	text t "* sizeOfは必要なメモリのサイズをbyte単位で返す", \t -> do
	itext t 1 "- a -> Intという型だが引数のaは型を決めるためだけ", \t -> do
	itext t 1 "- 値は実際には使わない", \t -> do
	text t "* alignmentはアドレスがその倍数であること示す値", \t -> do
	itext t 1 "- 引数の扱いに関してはsizeOfと同様"
 ]

storable3 :: Page
storable3 = [\t -> do
	writeTopTitle t "アラインメントについて"
	text t "", \t -> do
	text t "* ここでアラインメントについてすこし学ぶ必要がある", \t -> do
	text t "* メモリは必ずしも1byteずつアクセスされるとはかぎらない", \t -> do
	text t "* 32ビットCPUではたいてい4byteずつのブロックでアクセス", \t -> do
	text t "* よって1byteのデータならどこに置いてもかまわないが", \t -> do
	text t "* 2byte以上のデータには制約がかかる", \t -> do
	itext t 1 "- 2byteのデータであれば2の倍数の番地に", \t -> do
	itext t 1 "- 4byteのデータであれば4の倍数の番地に", \t -> do
	text t "* そうしないとCPUによって", \t -> do
	itext t 1 "- アクセスが遅くなる", \t -> do
	itext t 1 "- エラーとなる"
 ]

storable4 :: Page
storable4 = [\t -> do
	writeTopTitle t "アラインメントについて", \t -> do
	text t "* アラインメントはデータ型とCPUによって定まる", \t -> do
	text t "* 基本型のアラインメントは以下のようになる", \t -> do
	text t "* 一度にアクセスするデータの大きさをn byteとし", \t -> do
	text t "* その型のサイズをx byteとすると、アラインメントは", \t -> do
	text t "* x <= nのときには", \t -> do
	itext t 1 "- xより大きいnの約数のうち最小のものとなり", \t -> do
	text t "* x > nのときにはnとなる", \t -> do
	text t "* 複合データ型の場合は", \t -> do
	itext t 1 "- その型に含まれる型のアラインメントの最小公倍数", \t -> do
	text t "* ほとんどのCPUのデータバスが2の冪乗だと思うので", \t -> do
	itext t 1 "- 実用的には最小公倍数の代わりに最大値が使える"
 ]

storable5 :: Page
storable5 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* 自作の型をStorableにしてみる", \t -> do
	itext t 1 "data Shape"
	itext t 2 "= Circle CDouble"
	itext t 2 "| Rectangle CDouble CDouble", \t -> do
	text t "* Storableクラスのインスタンスにする", \t -> do
	text t "* sizeOf, alignment, peek, pokeを定義しよう", \t -> do
	text t "* 2つの数x, aを取りxより大きい最小のaの倍数を返す関数", \t -> do
	itext t 1 "align :: Int -> Int -> Int"
	itext t 1 "x `align` a = (d + if m > 0 then 1 else 0) * a"
	itext t 2 "where (d, m) = x `divMod` a"
 ]

storable6 :: Page
storable6 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* 形の種類をCIntで表すことにする", \t -> do
	text t "* するとサイズは", \t -> do
	itext t 1 "- CIntのサイズをCDoubleのアラインメントに合わせる", \t -> do
	itext t 1 "- それにCDoubleのサイズを2倍した値を足す", \t -> do
	itext t 1 "sizeOf _ = sizeOf (undefined :: CInt) `align`"
	itext t 2 "alignment (undefined :: CDouble) +"
	itext t 2 "2 * sizeOf (undefined :: CDouble)", \t -> do
	text t "* アラインメントは各要素のアラインメントの最大値", \t -> do
	itext t 1 "alignment _ = max"
	itext t 2 "(alignment (undefined :: CInt))"
	itext t 2 "(alignment (undefined :: CDouble))"
 ]

storable7 :: Page
storable7 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* 値の取り出しは", \t -> do
	itext t 1 "- まずは形のタイプを取り出し", \t -> do
	itext t 1 "- そのタイプに応じて必要な値を取り出す"
 ]

storable8 :: Page
storable8 = [\t -> do
	writeTopTitle t "Storable", \t -> do
	itext t (-1) "peek ptr = do"
	itext t 0 "typ :: CInt <- peekByteOff ptr tOffset"
	itext t 0 "case typ of"
	itext t 1 "0 -> do"
	preLine t
	itext t 2.5 "r <- peekByteOff ptr rOffset"
	itext t 2.5 "return $ Circle r"
	itext t 1 "1 -> do"
	preLine t
	itext t 2.5 "w <- peekByteOff ptr wOffset"
	itext t 2.5 "h <- peekByteOff ptr hOffset"
	itext t 2.5 "return $ Rectangle w h"
	itext t 0 "where"
	preLine t
	itext t 1 "tOffset = 0"
	itext t 1 "rOffset = sizeOf (undefined :: CInt)"
	itext t 1 "wOffset = sizeOf (undefined :: CInt)"
	itext t 1 "hOffset = wOffset + sizeOf (undefined :: CDouble)"
 ]

storable9 :: Page
storable9 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* 値の書き込みは形のタイプと値を書き込めば良い", \t -> do
	itext t 1 "poke ptr (Circle r) = do"
	itext t 2 "pokeByteOff ptr tOffset (0 :: CInt)"
	itext t 2 "pokeByteOff ptr rOffset r"
	itext t 2 "where"
	preLine t
	itext t 3 "tOffset = 0"
	itext t 3 "rOffset = sizeOf (undefined :: CInt)"
 ]

storable10 :: Page
storable10 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	itext t 0 "poke ptr (Rectangle w h) = do"
	itext t 1 "pokeByteOff ptr tOffset (1 :: CInt)"
	itext t 1 "pokeByteOff ptr wOffset w"
	itext t 1 "pokeByteOff ptr hOffset h"
	itext t 1 "where"
	itext t 1 "tOffset = 0"
	itext t 1 "wOffset = sizeOf (undefined :: CInt)"
	itext t 1 "hOffset = wOffset + sizeOf (undefined :: CDouble)"
 ]

storable11 :: Page
storable11 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* これでShapeはpeekやpokeで値の出し入れができる", \t -> do
	text t "* Haskell側で作ったShapeをCの関数にわたす例を見ていこう", \t -> do
	text t "* ヘッダファイルを作る", \t -> do
	itext t 1 "% cat shape.h"
 ]

storable12 :: Page
storable12 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	itext t 1 "typedef struct Shape {"
	itext t 2 "int shape_type;"
	itext t 2 "union _shape_value {"
	itext t 3 "double radius;"
	itext t 3 "struct _width_height {"
	itext t 4 "double width;"
	itext t 4 "double height;"
	itext t 3 "} width_height;"
	itext t 2 "} shape_value;"
	itext t 1 "} Shape;"
	itext t 1 "void printShape(Shape *);"
 ]

storable13 :: Page
storable13 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* 形を表示する関数", \t -> do
	itext t 1 "% cat shape.c"
 ]

storable14 :: Page
storable14 = [\t -> do
	writeTopTitle t "Storable", \t -> do
	itext t 0 "void printShape(Shape *ptr) {"
	itext t 1 "switch (ptr->shape_type) {"
	itext t 1 "case 0:"
	itext t 2 "printf(\"Circle %f\\n\","
	itext t 3 "ptr->shape_value.radius);"
	itext t 2 "break;"
	itext t 1 "case 1:"
	itext t 2 "printf(\"Rectangle %f\\n\","
	itext t 3 "ptr->shape_value.width_height.width,"
	itext t 3 "ptr->shape_value.width_height.height);"
	itext t 2 "break;"
	itext t 1 "default:"
	itext t 0 "}"
 ]

storable15 :: Page
storable15 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	text t "* Haskellから使う", \t -> do
	itext t 1 "import Foreign.C.Types"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "import Foreign.Storable"
	itext t 1 "import Foreign.Marshal"
	itext t 1 ""
	itext t 1 "foreign import ccall \"print_shape\""
	itext t 2 "c_printShape :: Ptr Shape -> IO ()"
 ]

storable16 :: Page
storable16 = [\t -> do
	writeTopTitle t "Storable"
	text t "", \t -> do
	itext t 0 "main :: IO ()"
	itext t 0 "main = alloca $ \\ptr -> do"
	itext t 1 "poke ptr $ Circle 82.5"
	itext t 1 "c_printShape ptr"
	itext t 1 "poke ptr $ Rectangle 33 89"
	itext t 1 "c_printShape ptr"
 ]

storableSummary :: Page
storableSummary = [\t -> do
	writeTopTitle t "Storable(まとめ)"
	text t "", \t -> do
	text t "* Haskell側の値をCからも使うことができる", \t -> do
	text t "* そのためにはStorableクラスのインスタンスにする", \t -> do
	text t "* sizeOf, alignment, peek, pokeメソッドを定義すれば良い", \t -> do
	text t "* sizeOfやalignmentについて", \t -> do
	itext t 1 "- 適切なアラインメントを考慮する必要がある", \t -> do
	text t "* peekやpokeは", \t -> do
	itext t 1 "- 構成要素のそれぞれのpeek, pokeを使えば良い"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 配列ヘのポインタを扱う方法を見た", \t -> do
	text t "* 構造体へのポインタを扱う方法を見た", \t -> do
	itext t 1 "- 各要素の位置をバイトで指定した", \t -> do
	text t "* ポインタで示された位置にあるデータの取り出しを見た", \t -> do
	text t "* ポインタで示された位置へのデータの書き込みを見た", \t -> do
	text t "* Haskell側でメモリを確保して使う方法を見た"
 ]

import Lecture

subtitle :: String
subtitle = "第42回 C言語とのインターフェース1"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	haskellInC, haskellInC2, haskellInC3, haskellInC4, haskellInC5,
	haskellInC6, haskellInCSummary,
	cInHaskell, cInHaskell2, cInHaskell3, cInHaskell4,
	cstring, cstring2, cstring3, ctypes,
	ptr, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10,
	ptr11, ptr12, cInHaskellSummary,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 既存のパッケージで多くのことは可能", \t -> do
	text t "* 他言語のライブラリを使いたくなることがある", \t -> do
	itext t 1 "- それをするパッケージが存在しない", \t -> do
	itext t 1 "- それをするパッケージが気に入らない", \t -> do
	itext t 1 "- 等", \t -> do
	text t "* ghcではC言語とのあいだで以下のことができる", \t -> do
	itext t 1 "- HaskellからCの関数を使う", \t -> do
	itext t 1 "- CからHaskellの関数を使う", \t -> do
	itext t 1 "- Haskell関数のポインタをCの関数にわたす", \t -> do
	text t "* 今回の講義では前2者を扱う", \t -> do
	text t "* 関数ポインタによるCとのやりとりは次々回の講義で"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 他言語とのインターフェースをFFIと呼ぶ", \t -> do
	itext t 1 "- foreign function interfaceの略", \t -> do
	text t "* 今回の講義は2つの部分に分けることができる", \t -> do
	itext t 1 "1. C言語からHaskellの関数を使う", \t -> do
	itext t 1 "2. HaskellからC言語の関数を使う", \t -> do
	text t "* それでは1から始めよう"
 ]

haskellInC :: Page
haskellInC = [\t -> do
	writeTopTitle t "CからHaskellの関数を"
	text t "", \t -> do
	text t "* C言語のなかでHaskellの関数を呼ぶことができる", \t -> do
	text t "* 何でもいいが、前回のライプニッツ法の例を見ていこう"
 ]

haskellInC2 :: Page
haskellInC2 = [\t -> do
	writeTopTitle t "CからHaskellの関数を", \t -> do
	itext t 0 "% cat Pi.hs"
	itext t 0 "module Pi (pi) where"
	itext t 0 "import Prelude hiding (pi)"
	itext t 0 "import Foreign.C.Types"
	itext t 0 "foreign export ccall \"h_pi\" pi :: CUInt -> CDouble"
	itext t 0 "pi :: CUInt -> CDouble"
	itext t 0 "pi n = 4 * getPi4 0 1 n"
	itext t 0 "getPi4 :: CDouble -> CDouble -> CUInt -> CDouble"
	itext t 0 "getPi4 p _ 0 = p"
	itext t 0 "getPi4 p i n = let"
	itext t 1 "p' = p + recip i"
	itext t 1 "i' = negate $ i + 2 * signum i in"
	itext t 1 "p' `seq` i' `seq` getPi4 p' i' (n - 1)"
 ]

haskellInC3 :: Page
haskellInC3 = [\t -> do
	writeTopTitle t "CからHaskellの関数を"
	text t "", \t -> do
	text t "* ここに注目"
	itext t 1 "foreign export ccall \"h_pi\" pi :: CUInt -> CDouble"
	text t "* 一度これをコンパイルしておく", \t -> do
	itext t 1 "% ghc -c Pi.hs", \t -> do
	text t "* Pi_stub.hができる", \t -> do
	text t "* Cのコードは次のようになる"
 ]

haskellInC4 :: Page
haskellInC4 = [\t -> do
	writeTopTitle t "CからHaskellの関数を", \t -> do
	text t "% cat usePi.c"
	text t "#include <stdio.h>"
	text t "#include \"HsFFI.h\""
	text t "#ifdef __GLASGOW_HASKELL__"
	text t "#include \"Pi_stub.h\""
	text t "#endif"
	text t "int main (int argc, char *argv[]) {"
	itext t 1 "hs_init(&argc, &argv);"
	itext t 1 "printf(\"%1.10f\\n\", h_pi(10000000))"
	itext t 1 "hs_exit();"
	itext t 1 "return 0;"
	text t "}"
 ]

haskellInC5 :: Page
haskellInC5 = [\t -> do
	writeTopTitle t "CからHaskellの関数を", \t -> do
	text t "* HsFFI.hをincludeする", \t -> do
	itext t 1 "#include \"HsFFI.h\"", \t -> do
	text t "* ghcの場合にはPi_stub.hもinclude", \t -> do
	itext t 1 "#ifdef __GLASGOW_HASKELL__"
	itext t 1 "#include \"Pi_stub.h\""
	itext t 1 "#endif", \t -> do
	text t "* Haskellのruntimeを初期化", \t -> do
	itext t 1 "hs_init(&argc, &argv);", \t -> do
	text t "* exportした関数は普通に呼び出せば良い", \t -> do
	itext t 1 "h_pi(10000000)", \t -> do
	text t "* runtimeの終了", \t -> do
	itext t 1 "hs_exit();"
 ]

haskellInC6 :: Page
haskellInC6 = [\t -> do
	writeTopTitle t "CからHaskellの関数を"
	text t "", \t -> do
	text t "* コンパイル", \t -> do
	itext t 1 "% ghc -no-hs-main usePi.c Pi.hs -o usePi", \t -> do
	itext t 1 "- -no-hs-mainでHaskellのMain.mainがないことを示す", \t -> do
	text t "* 実行", \t -> do
	itext t 1 "% ./usePi"
	itext t 1 "3.1415925536"
 ]

haskellInCSummary :: Page
haskellInCSummary = [\t -> do
	writeTopTitle t "CからHaskellの関数を(まとめ)"
	text t "", \t -> do
	text t "* exportを宣言する", \t -> do
	itext t 1 "foreign export ccall \"c_name\" h_name :: Type", \t -> do
	text t "* ..._stub.hを作るためにコンパイルしておく", \t -> do
	itext t 1 "% ghc -c HsFile.hs", \t -> do
	text t "* CのコードにはHsFFI.hと..._stub.hをincludeする", \t -> do
	text t "* hs_initで初期化、hs_exitで終了", \t -> do
	itext t 1 "- その間でHaskellの関数が使える", \t -> do
	text t "* コンパイルするときは-no-hs-mainが必要"

 ]

cInHaskell :: Page
cInHaskell = [\t -> do
	writeTopTitle t "HaskellからCの関数を"
	text t "", \t -> do
	text t "* Cの関数をHaskellから使える", \t -> do
	itext t 1 "- つまりCにできることは何でもできるということ", \t -> do
	text t "* 最初の例", \t -> do
	itext t 0 "% cat add3.c"
	itext t 0 "int add3(int n) { return (n + 3); }", \t -> do
	itext t 0 "% cat useAdd3.hs"
	itext t 0 "import Foreign.C.Types"
	itext t 0 "foreign import ccall \"add3\" c_add3 :: CInt -> CInt"
	itext t 0 "main :: IO ()"
	itext t 0 "main = print $ c_add3 7"
 ]

cInHaskell2 :: Page
cInHaskell2 = [\t -> do
	writeTopTitle t "HaskellからCの関数を"
	text t "", \t -> do
	text t "* コンパイル、実行", \t -> do
	itext t 1 "% ghc useAdd3.hs add3.c", \t -> do
	itext t 1 "% ./useAdd3"
	itext t 1 "10", \t -> do
	text t "* cコードだけコンパイルしておくこともできる", \t -> do
	itext t 1 "% ghc -c add3.c", \t -> do
	itext t 1 "% ghci useAdd3.hs add3.o"
 ]

cInHaskell3 :: Page
cInHaskell3 = [\t -> do
	writeTopTitle t "HaskellからCの関数を"
	text t "", \t -> do
	text t "* 以下の行に注目", \t -> do
	itext t 0 "foreign import ccall \"add3\" c_add3 :: CInt -> CInt", \t -> do
	itext t 1 "- foreign import ccallに続けて", \t -> do
	itext t 1 "- Cの関数の名前、Haskellで使うときの名前、型", \t -> do
	text t "* ヘッダファイルについて", \t -> do
	itext t 1 "% cat add3.h"
	itext t 1 "int add3(int);", \t -> do
	itext t 1 "- \"add3\"の代わりに\"add3.h add3\"とするのは良い習慣", \t -> do
	itext t 1 "- ghcでは無視されるが他の処理系では必要になるかも"
 ]

cInHaskell4 :: Page
cInHaskell4 = [\t -> do
	writeTopTitle t "HaskellからCの関数を"
	text t "", \t -> do
	text t "* IOを使う例", \t -> do
	itext t 0 "% cat sayHello.c"
	itext t 0 "#include <stdio.h>"
	itext t 0 "void sayHello(void) { printf(\"Hello, world!\\n\"); }", \t -> do
	itext t 0 "% cat useSayHello.hs"
	itext t 0 "import Foreign.C.Types"
	itext t 0 "foreign import ccall \"sayHello\" c_sayHello :: IO ()"
	itext t 0 "main :: IO ()"
	itext t 0 "main = c_sayHello"
 ]

cstring :: Page
cstring = [\t -> do
	writeTopTitle t "HaskellからCの関数を", \t -> do
	text t "* Cの文字列を使う", \t -> do
	itext t 0 "% cat sayHelloTo.c"
	itext t 0 "#include <stdio.h>"
	itext t 0 "void sayHelloTo(char *name) {"
	itext t 1 "printf(\"Hello, %s!\\n\", name); }", \t -> do
	itext t 0 "% cat useSayHelloTo.hs"
	itext t 0 "import Foreign.C.String"
	itext t 0 "foreign import ccall \"sayHelloTo\" c_sayHelloTo ::"
	itext t 5 "CString -> IO ()"
	itext t 0 "main :: IO ()"
	itext t 0 "main = do"
	itext t 1 "cstr <- newCString \"Yoshikuni\""
	itext t 1 "c_sayHelloTo cstr"
 ]

cstring2 :: Page
cstring2 = [\t -> do
	writeTopTitle t "HaskellからCの関数を", \t -> do
	text t "* HaskellのStringはCharのリスト", \t -> do
	text t "* Cの文字列はバイト配列", \t -> do
	text t "* 相互の変換は以下の関数でできる", \t -> do
	itext t 1 "newCString :: String -> IO CString", \t -> do
	itext t 1 "peekCString :: CString -> IO String", \t -> do
	text t "* newCStringはメモリの確保を行う", \t -> do
	arrowIText t 1 "freeで解放してやる必要がある", \t -> do
	arrowIText t 1 "例外が起きても解放する必要がある", \t -> do
	arrowIText t 1 "例外を捕捉して...", \t -> do
	text t "* そこらへんをきちんとやってくれる関数", \t -> do
	itext t 0 "withCString :: String -> (CString -> IO a) -> IO a", \t -> do
	text t "* newCStringの代わりにwithCStringを使ったほうが良い"
 ]

cstring3 :: Page
cstring3 = [\t -> do
	writeTopTitle t "HaskellからCの関数を"
	text t "", \t -> do
	text t "* withCStringを使って書き直す", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = withCString \"Yoshikuni\" $ \\cstr -> do"
	itext t 2 "c_sayHelloTo cstr", \t -> do
	text t "* CStringの定義", \t -> do
	itext t 1 "type CString = Ptr CChar"
 ]

ctypes :: Page
ctypes = [\t -> do
	writeTopTitle t "HaskellからCの関数を"
	text t "", \t -> do
	text t "* 引数や返り値として以下の型が使える", \t -> do
	itext t 1 "CChar, CUChar, CInt, CUInt, CLong, CULong,"
	itext t 1 "CFloat, CDouble", \t -> do
	text t "* 上に加えてPtr型が使える", \t -> do
	itext t 1 "Ptr a", \t -> do
	text t "* Ptr型はCの配列や構造体を表現することができる"
 ]

ptr :: Page
ptr = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* Ptr型の使いかたは以下の2通りある", \t -> do
	itext t 1 "- C言語の関数内でのみなかをのぞく", \t -> do
	itext t 1 "- Haskell側からも中身を見る", \t -> do
	text t "* 後者はさらに3通りに分けられる", \t -> do
	itext t 1 "- そのままの値を扱う", \t -> do
	itext t 1 "- 配列として扱う", \t -> do
	itext t 1 "- 構造体として扱う"
 ]

ptr2 :: Page
ptr2 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* Haskellから値をいじらない場合", \t -> do
	text t "* Cの関数で作った構造をCの関数にわたすようなとき", \t -> do
	text t "* 名前を表示する例を見てみよう", \t -> do
	itext t 1 "% cat name.h"
	itext t 1 "typedef struct name {"
	itext t 2 "char first_name[20];"
	itext t 2 "char last_name[20]; } name;"
	itext t 1 "name *mkName(char *, char *);"
	itext t 1 "void printName(name *);"
	itext t 1 "void freeName(name *);"
 ]

ptr3 :: Page
ptr3 = [\t -> do
	writeTopTitle t "Ptr型を使う", \t -> do
	itext t 0 "% cat name.c"
	itext t 0 "#include <string.h>"
	itext t 0 "#include <stdio.h>"
	itext t 0 "#include \"name.h\""
	itext t 0 "name *mkName(char *first, char *last) {"
	itext t 1 "name *n = calloc(1, sizeof(name));"
	itext t 1 "strncpy(n->first_name, first, 19);"
	itext t 1 "strncpy(n->last_name, last, 19);"
	itext t 1 "return n; }"
	itext t 0 "void printName(name *n) {"
	itext t 1 "printf(\"%s %s\\n\", n->first_name, n->last_name); }"
	itext t 0 "void freeName(name *n) { free(n); }"
 ]

ptr4 :: Page
ptr4 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* 比較のためCから使う例を見てみる", \t -> do
	itext t 1 "#include <stdio.h>"
	itext t 1 "#include \"name.h\""
	itext t 1 "int main(int argc, char *argv) {"
	itext t 2 "name *n = mkName(\"Yoshikuni\", \"Jujo\");"
	itext t 2 "printName(n);"
	itext t 2 "freeName(n); }"
 ]

ptr5 :: Page
ptr5 = [\t -> do
	writeTopTitle t "Ptr型を使う", \t -> do
	text t "* HaskellのuseName.hsがどうなるか見てみよう", \t -> do
	itext t 1 "import Foreign.C.String"
	itext t 1 "import Foreign.Ptr"
	itext t 1 "import Control.Applicative"
	itext t 1 "newtype Name = Name (Ptr Name)"
	itext t 1 "foreign import ccall \"name.h mkName\""
	itext t 2 "c_mkName :: CString -> CString -> IO (Ptr Name)"
	itext t 1 "foreign import ccall \"name.h printName\""
	itext t 2 "c_printName :: Ptr Name -> IO ()"
	itext t 1 "foreign import ccall \"name.h freeName\""
	itext t 2 "c_freeName :: Ptr Name -> IO ()"
	itext t 2 "(続く)"
 ]

ptr6 :: Page
ptr6 = [\t -> do
	writeTopTitle t "Ptr型を使う", \t -> do
	itext t 0 "mkName :: String -> String -> IO Name"
	itext t 0 "mkName fn ln = withCString fn $ \\cfn ->"
	itext t 1 "withCString ln $ \\cln ->"
	itext t 2 "Name <$> c_mkName cfn cln"
	itext t 0 "printName, freeName :: Name -> IO ()"
	itext t 0 "printName (Name pn) = c_printName pn"
	itext t 0 "freeName (Name pn) = c_freeName pn"
	itext t 0 "main :: IO ()"
	itext t 0 "main = do"
	itext t 1 "n <- mkName \"Yoshikuni\" \"Jujo\""
	itext t 1 "printName n"
	itext t 1 "freeName n"
 ]

ptr7 :: Page
ptr7 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* Name型を作成した", \t -> do
	itext t 1 "newtype Name = Name (Ptr Name)", \t -> do
	text t "* これはイディオムになっているが以下と同じ", \t -> do
	itext t 1 "data NameType"
	itext t 1 "newtype Name = Name (Ptr NameType)", \t -> do
	text t "* Haskellの型を使うラッパー関数を作成した", \t -> do
	itext t 1 "c_mkName :: CString -> CString -> IO (Ptr Name)", \t -> do
	arrowIText t 1 "mkName :: String -> String -> IO Name"
 ]

ptr8 :: Page
ptr8 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* main関数のは以下のようになっている", \t -> do
	itext t 1 "n <- mkName \"Yoshikuni\" \"Jujo\""
	itext t 1 "printName n"
	itext t 1 "freeName n", \t -> do
	text t "* わかりやすさのためにこうしたが本当は問題がある", \t -> do
	itext t 1 "- printNameで例外が発生するとfreeNameが行われない", \t -> do
	text t "* より良い定義はこうなる", \t -> do
	itext t 1 "bracket (mkName \"Yoshikuni\" \"Jujo\") freeName"
	itext t 2 "printName"
 ]

ptr9 :: Page
ptr9 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* 今の実装では明示的にメモリをfreeしている", \t -> do
	text t "* GC実行時にメモリを解放させることもできる", \t -> do
	text t "* importするモジュールはもとのコードに加えて", \t -> do
	itext t 1 "import Foreign.ForeignPtr", \t -> do
	text t "* コードの変更部分は", \t -> do
	itext t 1 "foreign import ccall \"name.h &freeName\""
	itext t 2 "c_freeName :: FinalizerPtr Name"
	itext t 2 "(続く)"
 ]

ptr10 :: Page
ptr10 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	itext t 0 "mkName fn ln = withCString fn $ \\cfn ->"
	itext t 1 "withCString ln $ \\cln ->"
	itext t 2 "Name <$> (c_mkName cfn cln >>="
	itext t 3 "newForeignPtr c_freeName)"
	itext t 0 ""
	itext t 0 "printName (Name pn) = withForeignPtr pn c_printName"
	itext t 0 ""
	itext t 0 "main = do"
	itext t 1 "n <- mkName \"Yoshikuni\" \"Jujo\""
	itext t 1 "printName n"
 ]

ptr11 :: Page
ptr11 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* freeNameの関数ポインタをimportしている", \t -> do
	itext t 1 "foreign import ccall \"name.h &freeName\""
	itext t 2 "c_freeName :: FinalizerPtr Name", \t -> do
	text t "* ポインタにfinalizerを関係づける", \t -> do
	itext t 1 "c_mkName cfn cln >>= newForeignPtr c_freeName", \t -> do
	text t "* withForeignPtr内のIOではそのPtrのGCは行われない", \t -> do
	itext t 1 "withForeignPtr pn c_printName"
 ]

ptr12 :: Page
ptr12 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* ForeignPtrをいつ使うか?", \t -> do
	text t "* ファイナライザが動くのはGCのときなので", \t -> do
	itext t 1 "- メモリが少なくなるまでは動く保証はない", \t -> do
	itext t 1 "- メモリ以外の資源が枯渇しても解放されない", \t -> do
	text t "* メモリ以外の資源を確保するようなPtrには使えない", \t -> do
	itext t 1 "- オープンしたファイルをクローズする必要がある等", \t -> do
	text t "* メモリのみを確保・解放するようなPtrには有用"
 ]

ptr13 :: Page
ptr13 = [\t -> do
	writeTopTitle t "Ptr型を使う"
	text t "", \t -> do
	text t "* "
 ]

ptrX :: Page
ptrX = [\t -> do
	text t "* ForeignPtrの使いどころは確保したのがメモリのみのとき", \t -> do
	text t "* それ以外に確保したリソースがあったりするときは問題に", \t -> do
	text t "* 必ずしなければならない後処理にも使えない"
 ]

cInHaskellSummary :: Page
cInHaskellSummary = [\t -> do
	writeTopTitle t "HaskellからCの関数を(まとめ)"
	text t "", \t -> do
	text t "* Cの関数をimportする", \t -> do
	itext t 1 "foreign import ccall \"cname\" hsname :: Type", \t -> do
	text t "* Cの関数は普通の関数としてもIOとしても使える", \t -> do
	text t "* 引数や返り値の型はForeign.C.Types内のものを使うと安全", \t -> do
	text t "* Ptr型を使うとC言語のいろいろな構造を使うことができる", \t -> do
	itext t 1 "- 今回はCのなかでのみ値を扱う場合のみを学んだ", \t -> do
	text t "* C言語側で確保した資源は明示的に解放する必要がある", \t -> do
	itext t 1 "- bracket等を使うと例外時にも対応できる", \t -> do
	itext t 1 "- 資源がメモリのみの場合はForeignPtrが使える"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* CからHaskellの関数を使う方法を学んだ", \t -> do
	itext t 1 "- hs_initとhs_exitが必要となる", \t -> do
	text t "* HaskellからCの関数を使う方法を学んだ", \t -> do
	text t "* 整数等の普通の値を扱う場合には特に問題はない", \t -> do
	text t "* Ptr型によるCとポインタを介したやりとりができる", \t -> do
	text t "* 確保した資源の解放がひとつの問題となる", \t -> do
	itext t 1 "- bracketを使う方法とForeignPtrを使う方法とがある", \t -> do
	text t "* 次回はPtr型をHaskell側から扱う方法を学ぶ"
 ]

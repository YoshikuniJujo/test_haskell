import Lecture

subtitle :: String
subtitle = "第42回 C言語とのやりとり"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	haskellInC, haskellInC2, haskellInC3, haskellInC4, haskellInC5,
	haskellInC6, haskellInCSummary
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
	text t "* 関数ポインタによるCとのやりとりは次回の講義で"
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

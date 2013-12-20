import Lecture

subtitle :: String
subtitle = "第44回 C言語とのインターフェース3"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	wrapperPrelude, wrapper1, wrapper2, wrapper3, wrapperSummary,
	stablePtrPrelude, stablePtr1, stablePtr2, stablePtr3, stablePtrSummary,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今回はwrapperとStablePtrについて説明する", \t -> do
	text t "* wrapperはHaskellの関数を関数ポインタにする方法", \t -> do
	itext t 1 "- できた関数ポインタはCの関数にわたすことができる", \t -> do
	text t "* StablePtrはHaskellの値をそのままの形でポインタにする", \t -> do
	itext t 1 "- なかの値はHaskell内でしか扱えない", \t -> do
	itext t 1 "- C言語内でHaskell関数を呼び出し、返ってきた値を", \t -> do
	itext t 1 "- そのままの形でHaskell関数にわたす"
 ]

wrapperPrelude :: Page
wrapperPrelude = [\t -> do
	writeTopTitle t "wrapper(はじめに)"
	text t "", \t -> do
	text t "* Haskellの関数やIOはStorableのインスタンスではない", \t -> do
	text t "* peekやpokeでPtrを作ってCの関数にわたす", \t -> do
	itext t 1 "- ということはできない", \t -> do
	text t "* Haskellの関数を関数ポインタとしてCにわたしたいとき", \t -> do
	itext t 1 "- たとえば以下のような例を考える", \t -> do
	itext t 1 "- 与えられた関数に1, 2, 3を適用した結果を表示する"
 ]

wrapper1 :: Page
wrapper1 = [\t -> do
	writeTopTitle t "wrapper"
	text t "", \t -> do
	text t "* Cのヘッダファイルは以下のようになる", \t -> do
	itext t 1 "void print_apply_123(int (*) (int));", \t -> do
	text t "* Cのコード", \t -> do
	itext t 1 "#include <stdio.h>"
	itext t 1 "#include \"apply_123.h\""
	itext t 1 "void print_apply_123(int (*f)(int)) {"
	itext t 2 "int i;"
	itext t 2 "for (i = 0; i < 3; i ++) {"
	itext t 3 "printf(\"%d\\n\", f(i + 1));"
	itext t 2 "}"
	itext t 1 "}"
 ]

wrapper2 :: Page
wrapper2 = [\t -> do
	writeTopTitle t "wrapper"
	text t "", \t -> do
	text t "* Haskellから使う場合", \t -> do
	itext t 0.5 "import Foreign.Ptr"
	itext t 0.5 "foreign import ccall \"apply_123.h print_apply_123\""
	itext t 1.4 "c_printApply123 :: FunPtr (Int -> Int) -> IO ()"
	itext t 0.5 "foreign import ccall \"wrapper\""
	itext t 1.4 "wrap :: (Int -> Int) -> IO (FunPtr (Int -> Int))"
	itext t 0.5 "main :: IO ()"
	itext t 0.5 "main = do"
	itext t 1.4 "c_printApply123 =<< wrap (\\x -> x * x)"
 ]

wrapper3 :: Page
wrapper3 = [\t -> do
	writeTopTitle t "wrapper"
	text t "", \t -> do
	text t "* ここに注目", \t -> do
	itext t 0.5 "foreign import ccall \"wrapper\""
	itext t 1.4 "wrap :: (Int -> Int) -> IO (FunPtr (Int -> Int))", \t -> do
	text t "* 仮想的なCの関数wrapperがあり", \t -> do
	itext t 1 "- その関数はHaskellの関数から関数ポインタを作る", \t -> do
	itext t 1 "- wrapperの型は[fun] -> IO (FunPtr [fun])となる", \t -> do
	itext t 1 "- wrapperを必要な型に応じて別の名前でimportする"
 ]

wrapperSummary :: Page
wrapperSummary = [\t -> do
	writeTopTitle t "wrapper(まとめ)"
	text t "", \t -> do
	text t "* Haskellの関数を関数ポインタとしてCにわたせる", \t -> do
	text t "* 仮想的な関数wrapperをいろいろなタイプとしてimport", \t -> do
	text t "* gtkなどとのインターフェースを作成するときに必要となる"
 ]

stablePtrPrelude :: Page
stablePtrPrelude = [\t -> do
	writeTopTitle t "StablePtr(はじめに)"
	text t "", \t -> do
	text t "* Haskellの値を固定し、それに対する参照を可能にする", \t -> do
	itext t 1 "- つまりGCの影響を受けない値にするということ", \t -> do
	itext t 1 "- その値はC言語にわたすことができる", \t -> do
	itext t 1 "- その値の中身をC言語側から見ることはできない", \t -> do
	itext t 1 "- 値の中身を見るには再度Haskell側にわたす", \t -> do
	text t "* Haskellの関数をC言語の枠組みのなかで組み合わせるとき", \t -> do
	text t "* Haskellの関数同士が値の受けわたしができる"
 ]

stablePtr1 :: Page
stablePtr1 = [\t -> do
	writeTopTitle t "StablePtr"
	text t "", \t -> do
	text t "* Haskellのコード", \t -> do
	itext t 0.5 "module Tarou where"
	itext t 0.5 "import Foreign.StablePtr"
	itext t 0.5 "foreign export ccall \"tarou\""
	itext t 1.5 "tarou :: IO (StablePtr (String, Int))"
	itext t 0.5 "foreign export ccall \"print_tarou\""
	itext t 1.5 "printTarou :: StablePtr (String, Int) -> IO ()"
	itext t 0.5 "foreign export ccall \"free_StablePtr\""
	itext t 1.5 "freeStablePtr :: Stableptr a -> IO ()"
	itext t 2 "(続く)"
 ]

stablePtr2 :: Page
stablePtr2 = [\t -> do
	writeTopTitle t "StablePtr"
	text t "", \t -> do
	itext t 0 "tarou :: IO (StablePtr (String, Int))"
	itext t 0 "tarou = newStablePtr (\"Tarou\", 33)"
	itext t 0 ""
	itext t 0 "printTarou :: StablePtr (String, Int) -> IO ()"
	itext t 0 "printTarou p = deRefStablePtr p >>= print"
 ]

stablePtr3 :: Page
stablePtr3 = [\t -> do
	writeTopTitle t "StablePtr", \t -> do
	text t "* Cのコード", \t -> do
	itext t 1 "#include \"HsFFI.h\""
	itext t 1 "#ifdef __GLASGOW_HASKELL__"
	itext t 1 "#indluce \"Tarou_stub.h\""
	itext t 1 "#endif"
	itext t 1 "int main (int argc, char *argv[]) {"
	itext t 2 "int i;"
	itext t 2 "hs_init(&argc, &argv);"
	itext t 2 "HsStablePtr t = tarou();"
	itext t 2 "for (i = 0; i < 10; i++) print_tarou(t);"
	itext t 2 "free_StablePtr(t);"
	itext t 2 "hs_exit();"
	itext t 2 "return 0; }"
 ]

stablePtrSummary :: Page
stablePtrSummary = [\t -> do
	writeTopTitle t "StablePtr(まとめ)"
	text t "", \t -> do
	text t "* StablePtrを使うことで", \t -> do
	itext t 1 "- C言語内でHaskell関数同士で値の受け渡しが可能", \t -> do
	text t "* 値はGCの影響を受けない", \t -> do
	text t "* newStablePtrによって作られたStablePtrは", \t -> do
	itext t 1 "- deRefStablePtrによって値を復元可能", \t -> do
	itext t 1 "- ポインタとしては何の保証もない", \t -> do
	itext t 1 "- 正当なメモリを指しているという保証もない"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* wrapperとStablePtrについて学んだ", \t -> do
	text t "* 両者には何の関係もない", \t -> do
	text t "* とりあえず説明をし残した2つをまとめただけ", \t -> do
	text t "* wrapperはgtk等とのインターフェースに使える", \t -> do
	text t "* StablePtrはたとえばOS側のスレッドの利用に使われている"
 ]

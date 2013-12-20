import Lecture

subtitle :: String
subtitle = "第44回 C言語とのインターフェース3"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	wrapperPrelude, wrapper1, wrapper2, wrapper3, wrapperSummary
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

import Lecture

subtitle :: String
subtitle = "第45回 hsc2hs"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, basic,
	include, define, letNameArgs, def, def2, ifelse, errorWarn,
	cnst, cnstStr, typ, peekPoke, peekPoke2, peekPoke3,
	ptrOffset, enum,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* hsc2hsという前処理用のプログラムがある", \t -> do
	text t "* C言語を扱う場合には", \t -> do
	itext t 1 "- マクロから逃れることはできない", \t -> do
	itext t 1 "- 構造体の構造はC言語内からしか見えない", \t -> do
	arrowIText t 1 "Haskell上でマジックナンバーを使うことになる", \t -> do
	text t "* それは避けたいので、前処理で", \t -> do
	itext t 1 "- マクロ展開や構造体に関する計算を行う"
 ]

basic :: Page
basic = [\t -> do
	writeTopTitle t "基本的な構文"
	text t "", \t -> do
	text t "* Haskellコードのなかに#によってうめこまれる", \t -> do
	text t "* コード中で#自体を使う場合は##とする", \t -> do
	text t "* 文字列リテラルやコメント中の#は処理されない", \t -> do
	text t "* 以下のようなときに特殊処理の部分は終わる", \t -> do
	itext t 1 "- 対応するもののない), ], }が現れる", \t -> do
	itext t 1 "- 以下の場合以外の改行"
	itext t 2 "() [] {} '' \"\" /**/内または\\が前置されている", \t -> do
	text t "* #{...}のような形も許される"
 ]

include :: Page
include = [\t -> do
	writeTopTitle t "#include"
	text t "", \t -> do
	text t "* #include <file.h>, #include \"file.h\"", \t -> do
	itext t 1 "- ヘッダファイルが読み込まれる", \t -> do
	itext t 1 "- 定義されたマクロや構造体等が以下の処理で使える"
 ]

define :: Page
define = [\t -> do
	writeTopTitle t "#define, #undef"
	text t "", \t -> do
	text t "* #define 名前, #define 名前 値", \t -> do
	itext t 1 "- 「名前」が定義される", \t -> do
	text t "* #undef 名前", \t -> do
	itext t 1 "- 「名前」が未定義となる"
 ]

letNameArgs :: Page
letNameArgs = [\t -> do
	writeTopTitle t "#let"
	text t "", \t -> do
	text t "* #let 名前 引数列 = \"定義\"", \t -> do
	text t "* 例:", \t -> do
	itext t 1 "#let test x,y = \"x: \" x \", y: \" y"
	itext t 1 "#test \"a\",\"b\"", \t -> do
	text t "* #による文字列化の例:", \t -> do
	itext t 1 "#let test x,y = \"x: \" #x \", y: \" #y"
	itext t 1 "#test 25,hello", \t -> do
	text t "* printfの書式指定子を使用する例:", \t -> do
	itext t 1 "#let test x,y = \"x: %d, y: %d\", x, y"
	itext t 1 "#test 3,99"
 ]

def :: Page
def = [\t -> do
	writeTopTitle t "#def"
	text t "", \t -> do
	text t "* #def [Cの定義]", \t -> do
	itext t 1 "- Cの関数定義、変数定義、構造体定義、typedef", \t -> do
	itext t 1 "- この定義はCファイルとして出力される", \t -> do
	text t "* 補助的なCの関数をhscファイル内で定義することができる"
 ]

def2 :: Page
def2 = [\t -> do
	writeTopTitle t "#def"
	text t "", \t -> do
	text t "* 例:"
	itext t 1 "import Data.Int"
	itext t 1 "foreign import ccall \"add3\""
	itext t 2 "c_add3 :: #{type int} -> #{type int}"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "putStrLn \"Hello, world!\""
	itext t 2 "print $ c_add3 8"
	itext t 1 "#def int add3(int i) { return i + 3; }"
 ]

ifelse :: Page
ifelse = [\t -> do
	writeTopTitle t "#if, #elif, #else, #endif"
	text t "", \t -> do
	text t "* #if [条件] ... #elif [条件] ... #else ... #endif", \t -> do
	itext t 1 "- 条件によってコードが選択される", \t -> do
	text t "* #ifdef [名前], #ifndef [名前]", \t -> do
	itext t 1 "- #ifと同様だが条件が[名前]の定義の如何となる"
 ]

errorWarn :: Page
errorWarn = [\t -> do
	writeTopTitle t "#error, #warning"
	text t "", \t -> do
	text t "* #error [メッセージ], #warning [メッセージ]", \t -> do
	itext t 1 "- メッセージを表示し", \t -> do
	itext t 1 "- #errorではhsc2hsは失敗する"
 ]

cnst :: Page
cnst = [\t -> do
	writeTopTitle t "#const"
	text t "", \t -> do
	text t "* #const [Cの式]", \t -> do
	itext t 1 "- これは非常に有用", \t -> do
	itext t 1 "- C内で定義されているマクロの値や", \t -> do
	itext t 1 "- 列挙型の値を置くことができる", \t -> do
	text t "* 値は整数型"
 ]

cnstStr :: Page
cnstStr = [\t -> do
	writeTopTitle t "#const_str"
	text t "", \t -> do
	text t "* #const_str [Cの式]", \t -> do
	itext t 1 "- #constと同様だが、値が文字列となる"
 ]

typ :: Page
typ = [\t -> do
	writeTopTitle t "#type"
	text t "", \t -> do
	text t "* #type [Cの型]", \t -> do
	itext t 1 "Cの数値型に対応するHaskellでの同等の型を出力"
 ]

peekPoke :: Page
peekPoke = [\t -> do
	writeTopTitle t "#size, #peek, #poke"
	text t "", \t -> do
	text t "* #size [構造体型]", \t -> do
	itext t 1 "- 構造体の大きさ(バイト)", \t -> do
	text t "* #peek [構造体型], [フィールド]", \t -> do
	itext t 1 "- 構造体のフィールドを読み込むコードを返す", \t -> do
	itext t 1 "(\\p -> peekByteOff p [オフセット])", \t -> do
	text t "* #poke [構造体型], [フィールド]", \t -> do
	itext t 1 "- 構造体のフィールドに書き込むコードを返す", \t -> do
	itext t 1 "(\\p -> pokeByteOff p [オフセット])", \t -> do
	text t "例:"
 ]

peekPoke2 :: Page
peekPoke2 = [\t -> do
	writeTopTitle t "#peek, #poke", \t -> do
	itext t (-0.7) "import Foreign.Marshal"
	itext t (-0.7) "import Foreign.C.String"
	itext t (-0.7) "import Foreign.Storable"
	itext t (-0.7) "main :: IO ()"
	itext t (-0.7) "main = allocaBytes #{size body} $ \\ptr -> do"
	itext t 0.1 "#{poke body, name} ptr =<< newCString"
	itext t 0.1 "#{poke body, weight} ptr (72.5 :: #{type double})"
	itext t 0.1 "#{poke body, height} ptr (182.5 :: #{type double})"
	itext t 0.1 "putStrLn =<< peekCString =<< #{peek body, name} ptr"
	itext t 0.1 "print =<< (#{peek body, weight} ptr :: IO #{type double})"
	itext t 0.1 "print =<< (#{peek body, height} ptr :: IO #{type double})"
	itext t 2 "(続く)"
 ]

peekPoke3 :: Page
peekPoke3 = [\t -> do
	writeTopTitle t "#peek, #poke"
	text t "", \t -> do
	itext t 0 "#{def"
	itext t 0 "typedef struct body {"
	itext t 1 "char *name;"
	itext t 1 "double weight;"
	itext t 1 "double height;"
	itext t 0 "} body;"
	itext t 0 "}"
 ]

ptrOffset :: Page
ptrOffset = [\t -> do
	writeTopTitle t "#ptr, #offset"
	text t "", \t -> do
	text t "* #ptr 構造体型, フィールド", \t -> do
	itext t 1 "- フィールドへのポインタを作る", \t -> do
	itext t 1 "(\\p -> p `plusPtr` [オフセット])", \t -> do
	text t "* #offset 構造体型, フィールド", \t -> do
	itext t 1 "- フィールドのオフセット"
 ]

enum :: Page
enum = [\t -> do
	writeTopTitle t "#enum"
	text t "", \t -> do
	text t "* #enum 型, 構築子, 値, 値, ...", \t -> do
	text t "* 例: #define hoge_no 33, #define my_little_lover 345で", \t -> do
	itext t 1 "#enum Type, Cnst, hoge = hoge_no, my_little_lover", \t -> do
	arrowIText t 1 "hoge :: Type"
	itext t 1 "hoge = Cnst 33"
	itext t 1 "myLittleLover :: Type"
	itext t 1 "myLittleLover = Cnst 345"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ", \t -> do
	text t "* ffiの枠組みではうまく扱えないものがある", \t -> do
	itext t 1 "- 列挙型、マクロ定数、マクロ関数、構造体", \t -> do
	itext t 1 "- これらはリンク時にはコード中に存在しない", \t -> do
	itext t 1 "- コードの字面だけに存在するので", \t -> do
	arrowIText t 1 "何らかのプリプロセッサが必要", \t -> do
	text t "* それがhsc2hs", \t -> do
	text t "* hsc2hsはマクロなので", \t -> do
	itext t 1 "- いろいろ便利に使える、が", \t -> do
	itext t 1 "- マクロなのでいろいろな問題がある、ので", \t -> do
	itext t 1 "- C言語とのインターフェース部分にのみ使おう", \t -> do
	text t "* それ以外の用途には上級編で扱うTemplateHaskellを使う"
 ]

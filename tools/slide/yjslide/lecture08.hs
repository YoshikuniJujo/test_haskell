module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第8回 モジュール"

pages :: [Page]
pages = [
	titlePage, prelude, hello, mainModule, preludeModule,
	importDataChar, onlyChr, qualifiedChar, asChar, importSummary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 大きなプログラムは複数のモジュールにわける", \t -> do
	text t "* モジュールは名前の可視性をコントロールできる", \t -> do
	itext t 1 "- インターフェースを定義する", \t -> do
	itext t 1 "- 実装の詳細を隠すことができる"
 ]

hello :: Page
hello = [\t -> do
	writeTopTitle t "Mainモジュール"
	text t "", \t -> do
	text t "main :: IO ()"
	text t "main = putStrLn \"Hello, world!\""
	text t ""
	text t "module名を省略しないで書くと", \t -> do
	dvArrowShort t
	text t ""
	text t "module Main where"
	text t ""
	text t "main :: IO ()"
	text t "main = putStrLn \"Hello, world!\""
 ]

mainModule :: Page
mainModule = [\t -> do
	writeTopTitle t "Mainモジュール"
	text t "", \t -> do
	text t "* 何も書かなければそのファイルはMainモジュールとなる", \t -> do
	text t "* モジュール名の指定は module [モジュール名] where"
 ]

preludeModule :: Page
preludeModule = [\t -> do
	writeTopTitle t "Preludeモジュール"
	text t "", \t -> do
	text t "* 基本的な関数や型を含むモジュール", \t -> do
	text t "* 暗黙のうちにインポートされている", \t -> do
	text t "* このモジュールを意識する必要がある場合がある", \t -> do
	itext t 1 "- Preludeの名前と衝突する名前を使うとき", \t -> do
	itext t 1 "- 詳細は後で"
 ]

importDataChar :: Page
importDataChar = [\t -> do
	writeTopTitle t "import"
	text t "", \t -> do
	text t "module Main where"
	text t "import Data.Char"
	text t ""
	text t "main :: IO ()"
	text t "main = putStrLn $ map chr [72, 101, 108, 108, 111]"
	text t "", \t -> do
	text t "* 文字コードを使って\"Hello\"を表示する関数", \t -> do
	text t "* Data.Charモジュールのchrを使っている"
 ]

onlyChr :: Page
onlyChr = [\t -> do
	writeTopTitle t "何をimportするか"
	text t "", \t -> do
	text t "import Data.Char (chr)"
	text t "", \t -> do
	text t "* chrだけが必要なのでそれだけをimportする", \t -> do
	text t "* import Data.Charのような形はあまり良くない", \t -> do
	itext t 1 "- chrがどこからimportされているかわからなくなる", \t -> do
	itext t 1 "- 上の形か次に述べるqualifiedを使った形が良い"
 ]

qualifiedChar :: Page
qualifiedChar = [\t -> do
	writeTopTitle t "qualified"
	text t "", \t -> do
	text t "module Main where"
	text t "import qualified Data.Char"
	text t ""
	text t "main :: IO ()"
	text t "main = putStrLn $ map Data.Char.chr"
	itext t 1 "[72, 101, 108, 108, 111]"
	text t "", \t -> do
	text t "* qualifiedをつけると完全修飾名しか使えなくなる", \t -> do
	text t "* どこからimportされた名前か一目瞭然", \t -> do
	text t "* しかし、長い"
 ]

asChar :: Page
asChar = [\t -> do
	writeTopTitle t "qualified ... as"
	text t "", \t -> do
	text t "module Main where"
	text t "import qualified Data.Char as C"
	text t ""
	text t "main :: IO ()"
	text t "main = putStrLn $ map C.chr [72, 101, 108, 108, 111]"
	text t "", \t -> do
	text t "* asをつけることでC.chrのように省略した形で使える"
 ]

importSummary :: Page
importSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* import [モジュール名]ですべての名前をimportできる", \t -> do
	itext t 1 "- 便利だが使い捨てのプログラム以外では避けるべき", \t -> do
	text t "* import [モジュール名] (名前のリスト)で指定", \t -> do
	itext t 1 "- 名前のリストは','で区切る", \t -> do
	text t "* qualifiedを使うと完全修飾名のみに制限できる", \t -> do
	text t "* qualified ... asを使うと短い修飾名を使える", \t -> do
	text t "* import qualified ... as ... (...)のような組み合わせも"
	text t "", \t -> do
	text t "importはOK?"
	text t "次はexportについて見ていこう"
 ]

{-

importやexportのときに、変数名、型名、型構築子名についても。
またSome(..)という形についても。

-}

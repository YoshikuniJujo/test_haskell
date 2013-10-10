module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第8回 モジュール"

pages :: [Page]
pages = [
	titlePage, prelude, hello, mainModule, preludeModule,
	importDataChar, onlyChr, qualifiedChar, asChar, importSummary,
	helloExport, helloImport, helloExportProblem, exportTypes, exportClasses,
	importList, fileName, aboutPrelude, summary
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

helloExport :: Page
helloExport = [\t -> do
	writeTopTitle t "Hello export"
	text t "", \t -> do
	text t "% cat Hello.hs"
	text t "module Hello where"
	text t ""
	text t "helloWorld :: String"
	text t "helloWorld = hello ++ \", \" ++ world ++ \"!\""
	text t ""
	text t "hello :: String"
	text t "hello = \"Hello\""
	text t ""
	text t "world :: String"
	text t "world = \"world\""
 ]

helloImport :: Page
helloImport = [\t -> do
	writeTopTitle t "Hello import"
	text t "", \t -> do
	text t "module Main where"
	text t "import Hello (helloWorld)"
	text t ""
	text t "main :: IO ()"
	text t "main = putStrLn helloWorld"
	text t "", \t -> do
	text t "* 1モジュール1ファイル", \t -> do
	text t "* ファイル名は[モジュール名].hsとする", \t -> do
	itext t 1 "- Mainモジュールのファイル名は[自由].hs", \t -> do
	text t "* module Hello whereだとトップレベルの定義全てがexport"
 ]

helloExportProblem :: Page
helloExportProblem = [\t -> do
	writeTopTitle t "問題点"
	text t "", \t -> do
	text t "* モジュールHelloがexportしたいのはhelloWorldだけ", \t -> do
	text t "* hello, worldは内部的に使われている定義", \t -> do
	text t "* このままだと実装の詳細がさらされてしまっている", \t -> do
	text t ""
	text t "exportする変数を指定しよう", \t -> do
	dvArrowShort t
	text t "module Hello (helloWorld) where"
	text t "", \t -> do
	text t "これで、helloWorldのみがexportされる"
 ]

{-

importやexportのときに、変数名、型名、型構築子名についても。
またSome(..)という形についても。
classのほうも。

-}

exportTypes :: Page
exportTypes = [\t -> do
	writeTopTitle t "型のエクスポート"
	text t "", \t -> do
	text t "module ExportType(Some(Some, Other)) where"
	text t ""
	text t "data Some = Some Int | Other Char"
	text t "", \t -> do
	text t "* すべての型構築子をエクスポートする場合の省略記法", \t -> do
	dvArrowShort t
	itext t 1 "module ExportType(Some(..)) where"
 ]

exportClasses :: Page
exportClasses = [\t -> do
	writeTopTitle t "クラスのエクスポート"
	text t "", \t -> do
	text t "module ExportClass(Some(some, other)) where"
	text t ""
	text t "class Some a where"
	itext t 1 "some :: a"
	itext t 1 "other :: a"
	text t "", \t -> do
	text t "* すべてのメソッドをエクスポートする場合の省略記法", \t -> do
	dvArrowShort t
	itext t 1 "module ExportClass(Some(..)) where"
 ]

importList :: Page
importList = [\t -> do
	writeTopTitle t "型、型クラスのインポート"
	text t "", \t -> do
	text t "* 型、型クラスのエクスポートと同じようにする"
	itext t 1 "- import(Some(..))等"
 ]

fileName :: Page
fileName = [\t -> do
	writeTopTitle t "モジュールファイル"
	text t "", \t -> do
	text t "* モジュールの階層構造はディレクトリ構造で表される", \t -> do
	text t "* パスの通っているディレクトリをトップとする", \t -> do
	itext t 1 "- カレントディレクトリにもパスが通っている", \t -> do
	itext t 1 "- パスを通すには -i[directory] とすれば良い", \t -> do
	text t "* Foo.Bar.Bazならば./Foo/Bar/Baz.hsに置けば良い"
 ]

aboutPrelude :: Page
aboutPrelude = [\t -> do
	writeTopTitle t "import Prelude"
	text t "", \t -> do
	text t "* Preludeは暗黙にインポートされる", \t -> do
	text t "* 明示的にimportするのはインポートしない名前がある場合", \t -> do
	text t "* import Prelude ()", \t -> do
	itext t 1 "- Preludeに定義されている名前をインポートしない", \t -> do
	text t "* import Prelude ((+))", \t -> do
	itext t 1 "- Preludeからは演算子+のみをインポート", \t -> do
	itext t 1 "- 演算子はインポートリストに書くときは()でくくる", \t -> do
	text t "* import Prelude hiding (head, tail)", \t -> do
	itext t 1 "- Preludeからheadとtail以外をインポートする"

 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 1ファイル1モジュール、ファイル名は[モジュール名].hs", \t -> do
	itext t 1 "- Mainモジュールのみ[自由].hs", \t -> do
	text t "* インポートにはいくつかのやりかたがある", \t -> do
	text t "* エクスポートする名前を指定することでカプセル化できる", \t -> do
	text t "* 型や型クラスのインポート/エクスポートの構文", \t -> do
	text t "* 演算子のインポート/エクスポートの構文", \t -> do
	text t "* Preludeは暗黙のインポート", \t -> do
	itext t 1 "- 同じ名前を使いたい場合などは明示的にインポート"
 ]

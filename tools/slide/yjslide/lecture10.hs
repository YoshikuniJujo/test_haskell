module Main where

import Lecture

subtitle :: String
subtitle = "第10回 ghcの使いかた"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	interpreter1, interpreter2,
	interactive1, interactive2,
	compiler1, compiler2, compiler3, compiler4, compiler5,
	profiler1,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ghcとはHaskellの", \t -> do
	itext t 1 "- コンパイラ", \t -> do
	itext t 1 "- インタプリタ", \t -> do
	itext t 1 "- 対話環境", \t -> do
	itext t 1 "- プロファイラ", \t -> do
	itext t 1 "- これらすべてである", \t -> do
	text t "* 簡単なmake的な機能もある", \t -> do
	itext t 1 "- ファイルの依存性をチェックして最少限のビルド", \t -> do
	text t "* よってオプションも多岐に渡る", \t -> do
	text t "* ある程度しぼって、それらの機能を見ていこう"
 ]

interpreter1 :: Page
interpreter1 = [\t -> do
	writeTopTitle t "インタプリタ"
	text t "", \t -> do
	text t "% runghc --version", \t -> do
	text t "runghc 7.6.3"
	text t "", \t -> do
	text t "% runghc hello.hs", \t -> do
	text t "Hello, world!"
	text t "", \t -> do
	text t "% ghc -e main hello.hs", \t -> do
	text t "Hello, world!"
	text t "", \t -> do
	text t "% ghc -e '33 + 88'", \t -> do
	text t "121"
 ]

interpreter2 :: Page
interpreter2 = [\t -> do
	writeTopTitle t "インタプリタ"
	text t "", \t -> do
	text t "* ghc -e [表現] [ファイル名]", \t -> do
	itext t 1 "- モジュールを読み込みその文脈で表現を評価実行", \t -> do
	itext t 1 "- ファイル名が指定されなければPreludeの文脈で", \t -> do
	text t "* runghc [ファイル名]", \t -> do
	itext t 1 "- ghc -e main [ファイル名]と同じ", \t -> do
	itext t 1 "- runghc -Wall [ファイル名]とするのは良い習慣"
 ]

interactive1 :: Page
interactive1 = [\t -> do
	writeTopTitle t "対話的環境"
	text t "", \t -> do
	text t "* ghci [ファイル名]", \t -> do
	itext t 1 "- モジュールを読み込み対話環境に入る", \t -> do
	itext t 1 "- ファイル名の指定なければPreludeだけを読み込む"
 ]

interactive2 :: Page
interactive2 = [\t -> do
	writeTopTitle t "対話的環境"
	text t "", \t -> do
	text t "* 対話環境において", \t -> do
	itext t 1 "- 式を入力すると値を表示", \t -> do
	itext t 1 "- IO aは実行し返値を表示できれば表示", \t -> do
	itext t 1 "- let [パターン] = [表現]で変数束縛", \t -> do
	itext t 1 "- :m [モジュール名]でモジュールの読み込み", \t -> do
	itext t 1 "- :m + [モジュール名]でモジュールの追加読み込み", \t -> do
	itext t 1 "- :reloadでモジュールの再読み込み", \t -> do
	itext t 1 "- :t [表現]で型を表示", \t -> do
	itext t 1 "- :i [名前]で情報を表示", \t -> do
	itext t 1 "- :run act arg1 arg2 ...でコマンドライン引数指定"
 ]

compiler1 :: Page
compiler1 = [\t -> do
	writeTopTitle t "コンパイラ"
	text t "", \t -> do
	text t "* とりあえず ghc --version", \t -> do
	itext t 1 "The Glorious Glasgow Haskell Compilation System,"
	itext t 1 "version 7.6.3", \t -> do
	text t "* 最も単純な例", \t -> do
	itext t 1 "ghc hello.hs"
	itext t 1 "./hello => Hello, world!", \t -> do
	text t "* ghcはimportされているモジュールを追いかけてくれる", \t -> do
	text t "* 更新されているモジュールだけを再コンパイルしてくれる", \t -> do
	arrowIText t 1 "makeの機能が組み込まれているということ"
 ]

compiler2 :: Page
compiler2 = [\t -> do
	writeTopTitle t "コンパイラ"
	text t "", \t -> do
	text t "* -Wallをつけよう", \t -> do
	itext t 1 "ghc -Wall hello.hs", \t -> do
	itext t 1 "- 良くない(かもしれない)コードを指摘してくれる", \t -> do
	text t "* C言語で書かれたコードを使うとき", \t -> do
	itext t 1 "- 詳細はFFIの回で説明する", \t -> do
	itext t 1 "ghc h_code.hs c_code.c", \t -> do
	itext t 1 "ghc h_code.hs c_code.o (オブジェクトファイル)", \t -> do
	text t "* モジュールの探索パス", \t -> do
	itext t 1 "- モジュールA.B.Cは./A/B/C.hsに置く", \t -> do
	itext t 1 "- d1/A/B/C.hsに置きたい場合、-id1をつける", \t -> do
	itext t 1 "-id1:d2:d3... と複数指定することもできる"
 ]

compiler3 :: Page
compiler3 = [\t -> do
	writeTopTitle t "コンパイラ"
	text t "", \t -> do
	text t "* 出力ファイルの名前を指定する", \t -> do
	itext t 1 "ghc -o goodbye hello.hs", \t -> do
	text t "* 分割コンパイル", \t -> do
	itext t 1 "ghc -c A.hs"
	itext t 1 "ghc -c B.hs"
	itext t 1 "ghc -c Main.hs"
	itext t 1 "ghc -o foo A.o B.o Main.o"
 ]

compiler4 :: Page
compiler4 = [\t -> do
	writeTopTitle t "コンパイラ", \t -> do
	text t "* スタックサイズ", \t -> do
	itext t 1 "./some"
	itext t 1 "Stack space overflow: ...", \t -> do
	itext t 1 "こういうとき、まずはソースコードを確認", \t -> do
	itext t 2 "無限ループになってしまっていないか?", \t -> do
	arrowIText t 1 "問題無い、たくさんスタックを使いそうなプログラム", \t -> do
	dvArrowShort t
	text t "ghc some.hs -rtsopts"
	text t "./some +RTS -K400m -RTS", \t -> do
	itext t 1 "- スタックサイズをデフォルトの8Mから400Mに"
	arrowIText t 1 "ghc some.hs -with-rtsopts=\"-K400m\"としても"
 ]

compiler5 :: Page
compiler5 = [\t -> do
	writeTopTitle t "コンパイラ"
	text t "", \t -> do
	text t "* -Oをつけると最適化される", \t -> do
	text t "* -Oと-O1は同じ", \t -> do
	text t "* -O2だともっと徹底的に最適化を行う", \t -> do
	text t "* -O2をつけても-Oよりも改善することはあまりない", \t -> do
	text t "* コンパイル時間はひどく悪化するらしい", \t -> do
	arrowIText t 1 "-O2は使わないほうが良さそう"
 ]

profiler1 :: Page
profiler1 = [\t -> do
	writeTopTitle t "プロファイラ"
	text t "", \t -> do
	text t "% ghc -prof -fprof-auto -rtsopts some.hs"
	text t "% ./some +RTS -p -h", \t -> do
	itext t 1 "some.profとsome.hpができる", \t -> do
	itext t 1 "some.profを見るとボトルネックを知ることができる", \t -> do
	text t "% hp2ps -c some.hp"
	text t "% ps2pdf some.ps"
	text t "% firefox some.pdf", \t -> do
	itext t 1 "メモリの使用状況を見ることができる"
	text t "", \t -> do
	semititle t "詳細はプロファイリングの回で"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* ghcの基本的な使いかたについてまとめた", \t -> do
	text t "* 典型的な流れとしては", \t -> do
	itext t 1 "- ghci some.hsで関数ごとのチェック", \t -> do
	itext t 1 "- runghc some.hsで動作をチェックする", \t -> do
	itext t 1 "- ghc some.hsでコンパイル", \t -> do
	itext t 1 "- 以下でプロファイリング", \t -> do
	itext t 2 "ghc -prof -fprof-auto -rtsopts some.hs"
	itext t 2 "./some +RTS -p -h", \t -> do
	itext t 1 "- ghc -O some.hs を試す等", \t -> do
	text t "* 分割コンパイルやCコードの使用等も可能"
 ]

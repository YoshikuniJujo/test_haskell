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
	interactive
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
	itext t 1 "- ghc -e main [ファイル名]と同じ"
 ]

interactive :: Page
interactive = [\t -> do
	writeTopTitle t "対話的環境", \t -> do
	text t "* ghci [ファイル名]", \t -> do
	itext t 1 "- モジュールを読み込み対話環境に入る", \t -> do
	itext t 1 "- ファイル名の指定なければPreludeだけを読み込む", \t -> do
	text t "* 対話環境において", \t -> do
	itext t 1 "- 式を入力すると値を表示", \t -> do
	itext t 1 "- IO aは実行し返値を表示できれば表示", \t -> do
	itext t 1 "- let [パターン] = [表現]で変数束縛", \t -> do
	itext t 1 "- :m + [モジュール名]でモジュールの追加読み込み", \t -> do
	itext t 1 "- :reloadでモジュールの再読み込み", \t -> do
	itext t 1 "- :t [表現]で型を表示", \t -> do
	itext t 1 "- :i [名前]で情報を表示", \t -> do
	itext t 1 "- :run act arg1 arg2 ...でコマンドライン引数指定"
 ]

version :: Page
version = [\t -> do
	writeTopTitle t "インタプリタ"
	text t "", \t -> do
	text t "% runghc --version", \t -> do
	text t "The Glorious Glasgow Haskell Compilation System,"
	text t "version 7.6.3"
 ]

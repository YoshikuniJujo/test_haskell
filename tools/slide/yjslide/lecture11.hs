module Main where

import Lecture

subtitle :: String
subtitle = "第11回 ドキュメント"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, showType, basic, example, parameter, moduleEx,
	sections, markup1, markup2,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* コメントからドキュメントを生成しよう", \t -> do
	itext t 1 "- 日本語で書くと他言語の人にはノイズになる", \t -> do
	itext t 1 "- 英語は苦手", \t -> do
	itext t 1 "- 使用例くらいなら書けるかな", \t -> do
	text t "* haddockを使おう", \t -> do
	text t "* 関数の型は自動的に表示してくれる", \t -> do
	itext t 1 "- 仕事は半分終わったようなもの", \t -> do
	itext t 1 "- 使用例以外は暇なときに辞書でも引きながら書くか", \t -> do
	dvArrowShort t
	itext t 1 "そんな感じ"
 ]

showType :: Page
showType = [\t -> do
	writeTopTitle t "型の表示"
	text t "", \t -> do
	text t "% cat Hello.hs", \t -> do
	text t "module Hello where"
	text t ""
	text t "greeting name = \"Hello, \" ++ name ++ \"!\""
	text t "", \t -> do
	text t "% haddock --html hello.hs -o html"
	text t "% firefox html/index.html"
	text t "", \t -> do
	text t "コードだけしか書いていなくても、型だけは表示してくれる"
 ]

basic :: Page
basic = [\t -> do
	writeTopTitle t "基本"
	text t "", \t -> do
	text t "-- |make greeting"
	text t "greeting name = \"Hello, \" ++ name ++ \"!\""
	text t "", \t -> do
	text t "これは以下と同じ", \t -> do
	text t "greeting name = \"Hello, \" ++ name ++ \"!\""
	text t "-- ^make greeting"
	text t "", \t -> do
	text t "* 上から説明するときは'|'", \t -> do
	text t "* 下から説明するときは'^'"
 ]

example :: Page
example = [\t -> do
	writeTopTitle t "使用例"
	text t "", \t -> do
	text t "使用例を書く場合'>>>'を使う"
	text t "", \t -> do
	text t "-- |"
	text t "-- >>> greeting \"world\""
	text t "-- \"Hello, world!\""
	text t "greeting name = \"Hello, \" ++ name ++ \"!\""
	text t "", \t -> do
	text t "この使用例はdoctestを使ってテストすることができる", \t -> do
	text t "% doctest hello.hs"
	text t "Examples: 1 Tried: 1 Errors: 0 Failures: 0"
 ]

parameter :: Page
parameter = [\t -> do
	writeTopTitle t "引数の説明"
	text t "", \t -> do
	text t "greeting :: String -- ^your name"
	itext t 1 "-> String -- ^greeting"
	text t "", \t -> do
	text t "* 型宣言のところに引数の説明をつけることができる"
 ]

moduleEx :: Page
moduleEx = [\t -> do
	writeTopTitle t "モジュール全体の説明"
	text t "", \t -> do
	text t "-- |greeting library"
	text t "module Hello where"
	text t "", \t -> do
	text t "* モジュール全体の説明もつけられる"
 ]

sections :: Page
sections = [\t -> do
	writeTopTitle t "セクションごとに分ける"
	text t "", \t -> do
	text t "module Hello ("
	itext t 1 "-- * greeting"
	itext t 1 "-- ** day"
	itext t 1 "greeting,"
	itext t 1 "-- ** night"
	itext t 1 "greetingNight"
	text t ") where"
	text t "", \t -> do
	text t "* '*'の数を増やすことで"
	itext t 1 "セクション、サブセクション、...を表せる"
 ]

markup1 :: Page
markup1 = [\t -> do
	writeTopTitle t "マークアップ"
	text t "", \t -> do
	text t "* 空行で段落を表現することができる", \t -> do
	text t "* 以下は特殊な意味を持ち得る文字('\\'でエスケープ可)"
	itext t 1 "/, ', `, \", @, <, >, *, -, >>>", \t -> do
	itext t 1 "- 使いかたによってはエスケープしなくてもすむ", \t -> do
	text t "* \"-- @\"ではさまれた行はコードブロック", \t -> do
	text t "* >ではじまる行もコードブロック", \t -> do
	text t "* >>>ではじまる行は使用例、後に結果を続ける", \t -> do
	text t "* /.../は強調、@...@は等幅", \t -> do
	text t "* *, -は列挙、(n), n.は番号付き列挙、[@...@] ...は定義"
 ]

markup2 :: Page
markup2 = [\t -> do
	writeTopTitle t "マークアップ"
	text t "", \t -> do
	text t "* 'var'とするとvarの説明へのリンクとなる", \t -> do
	text t "* \"Foo\"とするとFooモジュールの説明へのリンクとなる", \t -> do
	text t "* #label#でアンカー、\"Foo#label\"でアンカーへのリンク", \t -> do
	text t "* URLは<...>とする"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 英語を書くのが苦手な人でも便利に使える機能がある", \t -> do
	text t "* 特に使用例(>>>)はdoctestと組み合わせて使うと便利", \t -> do
	text t "* 他はURLが載せられるあたりは使えるかな"
 ]

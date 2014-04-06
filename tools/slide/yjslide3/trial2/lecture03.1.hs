import Lecture

subtitle :: String
subtitle = "トライアル 第3.1回 環境に慣れよう"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, whats,
	keyboard, editor, nano, backspace, backspace2, backspaceSummary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* この回の内容は", \t -> do
	text t "* 講義で使用するキーボード、エディタ、シェルの操作", \t -> do
	text t "* 手を動かすことで講義内容が身につく", \t -> do
	text t "* そのためには", \t -> do
	text t "* まずはレクチャー用の環境に慣れてもらうと効率的"
 ]

whats :: Page
whats = [\t -> do
	writeTopTitle t "何を学ぶのか"
	text t "", \t -> do
	text t "* 以下の操作について学んでいく", \t -> do
	text t "* 講義で使うキーボードの使いかた", \t -> do
	text t "* nanoエディタの使いかた", \t -> do
	text t "* シェル(コマンドプロンプト)の使いかた", \t -> do
	text t "* Haskellの対話環境の使いかた"
	text t "", \t -> do
	itext t 0 "(注) この講義では以下の3つをほぼ同じ意味に用いる", \t -> do
	itext t 1 "シェル", \t -> do
	itext t 1 "ターミナル", \t -> do
	itext t 1 "コマンドプロンプト"
 ]

keyboard :: Page
keyboard = [\t -> do
	writeTopTitle t "キーボード"
	text t "", \t -> do
	text t "* 今回用意したキーボードはHappy Hacking Keyboard", \t -> do
	itext t 1 "- 以降はHHKBと表記することにする", \t -> do
	text t "* HHKBの英語配列キーボードを用意した", \t -> do
	text t "* 不必要なキーを極力排除しシンプルでコンパクト", \t -> do
	text t "* そのため、入力のしかたにすこし癖がある", \t -> do
	text t "* エディタの使いかたを学ぶと同時にHHKBにも慣れておこう"
 ]

editor :: Page
editor = [\t -> do
	writeTopTitle t "エディタ"
	text t "", \t -> do
	text t "* 今回用意したエディタは以下の3つ", \t -> do
	itext t 1 "nano, vim, emacs", \t -> do
	text t "* この3つから選んでもらうことになる", \t -> do
	text t "* vimやemacsを選ぶ人に対しては", \t -> do
	itext t 1 "「いつものように編集してください」と言っておこう", \t -> do
	text t "* ちなみに僕はvim userなのでvimについてなら教えられる", \t -> do
	text t "* どれも使ったことがないという人はnanoを選ぼう"
 ]

nano :: Page
nano = [\t -> do
	writeTopTitle t "nano"
	text t "", \t -> do
	text t "* まずはnanoの操作について学んでいこう", \t -> do
	text t "* ついでにHHKBの操作についても学ぶ", \t -> do
	text t "* デスクトップに左右2つのターミナルがあると思う", \t -> do
	text t "* どちらかに以下のように打ち込もう", \t -> do
	itext t 1 "% nano -w"
	itext t 1 "('%'はシェルのプロンプトなので打ち込まない)", \t -> do
	text t "* 入力例において%ではじめた場合はシェルからの入力を示す", \t -> do
	text t "* -wオプションはワードラップ処理を無効化する", \t -> do
	text t "* ワードラップ処理とは", \t -> do
	itext t 1 "- 入力行が長くなったときに自動で改行を挿入する処理"
 ]

backspace :: Page
backspace = [\t -> do
	writeTopTitle t "バックスペース"
	text t "", \t -> do
	text t "* HHKBにはバックスペースキーがない", \t -> do
	text t "* バックスペースを入力したいときには", \t -> do
	itext t 1 "Fn + Delete", \t -> do
	text t "* やってみよう", \t -> do
	text t "* エディタに適当な文字たとえば以下を入力する", \t -> do
	itext t 1 "miss", \t -> do
	text t "* ここでFn + Deleteを4回入力してみよう", \t -> do
	text t "* 文字列が消えたはずだ"
 ]

backspace2 :: Page
backspace2 = [\t -> do
	writeTopTitle t "バックスペース"
	text t "", \t -> do
	text t "* バックスペースの入力には実はもっとお勧めの方法がある", \t -> do
	text t "* 以下のキーストロークで入力可能だ", \t -> do
	itext t 1 "Control + 'h'", \t -> do
	text t "* HHKBではControlキーが左手小指のすぐ左にある", \t -> do
	text t "* これだとほぼホームポジションのままで入力できる", \t -> do
	text t "* やってみよう", \t -> do
	text t "* エディタに適当な文字たとえば以下を入力する", \t -> do
	itext t 1 "machigaeta", \t -> do
	text t "* ここでControl + 'h'を10回入力してみよう", \t -> do
	text t "* 文字列が消えたはずだ"
 ]

backspaceSummary :: Page
backspaceSummary = [\t -> do
	writeTopTitle t "バックスペース"
	text t "", \t -> do
	text t "* 入力の最中にバックスペースが使いたくなったら", \t -> do
	itext t 1 "- 以下のいずれかのキーストロークで入力する", \t -> do
	itext t 1 "1. Fn + Delete", \t -> do
	itext t 1 "2. Control + 'h'", \t -> do
	text t "* 好きなほうを使おう"
 ]

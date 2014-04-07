import Lecture

subtitle :: String
subtitle = "トライアル 第3.2回 エディタ、シェル、対話環境"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	nano, nano2, nano3, nano4, nano5, nano6, nano7
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* この回ではエディタ、シェル、対話環境の操作を学ぶ", \t -> do
	text t "* エディタの操作についてはnanoエディタのみを扱う", \t -> do
	itext t 1 "- vimやEmacsユーザーは「いつも通りに」", \t -> do
	text t "* シェルの操作はvim的なキーストロークを使用する", \t -> do
	text t "* ghcの対話環境についても学ぶ"
 ]

nano :: Page
nano = [\t -> do
	writeTopTitle t "nano: 起動と終了"
	text t "", \t -> do
	text t "* まずはエディタを起動する", \t -> do
	itext t 1 "% nano -w", \t -> do
	text t "* 編集画面になったはずだ", \t -> do
	text t "* 何もせずに終了してみよう", \t -> do
	itext t 1 "Ctrl + x", \t -> do
	text t "* -wオプションはワードラップ処理を無効化している", \t -> do
	text t "* ワードラップ処理とは長い行に自動で改行を挿入する処理"
 ]

nano2 :: Page
nano2 = [\t -> do
	writeTopTitle t "nano: ファイル名を指定"
	text t "", \t -> do
	text t "* サンプルファイルを作る前にディレクトリを移動しよう", \t -> do
	itext t 1 "% cd ~/lectures/lecture00/", \t -> do
	itext t 1 "% pwd", \t -> do
	itext t 1 "/home/guest/lectures/lecture00/", \t -> do
	text t "* 空のファイルを作る", \t -> do
	itext t 1 "% touch test.txt", \t -> do
	text t "* ファイル名を指定して起動", \t -> do
	itext t 1 "% nano -w test.txt"
 ]

nano3 :: Page
nano3 = [\t -> do
	writeTopTitle t "nano: 保存と終了"
	text t "", \t -> do
	text t "* 適当な文字を入力してみよう", \t -> do
	itext t 1 "foo bar buz", \t -> do
	text t "* 保存する", \t -> do
	itext t 1 "Ctrl + o", \t -> do
	text t "* 保存するファイル名が正しければreturnを押す", \t -> do
	text t "* 終了する", \t -> do
	itext t 1 "Ctrl + x", \t -> do
	text t "* 確認してみよう", \t -> do
	itext t 1 "% ls", \t -> do
	itext t 1 "test.txt", \t -> do
	itext t 1 "% cat test.txt", \t -> do
	itext t 1 "foo bar baz"
 ]

nano4 :: Page
nano4 = [\t -> do
	writeTopTitle t "nano: カットとペースト"
	text t "", \t -> do
	text t "* 今のファイルを再度開く", \t -> do
	itext t 1 "% nano -w test.txt", \t -> do
	text t "* ファイルの先頭に以下を追加する", \t -> do
	itext t 1 "qux quux corge[改行]", \t -> do
	text t "* qux ...の行にカーソルを移動し", \t -> do
	itext t 1 "Ctrl + k", \t -> do
	text t "* foo ...の行の下にカーソルを移動し", \t -> do
	itext t 1 "Ctrl + u", \t -> do
	text t "* これでカット->ペーストができた"
 ]

nano5 :: Page
nano5 = [\t -> do
	writeTopTitle t "nano: コピーとペースト"
	text t "", \t -> do
	text t "* nanoにはコピーコマンドがないので", \t -> do
	itext t 1 "カット->ペーストでコピーをエミュレートする", \t -> do
	text t "* foo ...の行にカーソルを移動し", \t -> do
	itext t 1 "Ctrl + k -> Ctrl + u", \t -> do
	text t "* qux ...の行の下にカーソルを移動し", \t -> do
	itext t 1 "Ctrl + u", \t -> do
	text t "* これでコピ-->ペーストができた"
 ]

nano6 :: Page
nano6 = [\t -> do
	writeTopTitle t "nano: 範囲選択"
	text t "", \t -> do
	text t "* 範囲選択を行うとカット->ペーストはより便利に使える", \t -> do
	text t "* カーソルをたとえばbarのbの前のスペースに移動させ", \t -> do
	itext t 1 "Ctrl + 6", \t -> do
	text t "* カーソルをquuxのxの後のスペースに移動させる", \t -> do
	itext t 1 "Ctrl + k", \t -> do
	text t "* カーソルをfoo ...の行の下に置き", \t -> do
	itext t 1 "Ctrl + u", \t -> do
	text t "* これで選択範囲のカット->ペーストができる", \t -> do
	text t "* Ctrl + kをCtrl + k -> Ctrl + uとすればコピーとなる", \t -> do
	text t "* Ctrl + o, Ctrl + xで終了しよう"
 ]

nano7 :: Page
nano7 = [\t -> do
	writeTopTitle t "nano: まとめ"
	text t "", \t -> do
	text t "* nanoの操作について学んだ", \t -> do
	text t "* 起動はnano -w [ファイル名]", \t -> do
	text t "* 保存はCtrl + o", \t -> do
	text t "* 終了はCtrl + x", \t -> do
	text t "* 切り取りはCtrl + k", \t -> do
	text t "* 貼りつけはCtrl + u", \t -> do
	text t "* 範囲選択はCtrl + 6"
 ]

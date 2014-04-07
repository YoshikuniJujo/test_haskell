import Lecture

subtitle :: String
subtitle = "トライアル 第3.2回 エディタ、シェル、対話環境"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	nano, nano2, nano3, nano4, nano5, nano6, nano7,
	shell, shell2, shell3, shell4, shell5, shell6, shell7, shellSummary,
	ghci, ghci2, ghci3, ghci4, ghci5, ghciSummary,
	summary
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

shell :: Page
shell = [\t -> do
	writeTopTitle t "zsh"
	text t "", \t -> do
	text t "* 今回用意した環境のシェルはzsh", \t -> do
	text t "* モードはViモードとしてある", \t -> do
	text t "* このモードでの簡単な操作方法を学ぼう"
 ]

shell2 :: Page
shell2 = [\t -> do
	writeTopTitle t "zsh: バックスペース"
	text t "", \t -> do
	text t "* まずは普通にコマンドを打ち込んでみる", \t -> do
	itext t 1 "% pwd", \t -> do
	itext t 1 "/home/guest/lectures/lecture00", \t -> do
	itext t 1 "% ls", \t -> do
	itext t 1 "test.txt", \t -> do
	itext t 1 "% cat test.txtt"
	itext t 1 "(まだreturnは押さない)", \t -> do
	text t "* ひとつtを余分に入力してしまった", \t -> do
	text t "* バックスペースを入力する", \t -> do
	itext t 1 "Ctrl + h", \t -> do
	text t "* 正しいコマンドになったのでreturnを入力"
 ]

shell3 :: Page
shell3 = [\t -> do
	writeTopTitle t "zsh: カーソルの移動"
	text t "", \t -> do
	text t "* 以下のようにコマンドを打つ", \t -> do
	itext t 1 "% cat ttest.txt"
	itext t 1 "(まだreturnは押さない)", \t -> do
	text t "* ここでEscキーを押す"
	itext t 1 "(Escキーは1の左にある)", \t -> do
	text t "* Escキーを押すとカーソルの移動モードになる", \t -> do
	text t "* hキーで左にlキーで右にカーソルが移動する", \t -> do
	itext t 1 "hとlを利用して重複したtの上にカーソルを動かす", \t -> do
	text t "* xキーを押すと1文字削除される", \t -> do
	text t "* 正しいコマンドになったのでreturnを入力"
 ]

shell4 :: Page
shell4 = [\t -> do
	writeTopTitle t "zsh: 文字の挿入"
	text t "", \t -> do
	text t "* 以下のようにコマンドを打つ", \t -> do
	itext t 1 "% cat st.t"
	itext t 1 "(まだreturnは押さない)", \t -> do
	text t "* ここでEscキーを押す", \t -> do
	text t "* h, lキーでsのうえにカーソルを移動させる", \t -> do
	text t "* iキーを押すと挿入モードになる", \t -> do
	itext t 1 "teを入力", \t -> do
	text t "* Escキーでカーソル移動モードにもどる", \t -> do
	text t "* lキーで文字列の最後まで移動する", \t -> do
	text t "* aキーを押すと挿入モードになる", \t -> do
	itext t 1 "xtを入力", \t -> do
	text t "* 正しいコマンドになったのでreturnを入力"
 ]

shell5 :: Page
shell5 = [\t -> do
	writeTopTitle t "zsh: 文字の挿入"
	text t "", \t -> do
	text t "* 文字を挿入するためには", \t -> do
	itext t 1 "移動モードから挿入モードにする必要がある", \t -> do
	itext t 1 "- iキーはカーソル位置の直前に挿入", \t -> do
	itext t 1 "- aキーはカーソル位置の直後に挿入"
 ]

shell6 :: Page
shell6 = [\t -> do
	writeTopTitle t "zsh: 履歴を使用する"
	text t "", \t -> do
	text t "* echoコマンドで文字列を表示させる", \t -> do
	itext t 1 "% echo 'Hello, world!'", \t -> do
	itext t 1 "Hello, world!", \t -> do
	text t "* 次に\"Hello, my friend!\"と表示させたい", \t -> do
	text t "* このようなときに履歴機能が使える", \t -> do
	text t "* Escキーで移動モードに入る", \t -> do
	text t "* kキーで履歴をさかのぼりjキーで履歴を順送りする", \t -> do
	itext t 1 "kキーを3回押し、jキーを2回押す", \t -> do
	text t "* 履歴上の逆送りと順送りがされているのがわかるだろうか"
 ]

shell7 :: Page
shell7 = [\t -> do
	writeTopTitle t "zsh: 履歴を使用する"
	text t "", \t -> do
	text t "* 今、コマンド行は以下のようになっているはずだ", \t -> do
	itext t 1 "% echo 'Hello, world!'", \t -> do
	text t "* worldの部分をmy friendに変えれば良い", \t -> do
	text t "* hキーとlキーでカーソルをwのところに動かし", \t -> do
	itext t 1 "xキーを5回入力する", \t -> do
	text t "* これでworldが消える", \t -> do
	text t "* iキーで挿入モードに移行し以下を入力する", \t -> do
	itext t 1 "my friend", \t -> do
	text t "* これで正しいコマンドになったのでreturnを入力する"
 ]

shellSummary :: Page
shellSummary = [\t -> do
	writeTopTitle t "zsh: まとめ"
	text t "", \t -> do
	text t "* zshのViモードにおける"
	itext t 1 "コマンド行の編集と履歴の利用について学んだ", \t -> do
	text t "* 挿入モードとカーソル移動モードがある", \t -> do
	text t "* Escキーでカーソル移動モードに移行すると", \t -> do
	text t "* h, lキーでカーソルが移動する", \t -> do
	text t "* xキーで1文字削除が可能", \t -> do
	text t "* i, aキーで挿入モードに移行できる", \t -> do
	text t "* 移動モードでk, jキーを使って履歴内の移動が可能", \t -> do
	text t "* 履歴も、その場で打ち込んだコマンド同様に編集できる"
 ]

ghci :: Page
ghci = [\t -> do
	writeTopTitle t "ghci"
	text t "", \t -> do
	text t "* ghciコマンドでghcの対話環境を利用できる", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> ", \t -> do
	text t "* まちがえた場合はCtrl + hで消していけば良い", \t -> do
	itext t 1 "Prelude> miss"
	itext t 1 "(まだreturnは入力しない)", \t -> do
	itext t 1 "Ctrl + hを4回入力する", \t -> do
	itext t 1 "Prelude>"
 ]

ghci2 :: Page
ghci2 = [\t -> do
	writeTopTitle t "ghci: ファイルの読み込み"
	text t "", \t -> do
	text t "* 前準備としてtest.hsを作っておく", \t -> do
	itext t 1 "% cd ~/lectures/lecture00/", \t -> do
	itext t 1 "% nano -w test.hs", \t -> do
	itext t 1 "add x y = x + y", \t -> do
	text t "* Ctrl + oで保存", \t -> do
	text t "* エディタは開いたままにしておく", \t -> do
	text t "* シェルのコマンドライン引数でファイルを指定できる", \t -> do
	itext t 1 "別のターミナルで", \t -> do
	itext t 1 "% cd ~/lectures/lecture00/", \t -> do
	itext t 1 "% ghci test.hs", \t -> do
	itext t 1 "*Main> "
 ]

ghci3 :: Page
ghci3 = [\t -> do
	writeTopTitle t "ghci: ファイルの読み込み"
	text t "", \t -> do
	text t "* ちゃんと読み込めているか確認する", \t -> do
	itext t 1 "*Main> add 3 4", \t -> do
	itext t 1 "7", \t -> do
	text t "* test.hsを開いているエディタに以下を追加する", \t -> do
	itext t 1 "sub x y = x - y", \t -> do
	text t "* ghci側でのファイルの再読み込みが必要", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> sub 8 2", \t -> do
	itext t 1 "6"
 ]

ghci4 :: Page
ghci4 = [\t -> do
	writeTopTitle t "ghci: ファイルの読み込み"
	text t "", \t -> do
	text t "* エディタを抜ける", \t -> do
	itext t 1 "Ctrl + x", \t -> do
	text t "* 別のファイルを作る", \t -> do
	itext t 1 "% nano -w other.txt", \t -> do
	text t "* 以下を書き込む", \t -> do
	itext t 1 "mul x y = x * y", \t -> do
	text t "* 保存する", \t -> do
	itext t 1 "Ctrl + o", \t -> do
	text t "* ghciからこのファイルを読み込む", \t -> do
	itext t 1 "*Main> :load other.txt", \t -> do
	itext t 1 "*Main> mul 3 4", \t -> do
	itext t 1 "12"
 ]

ghci5 :: Page
ghci5 = [\t -> do
	writeTopTitle t "ghci: ディレクトリを移動"
	text t "", \t -> do
	text t "* Ctrl + xでエディタを抜ける", \t -> do
	text t "* 新しいディレクトリtestを作成する", \t -> do
	itext t 1 "% mkdir test", \t -> do
	text t "* test.hsを新しいディレクトリに移動させる", \t -> do
	itext t 1 "% mv test.hs test/", \t -> do
	text t "* ghciのなかでディレクトリを移動することができる", \t -> do
	itext t 1 "*Main> :cd test/", \t -> do
	itext t 1 "*Main> :load test.hs", \t -> do
	itext t 1 "*Main> add 3 5", \t -> do
	itext t 1 "8"
 ]

ghciSummary :: Page
ghciSummary = [\t -> do
	writeTopTitle t "ghci: まとめ"
	text t "", \t -> do
	text t "* ターミナルを2つ用意してあるので", \t -> do
	text t "* その片方をエディタ用とし、片方を対話環境用にする", \t -> do
	text t "* するとエディタで編集し、ghciで:reloadをすれば", \t -> do
	itext t 1 "コードを見ながら実行することが可能となる", \t -> do
	text t "* 読み込むファイルはコマンドライン引数で指定するか", \t -> do
	itext t 1 "ghci内で:loadの引数として指定できる", \t -> do
	text t "* ディレクトリの移動はghciを抜けなくても:cdで可能"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* エディタ、シェル、対話環境の最低限の操作方法を学んだ", \t -> do
	text t "* とりあえずの編集作業はこれで可能かと思われるが", \t -> do
	text t "* 不明な点が出てきたらその都度質問していただきたい"
 ]

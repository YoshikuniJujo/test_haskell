tr第3.2回 nano、zsh、ghci
=========================

はじめに
--------

この回ではエディタ、シェル、Haskellの対話環境の操作を学ぶ。

エディタの操作についてはnanoエディタのみを扱う。

vimやEmacsユーザーは「いつも通り」に編集すれば良い。
シェルの操作はvi的なキーストロークを使用する。

ghcの対話環境についても学ぶ。

nano
----

### 起動と終了

まずはエディタを起動する。

    % nano -w

編集画面になったはずだ。

何もせずに終了する。

    Ctrl + x

 -wオプションはワードラップ処理を無効化している。
ワードラップ処理とは長い行に自動で改行を挿入する処理である。

### ファイル名を指定して起動

サンプルファイルを作る前にディレクトリを移動する。

    % cd ~/lectures/lecture00/
    % pwd
    /home/guest/lectures/lecture00/
    % touch test.txt

ファイル名を指定して起動する。

    % nano -w test.txt

### 保存と終了

適当な文字を入力してみよう。

    foo bar baz

保存する。

    Ctrl + o

保存するファイル名が尋ねられる。
起動の際に指定したファイル名がデフォルトとなっているので、
それで良ければreturnを押す。

終了する。

    Ctrl + x

確認してみる。

    % ls
    test.txt
    % cat test.txt
    foo bar baz

### カットとペースト

今のファイルを再度開く。

    % nano -w test.txt

ファイルの先頭に以下を追加する。

    qux quux corge[改行]

qux ...の行にカーソルを移動し

    Ctrl + k

foo ...の行の下にカーソルを移動し

    Ctrl + u

これでカット->ペーストができた。

### コピーとペースト

nanoにはコピーコマンドがないので、
カット->ペーストでコピーをエミュレートする。

foo ...の行にカーソルを移動し

    Ctrl + k -> Ctrl + u

qux ...の行の下にカーソルを移動し

    Ctrl + u

これでコピー->ペーストができた。

### 範囲選択

範囲選択を行うとカット->ペーストはより便利に使える。

カーソルをたとえばbarのbの前のスペースに移動させ

    Ctrl + 6

カーソルをquuxのxの後のスペースに移動させ

    Ctrl + k

カーソルをfoo ...の行の下に置き

    Ctrl + u

これで選択範囲のカット->ペーストができる。
カットではなくコピーをするなら、Ctrl + kをCtrl + k -> Ctrl + uとする。

Ctrl + o, Ctrl + xで終了しよう。

### まとめ

nanoの操作について学んだ。

* 起動はnano -w [ファイル名]
* 保存はCtrl + o
* 終了はCtrl + x
* 切り取りはCtrl + k
* 貼りつけはCtrl + u
* 範囲選択はCtrl + 6

zsh
---

### はじめに

今回用意した環境のシェルはzshである。
モードはViモードとしてある。
このモードでの簡単な操作方法を学ぼう。

### バックスペース

まずは普通にコマンドを打ち込んでみる。

    % pwd
    /home/guest/lectures/lecture00
    % ls
    test.txt
    % cat test.txtt
    (まだreturnは押さない)

tをひとつ余分に入力してしまったので、
バックスペースを入力する。

    Ctrl + h

正しいコマンドになったのでreturnを入力する。

### カーソルの移動

以下のようにコマンドを打つ。

    % cat ttest.txt
    (まだreturnは押さない)

ここでEscキーを押す
(Escキーは1の左にある)。

Escキーを押すとカーソルの移動モードになる。
このモードではhキーで左にlキーで右にカーソルが移動する。

hとlを利用して重複したtのうえにカーソルを動かし、
そこでxキーを押す。
xキーを押すとカーソル上の文字が1文字削除される。

正しいコマンドになったのでreturnを入力する。

### 文字の挿入

以下のようにコマンドを打つ。

    % cat st.t
    (まだreturnは押さない)

ここでEscキーを押す。
h, lキーでsのうえにカーソルを移動させ、iキーを押す。
iキーを押すと挿入モードになる。

    teを入力

Escキーでカーソル移動モードにもどる。
lキーで文字列の最後まで移動する。
aキーを押すと挿入モードになる。

    xtを入力

正しいコマンドになったのでreturnを入力する。

文字を挿入するためには、
移動モードから挿入モードにする必要がある。

* iキーはカーソル位置の直前に挿入
* aキーはカーソル位置の直後に挿入

### 履歴を使用する

echoコマンドで文字列を表示させる。

    % echo 'Hello, world!'
    "Hello, world!"

次に"Hllo, my friend!"と表示させたいとする。
このようなときに履歴機能が使える。

Escキーで移動モードに入る。
移動モードではkキーで履歴をさかのぼり、jキーで履歴を順送りする。

    kキーを3回押し、jキーを2回押す

履歴上の逆送りと順送りがされているのがわかるだろうか。

今コマンドは以下のようになっている。

    % echo 'Hello, world!'

worldの部分をmy friendに変えれば良い。
hキーとlキーでカーソルをwのところに動かし

    xキーを5回入力

これでworldが消える。
iキーで挿入モードに移行し以下を入力する。

    my friend

これで正しいコマンドになったのでreturnを入力する。

### まとめ

zshのViモードにおける
コマンド行の編集と履歴の利用について学んだ。

挿入モードとカーソル移動モードがある。

Escキーでカーソル移動モードに移行すると、
h, lキーでカーソルが移動する。
また、xキーで1文字削除が可能である。

i, aキーで挿入モードに移行できる。

移動モードでk, jキーを使って履歴内の移動が可能である。
履歴も、その場で打ち込んだコマンド同様に編集できる。

ghci
----

### はじめに

ghciコマンドでghcの対話環境を利用できる。

    % ghci
    Prelude>

まちがえた場合はCtrl + hで消していけば良い。

    Prelude> miss
    (まだreturnは入力しない)

Ctrl + hを4回入力する。

    Prelude>

対話環境を終了するには

    Prelude> :quit

### ファイルの読み込み

#### コマンドライン引数で指定

前準備としてtest.hsを作っておく。

    % nano -w test.hs
    add x y = x + y

Ctrl + oで保存する。
エディタは開いたままにしておく。
コマンドライン引数でファイルを指定できる。

別のターミナルで

    % cd ~/lectures/lecture00/
    % ghci test.hs
    *Main>

ちゃんと読み込めているか確認する。

    *Main> add 3 4
    7

#### ファイルの再読み込み

test.hsを開いているエディタで以下を追加する。

    sub x y = x - y

ghci側でのファイルの再読み込みが必要となる。

    *Main> :reload
    *Main> sub 8 2
    6

#### 対話環境内からの読み込み

エディタを抜ける。

    Ctrl + x

別のファイルを作る。

    % nano -w other.hs

以下を書き込む。

    mul x y = x * y

保存する。

    Ctrl + o

ghciからこのファイルを読み込む。

    *Main> :load other.hs
    *Main> mul 3 4
    12

### ディレクトリを移動

Ctrl + xでエディタを抜け、
新しいディレクトリtestを作成する。

    % mkdir test

test.hsを新しいディレクトリに移動させる。

    % mv test.hs test/

ghciのなかでディレクトリを移動することができる。

    *Main> :cd test/
    *Main> :!pwd
    /home/guest/lectures/lecture00/test
    *Main> :!ls
    test.hs
    *Main> :load test.hs
    *Main> add 3 5
    8

### まとめ

ターミナルを2つ用意してあるので、
その片方をエディタ用とし、片方を対話環境用にする。
すると、エディタで編集しghciで:reloadすれば、
コードを見ながら実行することが可能となる。

読み込むファイルはコマンドライン引数で指定するが、
ghci内で:loadの引数として指定できる。

ディレクトリの移動はghciを抜けなくても:cdで可能である。
cd以外のコマンドは:![コマンド]で実行できる。

まとめ
------

エディタ、シェル、対話環境の最低限の操作方法を学んだ。
とりあえずの編集作業はこれで可能かと思われるが、
不明な点が出てきたらその都度質問していただきたい。

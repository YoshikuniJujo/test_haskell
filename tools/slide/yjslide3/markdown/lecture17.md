第17回 モナドの復習と演習
=========================

はじめに
--------

前回はモナドについて学んだ。
すこし難しかったかもしれない。
今回はもうすこし簡単な例を見る。
また、演習問題を解くことで理解することを試みよう。

モジュールシステム
------------------

今まではモジュールについてあまり意識してこなかった。
しかし、すでに2つのモジュールに触れている。

* Prelude: 基本的な関数が定義されているモジュール
* Main: モジュール宣言を省略した場合のデフォルトのモジュール

モジュール宣言は以下のような形式となる。

    module [モジュール名] ([エクスポートリスト]) where

[エクスポートリスト]は名前を','で分けたリストである。
値構築子のエクスポートは特別な形になる。

    [型名]([値構築子名])

つまり以下のような定義においてBarをエクスポートするには

    data Foo = Bar

以下のようにする必要がある。

    module Baz (Foo(Bar)) where

ライオンの檻
------------

### はじめに

檻に入れたライオンをエクスポートするモジュールを作る。

ライオンは状態Hungry, Normal, Fullを持つことにする。
play関数でライオンは空腹の方向に変化し、
feed関数で満腹の方向に変化する。
ライオンを操作するときのみ檻から出し、
操作後は絶対に檻の外にいてはならない。

lectures/lecture17ディレクトリを作りLion.hsを作る。

### 必要な型の定義

ライオンの持つ状態を表す型を定義する。

    data State = Hungry | Normal | Full deriving Show

ライオンは名前と状態を持つことにする。

    type Name = String
    data Lion = Lion Name State deriving Show

これらをLion.hsに書きこむ。

檻を表す型を定義する。

    newtype Caged a = Caged a deriving Show

はじめのCagedは型構築子で2つめのCagedは値構築子である。
Caged aはa型の値をひとつ取る値構築子Cagedで作れるということ。

### Caged型をモナドにする

Caged型をモナドクラスのインスタンスにしてみよう。

    instance Monad Caged where
        return = Caged
        Caged x >>= f = f x

これらをLion.hsに書きこむ。

### ライオンを扱う関数

ライオンを生み出す関数を作る。
ライオンは檻のなかに生まれることにする。

    lion :: Name -> Caged Lion
    lion n = Caged $ Lion n Hungry

ライオンに餌を与える関数

    feed :: Lion -> Lion
    feed (Lion n Hungry) = Lion n Normal
    feed (Lion n _) = Lion n Full

ライオンと遊ぶ関数

    play :: Lion -> Lion
    play (Lion n Full) = Lion n Normal
    play (Lion n _) = Lion n Hungry

これらをLion.hsに書きこむ。

### モジュールとエクスポートリスト

コードの先頭にモジュール宣言をつける。

モジュール名はLionとする。
外部から使用する型はLionとCagedなのでこれらをエクスポートする。
値構築子のLionをエクスポートすると、
檻の外でライオンを生み出すことができてしまう。
それは危険なので値構築子Lionはエクスポートせず、
檻の中でライオンを生み出すlionをエクスポートする。
ライオンを扱う関数feed, playもエクスポートする。

よってモジュール宣言は以下のようになる。

    module Lion (Lion, Caged, lion, feed, play) where

これをLion.hsに書きこむ。

### 試してみる

#### アフリカにて

試してみよう。

    % ghci Lion.hs
    *Lion> Lion "danger" Hungry
    Lion "danger" Hungry

危ない!ライオンが檻の外にいる。
値構築子Lionはエクスポートしていないはずなのに?

モジュール名の前にある'*'がポイントである。
この'*'はそのモジュールの「内部」にいますよ、という意味である。
つまり、ライオンの生息地(多分アフリカ)にいるので、
檻の外にライオンがいてもおかしくない。

#### 文明社会にて

僕らは文明社会のなかでライオンと戯れたいと思う。
つまり、Lionモジュールの外にいてLionモジュールをimportしたい。

ghciでは以下のようにする。

    *Lion> :m Lion
    Prelude Lion>

PreludeとLionモジュールがエクスポートする関数が使える
「どこでもない場所」にいることになる。

    Prelude Lion> lion "Simba"
    Caged (Lion "Simba" Hungry)
    Prelude Lion> let simba = it
    Prelude Lion> feed simba

    <interactive>:5:6:
        Couldn't match expected type `Lion' with actual type `Caged Lion'
        In the first argument of `feed', namely `simba'
        In the expression: feed simba
        In an equation for `it': it = feed simba
    Prelude Lion> simba >>= feed

    <interactive>:6:11:
        Couldn't match type `Lion' with `Caged b0'
        Expected type: Lion -> Caged b0
          Actual type: Lion -> Lion
        In the second argument of `(>>=)', namely `feed'
        In the expression: simba >>= feed
        In an equation for `it': it = simba >>= feed
    Prelude Lion> simba >>= return . feed
    Caged (Lion "Simba" Normal)

餌を与えた後はちゃんと檻にもどしてあげる必要がある。
ライオンが操作後に檻にもどっていることは型レベルで保証されるということ。

### まとめ

檻に入れたライオンを輸出するモジュールを作った。
モナド関数では、何かを檻に入れることはできる。
また、檻から一時的に出すことはできるが、檻から出しっぱなしにすることはできない。

モジュールを使って内部構造を隠蔽することができることを見た。
CagedやLionの値構築子を輸出していないので、
内部構造は安全に変更することができる。

計算のログ(演習)
----------------

### はじめに

モナドとは単なる形式である。
以下の型の関数が存在しモナド則を満たせばすべてモナドである。

    m a -> (a -> m b) -> m b
    a -> m a

以下のように書いても型じことである。

    (a -> m b) -> (b -> m c) -> (a -> m c)
    (a -> b) -> (a -> m b)

モナドという性質を共有していてもその中身は様々である。
今回の演習では計算のログを取るモナドを組み立ててみる。

### Logger型

計算をしながら計算のログ(記録)を取っていくモナドを作る。
ログは文字列のリストとする。

例えば以下の関数があり

    toCode :: Char -> Logger Int

toCode 'c'はログとして["tocode 'c'"]を持ち、
計算の結果として99を持つ。

つまり、Logger型は文字列のリストと結果の値を持つ。

* 演習17-1. Logger型を定義せよ(1分)

今後の流れのために模範解答以外の解を出した人も
模範解答の定義を使うことにする。

また、deriving Showをつけたうえでlogger.hsに書きこむ。

### toCode関数

ログを残しつつ文字コードを求める関数

    toCode :: Char -> Logger Int

以下のような動作となる。

    toCode 'c' => Logger ["toCode 'c'"] 99

* 演習17-2. toCodeを定義せよ(1分)
    + (import Data.Char (ord)が必要)

解をlogger.hsに書きこみ、logger.hsの先頭に以下を追加する。

    import Data.Char (ord)

試してみる。

    Prelude Lion> :load logger.hs
    *Main> toCode 'c'
    Logger ["toCode 'c'"] 99

### tell関数

次に以下の関数を作ろうと思うのだが

    double :: Int -> Logger Int
    double 3 => Logger ["double 3"] 6

その前に文字列をログにする関数を書く。
これは以下のような型になるだろう。

    tell :: String -> Logger ()

ログのほうだけを扱う関数なのでLogger aのaの部分を()で埋めている。

* 演習17-3. tellを定義せよ(1分)

tellを使ってtoCodeを再定義してみる。

さっきのtoCodeの定義を以下のように書き換えよう。

    toCode c = tell ("toCode " ++ show c) >> return (ord c)

toCodeを「ログの追加」と「文字コードを返す」に分けて構築している。

### Loggerをモナドにする

#### 必要な関数

新しいtoCodeの定義はまだ使えない。
LoggerをMonadクラスのインスタンスにする必要がある。

以下の関数を定義する。

    return :: a -> Logger a
    (>>=) :: Logger a (a -> Logger b) -> Logger b

#### return

まずは簡単なほうから。

returnは「何もせずに」値を包み込む関数である。
Loggerについて言えば「ログを変化させずに」ということ。

ログを空にしておけば良い。

* 演習17-4. Loggerのreturnを定義せよ(1分)

解答のインスタンス宣言をlogger.hsに書きこみ、試してみる。

    *Main> :reload

    logger.hs:X:YY: Warning:
        No explicit method or default declaration for `>>='
        In the instance declaration for `Monad Logger'
    OK, modules loaded: Main.
    *Main> return 8 :: Logger Int
    Logger [] 8

問題なく定義できているようだ。

#### (>>=)

それでは(>>=)の定義に移ろう。

    (>>=) :: Logger a -> (a -> Logger b) -> Logger b

この関数に何をして欲しいのか考える。

* 第1引数のa型の値を第2引数である関数にわたして
* 出てきた結果について
    + ログのほうは第1引数のログに追加し
    + b型の値のほうは結果の値とする

* 演習17-5. Loggerの(>>=)を定義せよ(2分)

解答をlogger.hsのインスタンス宣言に追加する。

これで前に再定義したtoCodeが動くようになる。

    toCode c = tell ("toCode " ++ show c) >> return (ord c)

試してみる。

    *Main> :reload
    *Main> toCode 'c'
    Logger ["toCode 'c'"] 99

### double関数

次に以下のように整数を2倍するdoubleを考える。

    double :: Int -> Logger Int
    double 8 => Logger ["double 8"] 16

これはtoCodeと同様に定義できる。

* 演習17-6. doubleを定義せよ(1分)

解答をlogger.hsに書きこむ。

### toCodeDouble関数

toCodeとdoubleを使えば
「ログを記録しながら文字コードを2倍する関数toCodeDouble」が作れる。

* 演習17-7. toCodeDoubleを定義せよ(1分)

logger.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> toCodeDouble 'c'
    Logger ["toCode 'c'", "double 99"] 198

### まとめ

モナドとは型mについて
(a -> m b)型の関数を次々につなげられるという性質である。
中身は何であれ、その性質を満たせばモナドである。

結果とその過程を保存するモナドLoggerを作った。

    data Logger a = Logger [String] a

Monadクラスのインスタンスとした。

このモナドは関数適用の裏でログを結合している。

まとめ
------

ライオンをいれておく檻であるCaged型を作った。
Lion型やCaged型の値構築子を隠蔽することで、
檻の外でライオンを作れないようにし、
檻からライオンを出しっぱなしにできないようにした。
モナド関数のみを使うことで、
檻から出したライオンを檻にもどすことを強制できる。

演習では計算のログを保存するモナドを作った。
このモナドでは表の計算と裏でのログの結合とが行われる。

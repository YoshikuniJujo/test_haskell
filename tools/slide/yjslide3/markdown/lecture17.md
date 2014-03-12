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

まとめ
------

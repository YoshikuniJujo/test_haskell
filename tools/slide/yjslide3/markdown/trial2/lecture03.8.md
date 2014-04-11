トライアル 第3.8回 自習課題
===========================

はじめに
--------

演習では様々な多相関数を作成した。
これらの関数はプログラムのなかの共通する枠組みをくくり出したものである。
自習課題では同様の関数をもうひとつ作ることにする。

関数ascending
-------------

3つ引数を取り、その引数が昇順かどうかを確認する関数を考える。
3つの引数に重複は許さないものとする。
以下のようになるだろう。

    ascending :: Int -> Int -> Int -> Bool
    ascending x y z = x < y && y < z

~/lectures/lecture01/に移動しstudy.hsに書き込み、試してみる。

    % ghci study.hs
    *Main> ascending 3 8 10
    True
    *Main> ascending 2 9 7
    False
    *Main> ascending 4 4 9
    False

関数ascending12
---------------

同様のことを12で割った余りについて行うとする。
以下のようになるだろう。

    ascending12 :: Int -> Int -> Int -> Bool
    ascending12 x y z = ascending (x `mod` 12) (y `mod` 12) (z `mod` 12)

「3つの引数すべてに同じ変換をする」という構思がある。
この構造を抽出した関数on3について考えよう。

on3を使ってascending12を定義すると以下のようになる。

    ascending12 x y z = on3 (`mod` 12) x y z

* 自習課題. on3を定義せよ

参考
----

Windowsにghcをインストールするには以下のexeを実行すれば良い。

    http://www.haskell.org/platform/download/2013.2.0.0/
        HaskellPlatform-2013.2.0.0-setup.exe

演習7-4 (解答)
==============

問い
----

リストの長さを返す関数myLengthを定義せよ。

考えかた
--------

空リストかそれ以外かで場合分けする。

* 空リストの長さは0である
* 何かの値をリストxsに追加したリストの長さは「1 + リストxsの長さ」

要素の値自体はいじらないので型は何でも良い。
つまり引数の型は[a]となる。

解答
----

    myLength :: [a] -> Int
    myLength [] = 0
    myLength (_ : xs) = 1 + myLength xs

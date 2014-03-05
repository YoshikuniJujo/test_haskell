演習10-12(解答)
===============

問い
----

関数reverseを定義するための以下のような補助関数reverseIterを考える。

    reverse = reverseIter []

再帰を直接使ってreverseIterを定義せよ。

解説
----

reverseIter ys xsのように引数を与えた場合について考える。

* xsが空の場合(すべての山を移し終えた): ysを返す
* xsが空でない場合: xsの先頭要素をysに移す
* くりかえす

解答
----

    reverseIter ys [] = ys
    reverseIter ys (x : xs) = reverseIter (x : ys) xs

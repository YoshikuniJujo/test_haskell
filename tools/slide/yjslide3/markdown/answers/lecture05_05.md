第5回 演習5(解答)
==================

考えかた
--------

### zip tfs lst

tfsとのzipをとる。

    [(True, x), (False, y), (True, z), (False, w), (True, v) ...]

### filter fst $ zip tfs lst

これをタプルの第1要素が真か偽かでfilterする。

    filter (\(b, _) -> b == True) $ zip tfs lst

bがTrueのとき、True == Trueで結果はTrueとなり、
bがFalseのとき、Flase == Trueで結果はFalseとなるので、
b == Trueとbは同じもの。

    filter (\(b, _) -> b) $ zip tfs lst

\(b, _) -> bは2要素タプルの1番目をとる関数なのでfstとできる。

    filter fst $ zip tfs lst

これは以下のようになる。

    [(True, x), (True, z), (True, v) ...]

### snd

このリストの要素であるすべてのタプルから2番目を取り出せば良い。

    map snd $ filter fst $ zip tfs lst

解答
----

    hop lst = map snd $ filter fst $ zip tfs lst

これはlstから始まり返り値を次の関数の引数として渡していく形である。
よって(.)を使って書き直すことができる。

    hop = map snd . filter fst . zip tfs

コメント
--------

### filter fstにおける型

filterとfstの型は以下のようになっている。

    filter :: (a -> Bool) -> [a] -> [a]
    fst :: (b, c) -> b

filterの第1引数がfstであるので、
(a -> Bool)と((b, c) -> b)のあいだですりあわせが行われる。

    Bool = b
    a == (b, c) == (Bool, c)

その結果以下のような型となる。

    filter :: ((Bool, c) -> Bool) -> [(Bool, c)] -> [(Bool, c)]
    fst :: (Bool, c) -> Bool

全体としては以下のような型になる。

    filter fst :: [(Bool, c)] -> [(Bool, c)]

### (.)を使った定義を読み下す

    map snd . filter fst . zip tfs

これは以下のように読み下せる。

1. tfsとのzipをとり
2. タプルの1番目の要素でfilterし
3. すべてのタプルの2番目の要素をとる

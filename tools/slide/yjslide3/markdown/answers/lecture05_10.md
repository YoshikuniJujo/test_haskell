第5回 演習10 (解答)
===================

問い
----

getPi関数は点を打つ数とランダムの種を入力として、
円周率の、モンテカルロ法による、近似値を出力とする。

getPiの型を決めよ。

考えかた
--------

### 入力

点を打つ数は整数なのでIntとなる。
ランダムの種はStdGenで良い。

### 出力

結果は円周率の近似値なので、浮動小数点数で良い。
よってDoubleとなる。

解答
----

getPi :: Int -> StdGen -> Double

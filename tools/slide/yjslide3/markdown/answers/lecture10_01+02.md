演習10-1,2(解答)
================

前提
----

関数exp2について考える。この関数はInt型の値を扱い、
引数nについて、2のn乗の値を返す。
また、関数の定義に(^)は使わないこととする。

演習10-1
--------

### 問い

関数exp2の型を求めよ。

### 解答

    exp2 :: Int -> Int

演習10-2
--------

### 問い

再帰を直接使って関数exp2を定義せよ。

### 解答

    exp2 0 = 1
    exp2 n = 2 * exp2 (n - 1)

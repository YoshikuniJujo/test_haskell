演習10-4,5(解答)
================

前提
----

関数oddSumはリストの要素のうちの奇数の総和を返す。

型
--

oddSum :: [Int] -> Int

演習10-4
--------

### 問い

sum, filterを使ってoddSumを定義せよ。

### 解答

    oddSum = sum . filter odd

演習10-5
--------

### 問い

再帰を直接使ってoddSumを定義せよ。

### 解答

    oddSum [] = 0
    oddSum (n : ns)
        | odd n = n + oddSum ns
        | otherwise = oddSum ns

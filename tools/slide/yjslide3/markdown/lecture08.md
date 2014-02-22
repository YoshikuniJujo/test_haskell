第8回 リストを作る再帰関数
==========================

はじめに
--------

前回はリストを引数に取る再帰関数について学んだ。
今回はリストを返す再帰関数について学ぶ。

「リスト」の回でやった3段階は

1. enumerate
2. map/filter
3. accumulate

であるが、そのうちの1. enumerateにあたる部分となる。

[m .. n]
--------

### 構文糖

「リスト」の回で見た[m .. n]という構文を見る。
これは構文糖であり脱糖すると以下の関数になる。

    enumFromTo m n

### 型

この関数の型を正確に理解するためには後でやる型クラスの知識が必要になる。
今回はとりあえず以下のように理解しておこう。

    enumFromTo :: Int -> Int -> [Int]

### 定義

enumFromTo n mはmから1刻みでnまでのリストを返す関数であり、
以下のように定義できる。

    enumFromTo m n
        | m > n = []
        | otherwise = m : enumFromTo (m + 1) n

### 試してみる

編集用と対話環境用にコマンドプロンプトを2個立ち上げる。

lectures/lecture08ディレクトリを作成し、そこに移動。
myList.hsを作成して以下を書き込む。

    myEnumFromTo :: Int -> Int -> [Int]
    myEnumFromTo m n
        | m > n = []
        | otherwise = m : enumFromTo (m + 1) n

試してみる。

    % ghci myList.hs
    *Main> myEnumFromTo 3 8
    [3,4,5,6,7,8]

### 関数myEnumFromToの説明

関数myEnumFromToの定義は以下のように読むことができる。

1. 開始の値が終了の値よりも大きければ空リスト
2. mからnまでの値のリストは
    * (m + 1)からnまでの値のリストにmを追加したもの

### まとめ

[m .. n]という構文は構文糖であり、脱糖すると以下のようになる。

    enumFromTo m n

enumFromTo m nは以下のように定義される。

1. m > n ならば空リスト
2. そうでなければm + 1からnのリストにmを追加したもの

リストを作成する関数の多くが同様の枠組みで作られる。
その枠組みとは、その関数自体の返り値に値を追加するというもの。

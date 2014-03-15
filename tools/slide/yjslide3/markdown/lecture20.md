第20回 まとめ:オセロ(盤)
==============================

はじめに
--------

Haskellの基本をだいたい学んだ。
最後にオセロを作って終わりにしよう。
難しい部分があるかもしれないが、
全体の雰囲気がつかめれば良いことにしよう。

4回に分けて作っていく。
今回はゲームの盤面を定義する。

モジュール構成
--------------

以下のようなモジュールに分ける。

    Main, Window, AI, Game, Board, Tools

それぞれのモジュールは以下のようになっている。

* Main: Windowの関数を呼び出すだけ
* Window: AIとGameの関数をGUI上で結合する
* AI: 与えられたゲームの状態からコンピュータの手を計算
* Game: ゲームの状態を表現する型と操作する関数を定義
* Board: 盤の状態を表現する型と操作する関数を定義
* Tools: より一般的に使えそうな関数を定義

今回はBoardとToolsの一部を作る。

Toolsモジュール
---------------

### はじめに

lectures/othelloディレクトリを作成する。
まずは、Boardモジュールで使用する、より一般的な道具を作成する。

Tools.hsに以下を書きこむ。

    module Tools (
    ) where

対話環境でテストしながら作っていくので

    % ghci Tools.hs
    *Tools>

ここで定義するBoardモジュールで使う道具は以下の4つである。

* scc: 引数で与えた値の「次の値」を返す
    + 「次の値」がない、つまり最大値ではNothingを返す
* prd: 引数で与えた値の「前の値」を返す
    + 「前の値」がない、つまり最小値ではNothingを返す
* foldlMaybe: foldlと同様だがJustが1つもなければNothingとなる
* modifyList: リストの要素の指定された1つに関数を適用する

### scc, prd

次の値や前の値を返す関数succ, predははじめから定義されている。
しかし、これらの関数は結果が最大値や最小値を越える場合にはエラーとなる。

エラーとなる引数の値を持つ関数は扱いにくい。
エラーとなるよりも明示的にNothingを返す関数のほうが扱いやすいので、
succ, predの代わりとなるscc, prdを定義することにする。

Tools.hsに以下を書きこむ。

    scc, prd :: (Ord a, Enum a, Bounded a) => a -> Maybe a
    scc x
        | x < maxBound = Just $ succ x
        | otherwise = Nothing
    prd x
        | x > minBound = Just $ pred x
        | otherwise = Nothing

succやpredはEnumクラスのクラス関数であり、
maxBoundやminBoundはBoundedクラスのクラス変数である。
また(<)や(>)はOrdクラスのクラス関数である。

Enumクラスは順番に並べられることを保証し、
Boundクラスは最大値と最小値が存在することを保証する。
Ordクラスは大小比較可能ということである。

つまり、sccやprdは順番に並べられ、上限と下限があり、
大小比較可能な型に対して定義されているということ。

### foldlMaybe

### modifyList

Boardモジュール
---------------

### はじめに

### Disk型

### Square型

### Board型

### X, Y型

### Direction型

### 関数定義の見通し

### 最下層の関数

#### 関数get

#### 関数put, cap

##### 関数modifySquare

##### 関数put

##### 関数cap

### 中間層の関数

#### はじめに

#### capture1Bool

#### capture1

#### capture

### 上部層の関数

#### はじめに

#### 関数disks

#### 関数place

#### 関数placeable

### エクスポートリスト

### 試してみる

まとめ
------

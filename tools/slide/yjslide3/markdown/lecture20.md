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

### 関数scc, prd

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

### 関数foldlMaybe

#### 導入

オセロでは8方向のうちのどこかで相手の石を取れる場所以外には、
自分の石を置くことができない。
また、石を置いた結果の状態は8方向の取れる石すべてを取った状態となる。

ある1方向について、取れるときは取った状態を返し、
取れないときはNothingを返す関数があるとする。

するとこの関数を8方向すべてについて試みて、ひとつでもJustがあれば、
Justを返すすべての方向について、その関数を適用した結果を返したい。

より一般的にすると、与えられた値のリストについて、
関数を次々に適用していき、ひとつでもJustがあれば、
関数適用の最終的な結果を返し、
すべての結果がNothingであったら全体の結果もNothingとする関数となる。

以下の型の関数を作っていく。

    foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a

オセロの場合を例にするならば、aは盤の状態であり、bはそれぞれの方向となる。
状態をそれぞれの方向について変化させていく。
変化がない(つまりJustがない)ときは全体の値もNothingとなる。

#### 関数foldlMaybeBool

##### Justの有無を表すBool値

foldlMaybe op x (y : ys)として、x `op` yがNothingならば、
そのyの値は無視し、Just x'ならばx'を新しい値として続ける、とする。

これでほとんどの場合はうまくいくが、すべての結果がNothingだった場合に、
全体の値がNothingとなるのではなく、もともとの値xを返すということになる。

すべてNothingの場合に結果もNothingとするためには、
関数適用の途中でJustが出てきたかどうかを保存するBool値が必要になる。
そのBool値を引数に追加した関数を作ろう。

    foldlMaybeBool :: Bool -> (a -> b -> Maybe a) -> a [b] -> Maybe a

Bool値はJustがあった場合にTrue, 無かった場合にはFalseとなる。

##### 関数定義

まずはすべての値を使い終わったときについて考える。
このとき、それまでの関数適用について、Justがあったなら現在の値を返し、
JustがなかったらNothingを返せば良い。

    foldlMaybeBool True _ x [] = Just x
    foldlMaybeBool False _ _ [] = Nothing

値が残っている場合には、
x `op` yがJust x'ならばBool値をTrueにしてxをx'にする。
x `op` yがNothingならyは無視しBool値もxもそのままとなる。

    foldlMaybeBool j op x (y : ys) = case x `op` y of
        Just x' -> foldlMaybeBool True op x' ys
        _ -> foldlMaybeBool j op x ys

#### 関数foldlMaybeの定義

foldlMaybeはfoldlMaybeBoolに初期値としてFalseを与えれば良い。
以下をTools.hsに書きこむ。

    foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
    foldlMaybe = foldlMaybeBool False

    foldlMaybeBool :: Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a
    foldlMaybeBool True _ x [] = Just x
    foldlMaybeBool False _ _ [] = Nothing
    foldlMaybeBool j op x (y : ys) = case x `op` y of
        Just x' -> foldlMaybeBool True op x' ys
        _ -> foldlMaybeBool j op x ys

### 関数modifyList

modifyListはリストの指定された位置にある要素を変化させる関数である。
前の部分、その要素、後の部分に分けて、
前の部分、要素を変化させたもの、後の部分をつなげる。

Tools.hsに以下を書きこむ。

    modifyList :: [a] -> Int -> (a -> a) -> [a]
    modifyList xs n f = take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs

takeは与えられた数だけ要素を取り出す。
dropは与えられた数だけ要素を捨てる。
(!!)は与えられたアンデックスで要素を取り出す。

### エクスポートリスト

作った関数をエクスポートリストに追加する。

    module Tools (
        scc, prd, foldlMaybe, modifyList
    ) where

ケアレスミスをチェックするためにreloadする。

    *Tools> :reload

### 試してみる

いくつか試してみる。

    *Tools> scc 'c'
    Just 'd'
    *Tools> scc '\0'
    Just '\SOH'
    *Tools> prd '\0'
    Nothing
    *Tools> :m + Data.Char
    *Tools Data.Char> modifyList "Hello" 2 toUpper
    "HeLlo"

Boardモジュール
---------------

### はじめに

ToolsモジュールにBoardモジュールで使う一般的な道具を作ったので、
次はBoardモジュールの作成に移る。

まずはモジュール宣言を書きこむ。

    module Board (
    ) where

エクスポートリストは今は空である。
これから()のなかにエクスポートする関数等を追加していく。

対話環境でテストしながら作っていこう。

    % ghci Board.hs
    *Board>

### Disk型

石を表すデータ構造を作る。以下をBoard.hsに書きこむ。

    data Disk = Black | White deriving (Eq, Show)

    rev :: Disk -> Disk
    rev Black = White
    rev White = Black

石には黒と白とがありそれぞれ互いの裏となっているということ。

### Square型

次は盤のマスを表すデータ構造を作る。
以下をBoard.hsに書きこむ。

    data Square = Disk { disk :: Disk } | Empty deriving (Eq, Show)

マスは石が置いてあるか、または空である。
Disk { disk :: Disk }という定義は以下の同じことである。

    Disk Disk

1つめのDiskは値構築子であり2つめのDiskは型の名前である。
また、同時にSquareからDiskを取り出すdisk関数を定義している。

マスに石が置いてあるかどうかを調べる関数。

    isDisk :: Square -> Bool
    isDisk (Disk _) = True
    isDisk _ = False

石を変化させる関数。

    modifyDisk :: (Disk -> Disk) -> Square -> Square
    modifyDisk f (Disk d) = Disk $ f d
    modifyDisk _ s = s

### Board型

#### 型の定義

マスの横の並びを行とすると、盤は行を集めたものである。
これを定義する。
また、対話環境用にshow関数を定義しておこう。

以下をBoard.hsに書きこむ。

    newtype Board = Board [[Square]]

    instance Show Board where
        show (Board b) = unlines $ map (concatMap sd) b
            where
            sd (Disk Black) = "*|"
            sd (Disk White) = "O|"
            sd Empty = "_"

#### initBoard

盤の初期状態を定義する。

    initBoard :: Board
    initBoard = Board $ map (map c2d) ["
        "________",
        "________",
        "________",
        "___O*___",
        "___*O___",
        "________",
        "________",
        "________" ]
        where
        c2d '_' = Empty
        c2d '*' = Disk Black
        c2d 'O' = Disk White

#### 試してみる

試してみる。

    *Tools> :load Board.hs
    *Board> initBoard
    _|_|_|_|_|_|_|_|
    _|_|_|_|_|_|_|_|
    _|_|_|_|_|_|_|_|
    _|_|_|O|*|_|_|_|
    _|_|_|*|O|_|_|_|
    _|_|_|_|_|_|_|_|
    _|_|_|_|_|_|_|_|
    _|_|_|_|_|_|_|_|

盤の初期状態が表示される。

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

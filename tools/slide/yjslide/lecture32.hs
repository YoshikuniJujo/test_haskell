import Lecture

subtitle :: String
subtitle = "第32回 辞書"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	listDict, listDict2, listDict3, listDict4, listDict5, listDict6,
	listDictSummary,
	hashTable, hashTable2, hashTable3, hashTable4, hashTable5, hashTable6,
	hashTable7, hashTable8, hashTableSummary,
	treeDict, treeDict2, treeDict3, treeDict4, treeDict5, treeDict6,
	treeDictSummary,
	attention, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* キーと値を組にする辞書という構造がある", \t -> do
	text t "* 最も素朴なアルゴリズムは以下の通り", \t -> do
	itext t 1 "- (キー, 値)ペアのリストを作り", \t -> do
	itext t 1 "- キーをひとつずつ見ていき探したいキーを見つける", \t -> do
	itext t 1 "- それとペアになっている値を得る", \t -> do
	text t "* これにはO(n)時間がかかる", \t -> do
	text t "* より効率の良い方法としてハッシュテーブルがある", \t -> do
	text t "* ハッシュテーブルと競合可能なより関数型的なMap型がある"
 ]

listDict :: Page
listDict = [\t -> do
	writeTopTitle t "リストを辞書として使う"
	text t "", \t -> do
	text t "* 一番簡単な方法", \t -> do
	text t "* わかりやすい", \t -> do
	text t "* 何をしているかが一目瞭然", \t -> do
	text t "* 辞書の要素が大きくならないことが明らかなとき使える", \t -> do
	itext t 1 "- ソフトウェアは意外な使われかたをするもの", \t -> do
	itext t 1 "- 手元で一度だけ使う使い捨てのプログラムならあり", \t -> do
	text t "* きちんと抽象化しておけば", \t -> do
	itext t 1 "- あとでより効率のよいMap型にさしかえ可"
 ]

listDict2 :: Page
listDict2 = [\t -> do
	writeTopTitle t "リストを辞書として使う"
	text t "", \t -> do
	text t "* データの構造は以下のようになる", \t -> do
	itext t 1 "type ListDict k v = [(k, v)]", \t -> do
	text t "* リストリテラルを使って辞書の初期値を設定できる", \t -> do
	itext t 1 "age :: ListDict String Int"
	itext t 1 "age = [(\"Tarou\", 33), (\"Hanako\", 28)]", \t -> do
	text t "* 値の取得はPreludeにあるlookup関数が使える", \t -> do
	itext t 1 "lookup :: Eq a => a -> [(a, b)] -> Maybe b"
	itext t 1 "> lookup \"Hanako\" age"
	itext t 1 "Just 28", \t -> do
	text t "* 値の追加は単純に頭に(キー, 値)ペアを足せば良い", \t -> do
	itext t 1 "age' = (\"Jirou\", 29) : age"
 ]

listDict3 :: Page
listDict3 = [\t -> do
	writeTopTitle t "リストを辞書として使う"
	text t "", \t -> do
	text t "* 値の削除は", \t -> do
	itext t 1 "age' = filter ((/= \"Hanako\") . fst) age", \t -> do
	text t "* 値の変更は?", \t -> do
	itext t 1 "- 与えられたキーを持つペアを削除して", \t -> do
	itext t 1 "- 新しい値を追加する", \t -> do
	text t "* としても良いが、", \t -> do
	itext t 1 "- lookupは見つかったはじめの要素を返すので", \t -> do
	itext t 1 "age' = (\"Hanako\", 29) : age"
 ]

listDict4 :: Page
listDict4 = [\t -> do
	writeTopTitle t "リストを辞書として使う"
	text t "", \t -> do
	text t "* リストを辞書として使った場合の効率", \t -> do
	itext t 1 "追加: O(1)"
	itext t 1 "削除: O(n)"
	itext t 1 "更新: O(1)"
	itext t 1 "検索: O(n)"
 ]

listDict5 :: Page
listDict5 = [\t -> do
	writeTopTitle t "リストを辞書として使う"
	text t "", \t -> do
	text t "* プロファイリング用のコード", \t -> do
	itext t 1 "dict :: [(String, Int)]"
	itext t 1 "dict = map (\\i -> (show i, i)) [0 .. 10 ^ 6]"
	itext t 1 ""
	itext t 1 "randomDict :: IO (Maybe Int)"
	itext t 1 "randomDict = do"
	itext t 2 "k <- show <$> randomRIO (0, 10 ^ 6)"
	itext t 2 "return $ lookup k dict"
	itext t 1 ""
	itext t 1 "main :: IO ()"
	itext t 1 "main = 1000 `timesDo` (randomDict >>= print)"
 ]

listDict6 :: Page
listDict6 = [\t -> do
	writeTopTitle t "リストを辞書として使う"
	text t "", \t -> do
	text t "* timesDoの定義", \t -> do
	itext t 1 "timesDo :: Int -> IO () -> IO ()"
	itext t 1 "0 `timesDo` _ = return ()"
	itext t 1 "n `timesDo` io = io >> ((n - 1) `timesDo` io)", \t -> do
	text t "* 「人工的な例」の説明", \t -> do
	itext t 1 "- 0から100万までの文字列から数への辞書を作り", \t -> do
	itext t 1 "- それを1000回ランダムなキーで検索している", \t -> do
	text t "* これにかかる時間が手元の環境で22.87秒"
 ]

listDictSummary :: Page
listDictSummary = [\t -> do
	writeTopTitle t "リストを辞書として使う(まとめ)"
	text t "", \t -> do
	text t "* 非常に単純な構造", \t -> do
	text t "* 何をしているのか一目瞭然", \t -> do
	text t "* 使い捨てのプログラムに使う", \t -> do
	text t "* 項目の追加はO(1)で行える", \t -> do
	text t "* 項目の検索がO(n)かかる", \t -> do
	text t "* 作られた辞書を使う回数が少なければ効率的", \t -> do
	text t "* 一般的に辞書は作る回数にくらべて検索する回数が多い", \t -> do
	text t "* たいていの用途でこのやりかたは非効率となる"
 ]

hashTable :: Page
hashTable = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* 多くのスクリプト言語で辞書の実装はハッシュテーブル", \t -> do
	text t "* ほとんど「辞書」と「ハッシュ」が同義語になっている", \t -> do
	text t "* ハッシュテーブルは値の書き換えを伴うアルゴリズム", \t -> do
	text t "* 機能強化した配列と言える", \t -> do
	text t "* 値の追加、削除、検索がO(1)で行える", \t -> do
	text t "* 仕組みとしては以下の通り", \t -> do
	itext t 1 "- キーから整数値を計算", \t -> do
	itext t 1 "- その整数値をインデックスとして配列を検索", \t -> do
	text t "* キーから整数値を計算する関数をハッシュ関数と呼ぶ"
 ]

hashTable2 :: Page
hashTable2 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* 文字列から整数値を得るハッシュ関数を考える", \t -> do
	text t "* 文字列はほとんど無限に存在する", \t -> do
	text t "* 整数値は0から配列のサイズなのである程度限られた大きさ", \t -> do
	text t "* ハッシュ関数は本質的に重複の可能性がある", \t -> do
	text t "* ハッシュ値が重複した場合の動作は複数考えられる", \t -> do
	text t "* 同一ハッシュ値をまとめたリストというやりかたがある", \t -> do
	text t "* 他のやりかたでも重複した要素数に対してO(n)時間", \t -> do
	text t "* ハッシュ値はできるだけ重複しないように", \t -> do
	itext t 1 "- 均等に分布するような関数であることが望ましい"
 ]

hashTable3 :: Page
hashTable3 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* ハッシュテーブルは基本的にO(1)のアルゴリズム", \t -> do
	text t "* しかし要素数が増えてくるとハッシュ値が重複してくる", \t -> do
	text t "* 要素数が非常に大きくなってくるとO(n)に近づいていく"
 ]

hashTable4 :: Page
hashTable4 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* baseパッケージにData.HashTableモジュールがある", \t -> do
	text t "* 現在では非推奨(deprecated)扱い", \t -> do
	text t "* GHC 7.8では削除が予定されている", \t -> do
	text t "* 今後は別のパッケージを使う必要がある", \t -> do
	text t "* 理由は、次に説明する木構造を使ったMap型が", \t -> do
	itext t 1 "- より関数型的に扱え", \t -> do
	itext t 1 "- パフォーマンス的にも競合可能だから", \t -> do
	text t "* 別のパッケージになっても本質的な使いかたは変わらない", \t -> do
	text t "* Data.HashTableの使いかたを見ていこう"
 ]

hashTable5 :: Page
hashTable5 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* 型は以下のようになっている", \t -> do
	itext t 1 "HashTable key val", \t -> do
	text t "* リストからの変換で初期値が設定できる", \t -> do
	itext t 1 "fromList :: Eq key => (key -> Int32) ->"
	itext t 2 "[(key, val)] -> IO (HashTable key val)", \t -> do
	itext t 1 "- ハッシュ関数を指定する必要がある", \t -> do
	itext t 1 "- 文字列用のハッシュ関数は用意されている", \t -> do
	itext t 1 "- 値の変更を伴うので、IOモナド内で使うことになる", \t -> do
	itext t 1 "age <- fromList hashString $"
	itext t 2 "[(\"Tarou\", 33), (\"Hanako\", 28)]"
 ]

hashTable6 :: Page
hashTable6 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* 値の取得にはData.HashTable.lookupを使う", \t -> do
	itext t 1 "lookup age \"Hanako\"", \t -> do
	text t "* 値の追加にはinsertを使う", \t -> do
	itext t 1 "insert age \"Jirou\" 29", \t -> do
	text t "* 値の削除にはdeleteを使う", \t -> do
	itext t 1 "delete age \"Hanako\"", \t -> do
	text t "* 値の変更にはupdateを使う", \t -> do
	itext t 1 "update age \"Hanako\" 29"
 ]

hashTable7 :: Page
hashTable7 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* プロファイリング用のコード", \t -> do
	itext t 1 "randomDict ::"
	itext t 2 "HashTable String Int -> IO (Maybe Int)"
	itext t 1 "randomDict dict = do"
	itext t 2 "k <- show <$> randomRIO (0, 10 ^ 6)"
	itext t 2 "lookup k dict"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "dict <- fromList hashString $"
	itext t 3 "map (\\ i -> (show i, i)) [0 .. 10 ^ 6]"
	itext t 2 "1000 `timesDo` (randomDict dict >>= print)"
 ]

hashTable8 :: Page
hashTable8 = [\t -> do
	writeTopTitle t "ハッシュテーブル"
	text t "", \t -> do
	text t "* 0から100万までの文字列から整数への辞書を作り", \t -> do
	text t "* ランダムなキーで1000回検索した", \t -> do
	text t "* 手元の環境では0.42秒", \t -> do
	itext t 1 "- リストを使った場合の22.87秒と比べて大幅な改善", \t -> do
	text t "* 次のMap型との比較のために検索回数を100万回とすると", \t -> do
	itext t 1 "- 44.84秒そのうちrandomDictが12.7%の時間", \t -> do
	itext t 1 "- 文字列の表示に多くの時間が費されている", \t -> do
	itext t 1 "- よってより参考になる時間はrandomDictの5.69秒", \t -> do
	text t "* 大幅な改善が見られるが", \t -> do
	itext t 1 "- キーの型ごとにハッシュ関数を作る必要がある", \t -> do
	itext t 1 "- 状態変化が生じるため堅牢性が低下する"
 ]

hashTableSummary :: Page
hashTableSummary = [\t -> do
	writeTopTitle t "ハッシュテーブル(まとめ)"
	text t "", \t -> do
	text t "* 値の検索がO(1)で可能", \t -> do
	text t "* ただしテーブルが巨大になるとO(n)に近づいていく", \t -> do
	text t "* 値の変化を伴うアルゴリズムなのでIOモナド内で", \t -> do
	text t "* 純粋でない関数を多用するので", \t -> do
	itext t 1 "- プログラムの堅牢性が低下", \t -> do
	itext t 1 "- Haskeller的にはくやしい", \t -> do
	text t "* ちなみに状態変化を使うアルゴリズムは", \t -> do
	itext t 1 "- もちろん使うことができる", \t -> do
	itext t 1 "- 使うときにはその危険性が目に見える", \t -> do
	text t "* というのがHaskellの良さ", \t -> do
	text t "* 幸い「辞書」にはより関数型的な代替手段がある"
 ]

treeDict :: Page
treeDict = [\t -> do
	writeTopTitle t "辞書に木を使う"
	text t "", \t -> do
	text t "* 十分に実用的な効率で", \t -> do
	text t "* 状態変化を伴わずに辞書を実現するには", \t -> do
	text t "* 二分探索木を使う", \t -> do
	text t "* キーは大小比較ができる必要がある", \t -> do
	itext t 1 "- Ord k => ...ということ", \t -> do
	text t "* 二分探索木はソート済みの構造なので", \t -> do
	itext t 1 "- 木のトップから値の比較を始め", \t -> do
	itext t 1 "- 探している値のほうが大きければ右へ", \t -> do
	itext t 1 "- 小さければ左へ木を降りていき", \t -> do
	itext t 1 "- 等しいところの値を返せば良い"
 ]

treeDict2 :: Page
treeDict2 = [\t -> do
	writeTopTitle t "辞書に木を使う"
	text t "", \t -> do
	text t "* 平衡の保たれた木であれば", \t -> do
	text t "* n個の値に対して木の高さはlog nとなる", \t -> do
	text t "* よって値の探索にはlog n回の比較をすれば良い", \t -> do
	text t "* HaskellではData.MapモジュールにMap型が定義されている", \t -> do
	text t "* Map型には前回の「重み平衡木」のアルゴリズムを採用"
 ]

treeDict3 :: Page
treeDict3 = [\t -> do
	writeTopTitle t "辞書に木を使う"
	text t "", \t -> do
	text t "* リストからの変換で新しい木を作る", \t -> do
	itext t 1 "dict = fromList [(\"Tarou\", 33), (\"Hanako\", 28)]", \t -> do
	text t "* 値の検索にはData.Map.lookupを使う", \t -> do
	itext t 1 "lookup \"Hanako\" dict", \t -> do
	text t "* 値の追加にはinsertを使う", \t -> do
	itext t 1 "insert \"Jirou\" 29 dict", \t -> do
	text t "* 値の削除にはdeleteを使う", \t -> do
	itext t 1 "delete \"Hanako\" dict", \t -> do
	text t "* 値の更新にもinsertが使える", \t -> do
	itext t 1 "insert \"Hanako\" 29 dict"
 ]

treeDict4 :: Page
treeDict4 = [\t -> do
	writeTopTitle t "辞書に木を使う"
	text t "", \t -> do
	text t "* 前値を利用した値の更新にはadjustが使える", \t -> do
	itext t 1 "adjust (+ 1) \"Hanako\" dict", \t -> do
	text t "* その他、値の更新にkeyを利用する等のバリエーションが"
 ]

treeDict5 :: Page
treeDict5 = [\t -> do
	writeTopTitle t "辞書に木を使う"
	text t "", \t -> do
	text t "* プロファイリング用のコード", \t -> do
	itext t 1 "dict :: Map String Int"
	itext t 1 "dict = fromList $"
	itext t 2 "map (\\i -> (show i, i)) [0 .. 10 ^ 6]"
	itext t 1 "randomDict :: IO (Maybe Int)"
	itext t 1 "randomDict = do"
	itext t 2 "k <- show <$> randomRIO (0, 10 ^ 6)"
	itext t 2 "return $ M.lookup k dict"
	itext t 1 "main :: IO ()"
	itext t 1 "main = (10 ^ 6) `timesDo` (randomDict >>= print)"
 ]

treeDict6 :: Page
treeDict6 = [\t -> do
	writeTopTitle t "辞書に木を使う"
	text t "", \t -> do
	text t "* 0から100万までの文字列から数への辞書を作り", \t -> do
	text t "* ランダム値で100万回検索している", \t -> do
	text t "* これにかかる時間が", \t -> do
	itext t 1 "- 54.79秒で、そのうちの17.3%がrandomDict", \t -> do
	itext t 1 "- よってrandomDictに費された時間は9.48秒", \t -> do
	itext t 1 "- ハッシュリストを使ったものは5.69秒", \t -> do
	text t "* O(log n)なので単純計算すると", \t -> do
	itext t 1 "- 項目数が1兆になっても100万回の検索が20秒", \t -> do
	itext t 1 "- ハッシュリストでその数だとO(n)に近くなる", \t -> do
	itext t 1 "- むしろ木を使ったほうが速くなるかもしれない", \t -> do
	text t "* つまりHashよりもMapのほうがスケールする"
 ]

treeDictSummary :: Page
treeDictSummary = [\t -> do
	writeTopTitle t "辞書に木を使う(まとめ)"
	text t "", \t -> do
	text t "* 状態変化を伴わない効率的な辞書が作れた", \t -> do
	text t "* 平衡2分探索木を利用している", \t -> do
	text t "* 平衡を保つアルゴリズムには「重み平衡木」を利用", \t -> do
	text t "* 一般的なレベルの項目数ではハッシュリストに勝てない", \t -> do
	itext t 1 "- しかし十分に競合可能な効率ではある", \t -> do
	text t "* モンスターマシンでの作業ではハッシュリストに勝てる", \t -> do
	itext t 1 "- とくに大量のメモリを積んだマシンでの作業", \t -> do
	itext t 1 "- ハッシュリストも実装を変えればスケールする", \t -> do
	itext t 1 "- Mapを使えば「そのままの」実装でスケールする", \t -> do
	text t "* 何よりも不要な「状態変化」から逃れられたことが大きい"
 ]

attention :: Page
attention = [\t -> do
	writeTopTitle t "注意事項"
	text t "", \t -> do
	text t "* Data.MapはPreludeと同名の関数を多くエクスポート", \t -> do
	text t "* このタイプのモジュールを使うときは修飾名を使うと良い", \t -> do
	itext t 1 "import qualified Data.Map as M"
	itext t 1 "M.lookup ...", \t -> do
	text t "* あるいはより説明的に", \t -> do
	itext t 1 "import qualified Data.Map as Map"
	itext t 1 "Map.lookup ..."
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* リストによる実装は純粋だが効率が悪い", \t -> do
	text t "* ハッシュテーブルによる実装は効率は良いが不純だ", \t -> do
	text t "* 平衡2分探索木による実装は純粋でしかも効率が良い", \t -> do
	text t "* 辞書を使いたいとき", \t -> do
	itext t 1 "- まずはリストを使って実装", \t -> do
	itext t 1 "- そのとき十分に抽象化しておく", \t -> do
	itext t 1 "- 効率が問題になった段階でMapにさしかえる", \t -> do
	text t "* 関数型言語において純粋さと効率を両立できるのは木構造", \t -> do
	text t "* 木は関数型言語の陰の主役と言える"
 ]

import Lecture

subtitle :: String
subtitle = "第31回 Map型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	listDict, listDict2, listDict3, listDict4, listDict5, listDict6,
	listDictSummary
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
	text t "* 人工的な例"
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
	text t "* timesDoの定義"
	itext t 1 "timesDo :: Int -> IO () -> IO ()"
	itext t 1 "0 `timesDo` _ = return ()"
	itext t 1 "500 `timesDo` io = io >> ((n - 1) `timesDo` io)", \t -> do
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
	text t "* 使いすてのプログラムに使う", \t -> do
	text t "* 項目の追加はO(1)で行える", \t -> do
	text t "* 項目の検索がO(n)かかる", \t -> do
	text t "* 作られた辞書を使う回数が少なければ効率的", \t -> do
	text t "* 一般的に辞書は作る回数にくらべて検索する回数が多い", \t -> do
	text t "* たいていの用途でこのやりかたは非効率となる"
 ]

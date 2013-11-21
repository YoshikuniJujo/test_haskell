import Lecture

subtitle :: String
subtitle = "第18回 Showクラス"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, prelude2, prelude3,
	showClass, showClass2,
	animal, animal2, animal3, animal4, animal5
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Showクラスについて見ていく", \t -> do
	itext t 1 "- 値を表示するためのクラス", \t -> do
	text t "* 新しい概念は出てこない", \t -> do
	itext t 1 "- 今までの概念の組み合わせかたを学ぶ", \t -> do
	text t "* 基本は簡単", \t -> do
	itext t 1 "- Stringへの変換を定義すればいいだけ", \t -> do
	text t "* 見やすく表示するためにやや複雑になっている", \t -> do
	itext t 1 "- 一番外側の括弧は表示したくない", \t -> do
	itext t 1 "- 文字列は文字列らしく表示したい"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "今回の目的"
	text t "", \t -> do
	text t "* 自分で作った型を表示する", \t -> do
	itext t 1 "- 普通はderiving Showで足りる", \t -> do
	itext t 1 "- 次回に学ぶ存在型ではderivingが利かない", \t -> do
	itext t 1 "- 存在型をきれいに表示したい", \t -> do
	text t "* Haskellでのプログラムの手法を学ぶ"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "注意点"
	text t "", \t -> do
	text t "* 普通は型の定義のあとにderiving Showをつければ足りる", \t -> do
	text t "* 特にReadクラスのインスタンスにもする場合", \t -> do
	itext t 1 "- deriving (Show, Read)とするのが良い", \t -> do
	itext t 1 "- readとshowは互いに逆関数となるのが望ましい", \t -> do
	itext t 1 "- showを手書きしたら、それに合わせてreadも手書き", \t -> do
	itext t 1 "- 例外はあるがとくに理由がなければこれを守るべき", \t -> do
	text t "* 今回の内容はすべてderiving Showだけですむ", \t -> do
	itext t 1 "- deriving Showが何をしているのか見ていく", \t -> do
	text t "* deriving Showが使えなくても同じことができるように"
 ]

showClass :: Page
showClass = [\t -> do
	writeTopTitle t "Showクラスの定義"
	text t "", \t -> do
	text t "type ShowS = String -> String"
	text t "", \t -> do
	text t "class Show a where"
	itext t 1 "showsPrec :: Int -> a -> ShowS"
	itext t 1 "show :: a -> String"
	itext t 1 "showList :: [a] -> ShowS"
	itext t 1 "", \t -> do
	itext t 1 "showsPrec _ x s = show x ++ s"
	itext t 1 "show x = showsPrec 0 x \"\""
	itext t 1 "showList ls s = ..."
 ]

showClass2 :: Page
showClass2 = [\t -> do
	writeTopTitle t "クラス関数の説明"
	text t "", \t -> do
	text t "* showsPrecとshowのどちらかひとつを定義", \t -> do
	itext t 1 "- もう一方とshowListは自動的に定義される", \t -> do
	text t "* showは単純な定義", \t -> do
	itext t 1 "- 単にその型を文字列に変換すれば良い", \t -> do
	text t "* showsPrecは括弧の制御をしたい場合に使う", \t -> do
	itext t 1 "- Some 8はJustをつけるとJust (Some 8)となる", \t -> do
	text t "* showListにはその型のリストの表示方法を定義する", \t -> do
	itext t 1 "- 普通の型はデフォルトの定義で良い", \t -> do
	itext t 1 "- デフォルトでは[1, 2, 3 ...]のような表示となる", \t -> do
	itext t 1 "- 文字のリストの場合は\"hello\"のように表示したい"
 ]

animal :: Page
animal = [\t -> do
	writeTopTitle t "簡単な例"
	text t "", \t -> do
	text t "* 例えば犬と猫を含む動物型を考える", \t -> do
	itext t 1 "data Animal = Dog String | Cat String", \t -> do
	itext t 1 "instance Show Animal where"
	itext t 2 "show (Dog n) = \"Dog \" ++ show n"
	itext t 2 "show (Cat n) = \"Cat \" ++ show n", \t -> do
	arrowIText t 1 "簡単", \t -> do
	text t "しかし、", \t -> do
	itext t 1 "> print $ Just (Dog \"pochi\")"
	itext t 1 "Just Dog \"pochi\""
 ]

animal2 :: Page
animal2 = [\t -> do
	writeTopTitle t "括弧をつける"
	text t "", \t -> do
	text t "* 括弧を適切につけるには", \t -> do
	itext t 1 "- 型構築子の優先順位を考慮する必要がある", \t -> do
	itext t 1 "- 例えば以下のような例で", \t -> do
	itext t 2 "infixl 6 :+:"
	itext t 2 "infixl 7 :*:", \t -> do
	itext t 1 "- (3 :+: 4) :*: (2 :+: (5 :*: 3))の表示は", \t -> do
	itext t 2 "(3 :+: 4) :*: (2 :+: 5 :*: 3)", \t -> do
	text t "* つまり、より優先度の高い構築子が内側にある場合", \t -> do
	itext t 1 "- 括弧は省略できる"
 ]

animal3 :: Page
animal3 = [\t -> do
	writeTopTitle t "関数適用の優先度"
	text t "", \t -> do
	text t "* 関数適用の優先度は演算子の優先度より高い", \t -> do
	text t "* 演算子の優先度は0から9なので", \t -> do
	itext t 1 "- 関数適用の優先度は10とする", \t -> do
	text t "* 型構築子に関しても同じこと", \t -> do
	text t "* しばらくは普通の型構築子のみを考える", \t -> do
	text t "* つまり優先度10の場合のみを考えていく", \t -> do
	text t "* この場合以下のようになる", \t -> do
	itext t 1 "- 外側の優先度が10であるときは括弧の表示が必要", \t -> do
	itext t 1 "- 内側のshowに自分の優先度が10であることを伝える", \t -> do
	itext t 1 "- ただし一番外側にいる場合を0としたいので", \t -> do
	itext t 2 "渡していく数は(+ 1)された値となる"
 ]

animal4 :: Page
animal4 = [\t -> do
	writeTopTitle t "今いる場所の優先度"
	text t "", \t -> do
	text t "* showsPrecの型を見てみよう", \t -> do
	itext t 1 "Int -> a -> ShowS", \t -> do
	itext t 1 "- ここでのIntが今いる場所の結合強度となる", \t -> do
	text t "* よってその値を取ってそれが10より大ならば括弧が必要", \t -> do
	itext t 1 "showsPrec d x = if (d > 10)"
	itext t 2 "then 括弧必要"
	itext t 2 "else 括弧不要", \t -> do
	text t "* またshowsPrecのなかでさらにshowする場合", \t -> do
	itext t 1 "showsPrec 11 inner"
 ]

animal5 :: Page
animal5 = [\t -> do
	writeTopTitle t "ShowSについて"
	text t "", \t -> do
	text t "* ここでShowSについて見てみよう", \t -> do
	itext t 1 "type ShowS = String -> String", \t -> do
	text t "* リスト構造の特徴として(++)は右結合でつなげたい", \t -> do
	itext t 1 "- このへんの議論は差分リストとしてどこかでやる", \t -> do
	itext t 1 "- ((as ++ bs) ++ cs) ++ dsを以下のように変換する", \t -> do
	itext t 2 "as ++ (bs ++ (cs ++ ds))", \t -> do
	itext t 1 "- そのための仕組みがShowSである"
 ]

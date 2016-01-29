import Lecture

subtitle :: String
subtitle = "4. 代数的データ型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, bool, prod, union, prodUnion,
	param, pattern, label, newtype1, summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 新しい型を定義することができる", \t -> do
	text t "* 自分で定義する型を代数的データ型と呼ぶ", \t -> do
	text t "* 列挙による定義と既存の型からの定義とがある", \t -> do
	text t "* 多相的な型を定義することもできる"
	]

bool :: Page
bool = [ \t -> do
	writeTopTitle t "列挙の例"
	text t "", \t -> do
	text t "* Bool型は以下のように定義される", \t -> do
	itext t 1 "data Bool = False | True", \t -> do
	text t "* 型の名前を指定しその型の値を列挙すればよい", \t -> do
	text t "* 既存の型から新しい型をつくるには", \t -> do
	itext t 1 "+ 既存の型を組み合わせるか", \t -> do
	itext t 1 "+ 既存の型のどちらかとするか", \t -> do
	itext t 0.5 "の2通りの方法がある"
	]

prod :: Page
prod = [ \t -> do
	writeTopTitle t "組み合わせる"
	text t "", \t -> do
	text t "* たとえばBoolとCharを組み合わせた新しい型は", \t -> do
	itext t 1 "data BC = BC Bool Char", \t -> do
	text t "* はじめのBCは型の名前である", \t -> do
	text t "* 2つめのBCは値構築子である", \t -> do
	text t "* この型の値は以下のように書ける", \t -> do
	itext t 1 "BC False 'c'"
	]

union :: Page
union = [ \t -> do
	writeTopTitle t "どちらかとする"
	text t "", \t -> do
	text t "* BoolかCharのどちらかの値となる型は", \t -> do
	itext t 1 "data BOC = B Bool | C Char", \t -> do
	text t "* この型の値は以下のように書ける", \t -> do
	itext t 1 "B False", \t -> do
	itext t 1 "C 'c'"
	]

prodUnion :: Page
prodUnion = [ \t -> do
	writeTopTitle t "組み合わせのどちらかとする"
	text t "", \t -> do
	text t "* 組み合わせと「どちらかとする」は同時にできる", \t -> do
	itext t 1 "data Some = BC Bool Char | CI Char Int", \t -> do
	text t "* 「どちらかとする」の枠組みで既存の型をなくすと列挙だ", \t -> do
	text t "* 代数的データ型の定義の構文は以下のようになる", \t -> do
	itext t 1 "data [型名]"
	itext t 2 "= [値構築子1] [型1-1] [型1-2] ..."
	itext t 2 "| [値構築子2] [型2-1] [型2-2] ..."
	itext t 2 "...", \t -> do
	text t "* 値構築子のとる型の数が0のときに列挙型となる"
	]

param :: Page
param = [ \t -> do
	writeTopTitle t "多相型"
	text t "", \t -> do
	text t "* 代数的データ型の定義には型変数が使える", \t -> do
	text t "* Maybe型の定義は以下のようになっている", \t -> do
	itext t 1 "data Maybe a = Nothing | Just a", \t -> do
	text t "* aにCharを代入すると以下のようになるだろう", \t -> do
	itext t 1 "data Maybe Char = Nothing | Just Char", \t -> do
	text t "* つまりMaybe Char型の値は以下のようになる", \t -> do
	itext t 1 "Nothing", \t -> do
	itext t 1 "Just 'c'", \t -> do
	text t "* 型変数aに代入する型を変えるといろいろな型ができる"
	]

pattern :: Page
pattern = [ \t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* 値構築子を使ってパターンを作ることができる", \t -> do
	text t "* 関数定義の引数の部分にパターンをいれることで", \t -> do
	itext t 1 "+ どの値構築子で作られた値かによる分岐と", \t -> do
	itext t 1 "+ 値構築子でまとめられた値のとりだし", \t -> do
	itext t 0.5 "とができる", \t -> do
	text t "* Maybe値に対するパターンマッチの例だ", \t -> do
	itext t 1 "fun (Just n) = n", \t -> do
	itext t 1 "fun Nothing = 0", \t -> do
	text t "* fun Nothingの部分は値のとりだしの必要がないので", \t -> do
	itext t 1 "fun _ = 0", \t -> do
	itext t 0.5 "のようにワイルドカードを使ってもよい"
	]

label :: Page
label = [ \t -> do
	writeTopTitle t "フィールドラベル"
	text t "", \t -> do
	text t "* 単に値をとりだすだけの関数がしばしば必要になる", \t -> do
	itext t 1 "data BC = BC Bool Char", \t -> do
	itext t 1 "getBool :: BC -> Bool", \t -> do
	itext t 1 "getBool (BC b _) = b", \t -> do
	text t "* このような関数を自動に作ってくれる構文糖がある", \t -> do
	itext t 1 "data BC = BC { getBool :: Bool, getChar :: Char }", \t -> do
	text t "* Bool型のフィールドにgetBoolという名前を", \t -> do
	itext t 1 "Char型のフィールドにgetCharという名前をつけた"
	]

newtype1 :: Page
newtype1 = [ \t -> do
	writeTopTitle t "newtype"
	text t "", \t -> do
	text t "* 値構築子がひとつでそれがとる型もひとつのとき", \t -> do
	itext t 1 "data Some = Some Int", \t -> do
	text t "* このようなときIntとSomeは別の型として扱われる", \t -> do
	text t "* 別の型として扱われるが内部的には同じ表現でもいい", \t -> do
	text t "* Haskellではこのようなことを明示するために", \t -> do
	itext t 1 "newtype Some = Some Int", \t -> do
	itext t 0.5 "のように書ける", \t -> do
	text t "* dataではなくnewtypeを使うことでより効率的な動作となる"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 代数的データ型の作りかた、使いかたを見た"
	]

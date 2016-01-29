import Lecture

subtitle :: String
subtitle = "7. モノイド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, sumProd, list, bool,
	classMonoid, instanceList, instanceBool1, instanceBool2,
	summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 以下のような性質を満たす値をモノイドと呼ぶ", \t -> do
	itext t 1 "+ その型の値に対して結合則を満たす2項演算があり", \t -> do
	itext t 1 "+ その演算によって他の項を変化させない値がある", \t -> do
	text t "* モノイドを表す型クラスMonoidの定義を見ていこう"
	]

sumProd :: Page
sumProd = [ \t -> do
	writeTopTitle t "足し算とかけ算"
	text t "", \t -> do
	text t "* 足し算には以下の性質がある", \t -> do
	itext t 1 "0 + a == a", \t -> do
	itext t 1 "a + 0 == a", \t -> do
	itext t 1 "(a + b) + c = a + (b + c)", \t -> do
	text t "* かけ算にも同じように以下の性質がある", \t -> do
	itext t 1 "1 * a == a", \t -> do
	itext t 1 "a * 1 == a", \t -> do
	itext t 1 "(a * b) * c = a * (b * c)", \t -> do
	text t "* よって数は足し算に対してモノイドであり", \t -> do
	text t "* かけ算に対してもモノイドである"
	]

list :: Page
list = [ \t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* リストの結合演算子は(++)であり以下のような性質がある", \t -> do
	itext t 1 "[] ++ as == as", \t -> do
	itext t 1 "as ++ [] == as", \t -> do
	itext t 1 "(as ++ bs) ++ cs == as ++ (bs ++ cs)", \t -> do
	text t "* よってリストは結合演算に対してモノイドである"
	]

bool :: Page
bool = [ \t -> do
	writeTopTitle t "ブール値"
	text t "", \t -> do
	text t "* 論理和には以下の性質がある", \t -> do
	itext t 1 "False || b = b", \t -> do
	itext t 1 "b || False = b", \t -> do
	itext t 1 "(b1 || b2) || b3 = b1 || (b2 || b3)", \t -> do
	text t "* 論理積には以下の性質がある", \t -> do
	itext t 1 "True && b = b", \t -> do
	itext t 1 "b && True = b", \t -> do
	itext t 1 "(b1 && b2) && b3 = b1 && (b2 && b3)", \t -> do
	text t "* よってブール値は論理和に対してモノイドであり", \t -> do
	text t "* 論理積に対してもモノイドである"
	]

classMonoid :: Page
classMonoid = [ \t -> do
	writeTopTitle t "クラスMonoid"
	text t "", \t -> do
	text t "* クラスMonoidの定義は以下のようになる", \t -> do
	itext t 1 "class Monoid a where", \t -> do
	itext t 2 "mempty :: a", \t -> do
	itext t 2 "mappend :: a -> a -> a", \t -> do
	text t "* mappendが対象となる演算であり", \t -> do
	itext t 1 "memptyはその演算で相手を変化させない値である"
	]

instanceList :: Page
instanceList = [ \t -> do
	writeTopTitle t "リストをMonoidにする"
	text t "", \t -> do
	text t "* リストをMonoidにしてみよう", \t -> do
	itext t 1 "instance Mnoid [a] where", \t -> do
	itext t 2 "mempty = []", \t -> do
	itext t 2 "mappend = (++)", \t -> do
	text t "* 対象となる演算は結合演算であり", \t -> do
	itext t 1 "相手を変化させない値は空リストだ"
	]

instanceBool1 :: Page
instanceBool1 = [ \t -> do
	writeTopTitle t "ブールをMonoidにする"
	text t "", \t -> do
	text t "* Bool値は論理和と論理積のどちらに対してもモノイドだ", \t -> do
	text t "* どちらの演算を選ぶかは恣意的なものになってしまう", \t -> do
	text t "* このようなときにはnewtypeによって新しい型にする", \t -> do
	itext t 1 "newtype Any = Any { getAny :: Bool }", \t -> do
	itext t 1 "newtype All = All { getAll :: Bool }", \t -> do
	text t "* 論理和に対してMonoidにする", \t -> do
	itext t 1 "instance Monoid Any where", \t -> do
	itext t 2 "mempty = Any False", \t -> do
	itext t 2 "mappend (Any b1) (Any b2) = Any $ b1 || b2"
	]

instanceBool2 :: Page
instanceBool2 = [ \t -> do
	writeTopTitle t "ブールをMonoidにする"
	text t "", \t -> do
	text t "* 論理積に対してMonoidにする", \t -> do
	itext t 1 "instance Monoid All where", \t -> do
	itext t 2 "mempty = All True", \t -> do
	itext t 2 "mappend (All b1) (All b2) = All $ b1 && b2"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* モノイドを表現する型クラスMonoidを見た", \t -> do
	text t "* モノイドは値をためこんでいくような処理に使える"
	]

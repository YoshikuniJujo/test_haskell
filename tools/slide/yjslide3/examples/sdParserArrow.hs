import Prelude hiding (id, (.))

import Control.Category

data StaticParser s = SP Bool [s]
newtype DynamicParser s a b = DP ((a, [s]) -> (b, [s]))
data Parser s a b = P (StaticParser s) (DynamicParser s a b)

instance (Bounded s, Enum s, Eq s) => Category (Parser s) where
	id = P (SP True [minBound .. maxBound]) (DP id)
	P (SP e1 a1) (DP f1) . P (SP e2 a2) (DP f2) = P
		(SP (e1 && e2) (a2 ++ if e2 then a1 else []))
		(DP $ f1 . f2)

-- (>=>) :: (a -> Parser s () b) -> (b -> Parser s () c) -> (a -> Parser s () c)
--	Parser s () b = P (StaticParser s) (DynamicParser s () b)
-- (>>>) :: Parser s a b -> Parser s b c -> Parser s a c
--
-- a -> Parser s () b = a -> P (StaticParser s) (DynamicParser s () b)
-- Parser s a b = P (StaticPaser s) (DynamicParser s a b)
--
-- Parser s a bとaがあったとしてParser s () bは求められるか?
-- つまりParser s a bをa -> Parser s () bに変換できるか?
-- (Bool, [s], (a, [s]) -> (b, [s]))とaがあるとき、
-- (Bool, [s], ((), [s]) -> (b, [s]))が作れるかということ。
--
-- (a, [s]) -> (b, [s])だけしかなければaがあれば
-- ((), [s]) -> (b, [s])は作れる。
--
-- Boolと[s]を構築するルールが必要になる。
--
-- a b c -> a c d -> a b d
--
-- (bからcへの写像 + 何か)と(cからdへの写像 + 何か) -> (bからdへの写像 + 何か)
-- この場合、(bからcへの写像 + 何か)とbがあったとしても、
-- 結果の「何か」が作れないので、
-- app :: a (a b c, b) c は作れないことになる。
--
-- (bから(c + 何か)への写像)と(cから(d + 何か)への写像) -> (bから(d + 何か)への写像)

arr :: (b -> c) -> IOMcn b c
app :: IOMcn (IOMcn b c, b) c
	IOMcn b c -> b -> IOMcn () c

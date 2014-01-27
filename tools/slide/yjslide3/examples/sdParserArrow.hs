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
-- Parser s a b��a�����ä��Ȥ���Parser s () b�ϵ����뤫?
-- �Ĥޤ�Parser s a b��a -> Parser s () b���Ѵ��Ǥ��뤫?
-- (Bool, [s], (a, [s]) -> (b, [s]))��a������Ȥ���
-- (Bool, [s], ((), [s]) -> (b, [s]))�����뤫�Ȥ������ȡ�
--
-- (a, [s]) -> (b, [s])���������ʤ����a�������
-- ((), [s]) -> (b, [s])�Ϻ��롣
--
-- Bool��[s]���ۤ���롼�뤬ɬ�פˤʤ롣
--
-- a b c -> a c d -> a b d
--
-- (b����c�ؤμ��� + ����)��(c����d�ؤμ��� + ����) -> (b����d�ؤμ��� + ����)
-- ���ξ�硢(b����c�ؤμ��� + ����)��b�����ä��Ȥ��Ƥ⡢
-- ��̤Ρֲ����פ����ʤ��Τǡ�
-- app :: a (a b c, b) c �Ϻ��ʤ����Ȥˤʤ롣
--
-- (b����(c + ����)�ؤμ���)��(c����(d + ����)�ؤμ���) -> (b����(d + ����)�ؤμ���)

arr :: (b -> c) -> IOMcn b c
app :: IOMcn (IOMcn b c, b) c
	IOMcn b c -> b -> IOMcn () c

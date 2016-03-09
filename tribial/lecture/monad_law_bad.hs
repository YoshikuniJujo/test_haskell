{-

(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(f >=> g) >=> h /= f >=> (g >=> h)

単位元はあるけど結合則を満たさないような演算を考える。

-}

{-

(f >=> g) x = let
	NM m y = f x
	NM n z = g y in
	NM (m - n) z

(>>=) :: NotMonad a -> (a -> NotMonad b) -> NotMonad b
m >>= f = (const m >=> f) ()

-}

(>==>) :: (a -> NotMonad b) -> (b -> NotMonad c) -> a -> NotMonad c
(f >==> g) x = let
	NM m y = f x
	NM n z = g y in
	NM (m `sub` n) z

data Mistery = Unit | N Int deriving Show

sub :: Mistery -> Mistery -> Mistery
sub Unit y = y
sub x Unit = x
sub (N x) (N y) = N $ x - y

data NotMonad a = NM Mistery a deriving Show

instance Monad NotMonad where
	return = NM Unit
	NM m x >>= f = let NM n y = f x in NM (m `sub` n) y

{-

return x >>= f
=> NM Unit x >>= f
=> let NM n y = f x in NM (Unit `sub` n) y
=> let NM n y = f x in NM n y
=> f x

m@(NM n x) >>= return
=> NM n x >>= NM Unit
=> let NM p y = NM Unit x in NM (n `sub` p) y
=> NM (n `sub` Unit) x
=> NM n x

-}

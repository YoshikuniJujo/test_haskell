import Control.Monad (foldM)

type Yoshio = (Integer, Integer)
data Event = E Enemy | I Item deriving Show
data Enemy = Ghost | Troll | Vampire deriving Show
data Item = Shell | Coin | Jewel | Star deriving Show

damage :: Enemy -> Integer
damage Ghost = 10
damage Troll = 50
damage Vampire = 100

score :: Item -> Integer
score Shell = 50
score Coin = 100
score Jewel = 500
score Star = 1000

event :: Yoshio -> Event -> Maybe Yoshio
event (hp, s) (E e) = do
	let hp' = hp - damage e
	mguard $ hp' > 0
	return (hp', s)
event (hp, s) (I i) = return (hp, s + score i)

game :: Yoshio -> [Event] -> Maybe Yoshio
game = foldM event

stage1, stage2 :: [Event]
stage1 = [E Ghost, I Shell, I Star, I Coin, E Troll, I Coin, E Vampire]
stage2 = [E Vampire, E Vampire, I Star, I Star, I Star, I Coin, I Star]

mguard :: Bool -> Maybe ()
mguard True = Just ()
mguard _ = Nothing

memp :: Maybe a
memp = Nothing

malt :: Maybe a -> Maybe a -> Maybe a
mx@(Just x) `malt` _ = mx
_ `malt` my = my

malts :: [Maybe a] -> Maybe a
malts (x : xs) = x `malt` malts xs
malts _ = memp

lfoldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
lfoldM f v (x : xs) = f v x >>= \v' -> lfoldM f v' xs
lfoldM _ v _ = return v

{-
myFoldM :: (Foldable t, Monad m) => (a -> b -> m a) -> a -> t b -> m a
myFoldM f = flip $ foldr f' return
	where
	f' x k = (>>= k) . flip f x
	-}

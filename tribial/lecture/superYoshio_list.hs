import Control.Monad (foldM)

data Event = E Enemy | I Item deriving Show
data Enemy = Ghost | Troll | Vampire deriving Show
data Item = Shell | Coin | Jewel | Star deriving Show
type Yoshio = (Integer, Integer)

damage :: Enemy -> Integer
damage Ghost = 10
damage Troll = 50
damage Vampire = 100

score :: Item -> Integer
score Shell = 50
score Coin = 100
score Jewel = 500
score Star = 1000

event :: Yoshio -> Event -> [Yoshio]
event (hp, s) (E e) = do
	let hp' = hp - damage e
	lguard $ hp' > 0
	return (hp', s)
event (hp, s) (I i) = return (hp, s + score i)

game :: Yoshio -> [Event] -> [Yoshio]
game = foldM event

lguard :: Bool -> [()]
lguard True = [()]
lguard _ = []

lalt :: [a] -> [a] -> [a]
lalt = (++)

lalts :: [[a]] -> [a]
lalts (x : xs) = x `lalt` lalts xs
lalts _ = []

stage1, stage2 :: [Event]
stage1 = [E Ghost, I Shell, I Star, I Coin, E Troll, I Coin, E Vampire]
stage2 = [E Vampire, E Vampire, I Star, I Star, I Star, I Coin, I Star]

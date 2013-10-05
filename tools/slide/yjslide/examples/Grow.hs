module Grow where

class Growable a where
	grow :: a -> a
	isGoal :: a -> Bool

data Human = Baby | Child | Adult | Old deriving Show
data Buri = Tsubasu | Hamachi | Mejiro | Buri deriving Show
data Bora = Oboko | Subashiri | Ina | Bora | Todo deriving Show

instance Growable Human where
	grow Baby = Child
	grow Child = Adult
	grow Adult = Old
	grow Old = Old
	isGoal Old = True
	isGoal _ = False

instance Growable Buri where
	grow Tsubasu = Hamachi
	grow Hamachi = Mejiro
	grow Mejiro = Buri
	grow Buri = Buri
	isGoal Buri = True
	isGoal _ = False

instance Growable Bora where
	grow Oboko = Subashiri
	grow Subashiri = Ina
	grow Ina = Bora
	grow Bora = Todo
	grow Todo = Todo
	isGoal Todo = True
	isGoal _ = False

instance Growable Int where
	grow n = n + 1
	isGoal = (== maxBound)

printFuture :: (Growable a, Show a) => a -> IO ()
printFuture x = do
	print x
	if isGoal x
		then return ()
		else printFuture $ grow x

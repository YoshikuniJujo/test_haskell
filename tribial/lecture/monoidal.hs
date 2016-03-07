import Control.Applicative

class Functor f => Monoidal f where
	unit :: f ()
	(.**) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
	unit = Just ()
	Just x .** Just y = Just (x, y)
	_ .** _ = Nothing

pure' :: Monoidal f => a -> f a
pure' = ($ unit) . fmap . const

app :: Monoidal f => f (a -> b) -> f a -> f b
app = curry $ fmap (uncurry ($)) . uncurry (.**)

unit' :: Applicative f => f ()
unit' = pure ()

(.**.) :: Applicative f => f a -> f b -> f (a, b)
x .**. y = pure (,) <*> x <*> y

{-

Functor law

fmap id == id
fmap (f . g) == fmap f . fmap g

-}

{-

Monoidal law

fmap snd $ unit .** v == v
fmap fst $ u .** unit == u
fmap asl $ u .** (v .** w) = (u .** v) .** w
	where asl (x, (y, z)) = ((x, y), z)

-}

{-

fmap (g *** h) (u .** v) = fmap g u .** fmap h v

-}

{-

fmap (first g) (u .** v) = fmap g u .** v

-}

{-

Applicative law

(pure id <*>) == id
pure f <*> pure x == pure (f x)
u <*> pure y = pure ($ y) <*> u
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-}

{-

pure id <*> v == v

pure id <*> v
=> fmap (uncurry ($)) $ pure id .** v
=> fmap (uncurry ($)) $ fmap (const id) unit .** v

-}

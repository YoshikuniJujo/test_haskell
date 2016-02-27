import Data.Monoid

data Fun m a b = Fun m (a -> b)

apply :: Fun m a b -> a -> (m, b)
apply (Fun m f) x = (m, f x)

dot :: Monoid m => Fun m b c -> Fun m a b -> Fun m a c
Fun m f `dot` Fun n g = Fun (m `mappend` n) (f . g)

fun :: Monoid m => (a -> b) -> Fun m a b
fun f = Fun mempty f

put :: (a -> b) -> m -> Fun m a b
put f m = Fun m f

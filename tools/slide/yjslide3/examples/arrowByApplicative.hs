import Prelude hiding (id)

import Control.Category
import Control.Aplicative

(&&&) :: Applicative m => m a -> m b -> m (a, b)
(&&&) = liftA2 (,)

class Category arr => Arrow arr where
	arr :: Functor (arr a) => (a -> b) -> arr a b
	arr f = fmap f id

	first :: Applicative

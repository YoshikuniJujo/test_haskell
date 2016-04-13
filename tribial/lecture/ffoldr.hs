import Prelude hiding (foldr)
import qualified Prelude

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = Prelude.foldr

ffoldr :: (a -> b -> b) -> [a] -> b -> b
ffoldr = flip . foldr

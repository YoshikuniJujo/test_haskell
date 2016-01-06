import Control.Applicative
import Data.Bits
import Data.Bool

type B = [Bool]

decimal :: B -> Integer
decimal (a : as) = bool 0 1 a .|. (decimal as `shiftL` 1)
decimal _ = 0

bools :: Integer -> B
bools n | n < 1 = []
bools n = n `testBit` 0 : bools (n `shiftR` 1)

scc :: B -> B
scc (False : as) = True : as
scc (_ : as) = False : scc as
scc _ = [True]

prd :: B -> Maybe B
prd [] = Nothing
prd (True : as) = Just $ False : as
prd (_ : as) = (True :) <$> prd as

add :: B -> B -> B
(False : as) `add` (b : bs) = b : as `add` bs
(a : as) `add` (False : bs) = a : as `add` bs
(_ : as) `add` (_ : bs) = False : as `add` scc bs
[] `add` bs = bs
as `add` _ = as

sub :: B -> B -> Maybe B
(a : as) `sub` (False : bs) = (a :) <$> as `sub` bs
(True : as) `sub` (True : bs) = (False :) <$> as `sub` bs
(_ : as) `sub` (_ : bs) = (True :) <$> ((`sub` bs) =<< prd as)
as `sub` [] = Just as
_ `sub` bs
	| or bs = Nothing
	| otherwise = Just []

-- module Zot (main, mainFile) where
module Zot where

import Control.Monad
import Data.Semigroup
import Data.Show
import Data.Function
import Data.Maybe
import Effect
import Data.Unit

data Fun = Fun (Fun -> Effect Fun) | Zero | One

mkFun :: (Fun -> Fun) -> Fun
mkFun f = Fun $ pure <<< f

instance Show Fun where
        show Zero = "0"
        show One = "1"
        show (Fun _) = "function"

codePointToFun :: Int -> Maybe Fun
codePointToFun 48 = Just Zero
codePointToFun 49 = Just One
codePointToFun _ = Nothing

dd :: Fun -> Fun -> Effect Fun
dd (Fun f) = f
dd Zero = dd $ zero unit
dd One = dd one

infixl 9 dd as $$

gtdd :: Effect Fun -> Fun -> Effect Fun
gtdd f a = f >>= (_ $$ a)

infixl 6 gtdd as >$$

ddlt :: Fun -> Effect Fun -> Effect Fun
ddlt f a = (f $$ _) =<< a

infixr 8 ddlt as $$<

gtddlt :: Effect Fun -> Effect Fun -> Effect Fun
gtddlt f a = f >>= (_ $$< a)

infixl 7 gtddlt as >$$<

cont :: Fun -> Fun
cont f = Fun (_ $$ f)

s :: Fun
s = mkFun $ \x -> mkFun $ \y -> Fun $ \z -> x $$ z >$$< y $$ z

k :: Fun
k = mkFun $ mkFun <<< const

zero :: Unit -> Fun
zero _ = cont (Fun (\f -> f $$ s >$$ k))

one :: Fun
one = mkFun $ \c -> cont $ mkFun $ \l -> cont $ Fun $ \r -> c $$< l $$ r

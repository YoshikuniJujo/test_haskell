module SKI where

import Data.Semigroup
import Data.Function
import Data.Eq
import Data.Enum
import Data.Show
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Boolean
import Effect.Exception
import Effect.Unsafe

data Rec :: forall k . k -> Type
data Rec a
        = In { out :: Rec a -> Rec a }
        | Int { fromInt :: Int }
        | Char Char | Bool Boolean
        | Error String

instance Eq (Rec a) where
        eq (Int i) (Int j) = i == j
        eq (Char c) (Char d) = c == d
        eq (Bool b) (Bool c) = b == c
        eq (In _) (In _) = unsafePerformEffect $ throwException $ error "can not compare"
        eq _ _ = false

instance Show ( Rec a ) where
        show ( Int i ) = show i
        show ( Error e ) = "Error: " <> e
        show _ = "function"

suc :: forall a . Partial => Rec a
suc = In { out : scc }
        where
        scc ( Int { fromInt : i } ) = Int { fromInt : fromJust $ succ i }
        scc ( Char c ) = Char $ fromJust $ succ c
        scc _ = unsafePerformEffect $ throwException $ error "not int"

out_ :: forall a . Rec a -> Rec a -> Rec a
out_ x = case x of
        In { out : f } -> f
        _        -> \_ -> Error "not function"

oi :: forall a . Rec a -> Rec a
oi x = x
ii :: forall a . Rec a
ii = In { out : oi }

ok :: forall a . Rec a -> Rec a
ok x = In { out : \_ -> x }
k :: forall a . Rec a
k = In { out : ok }

os :: forall a . Rec a -> Rec a
-- s x y z = x z ( y z )
os x = In { out : \y -> In {  out : \z -> ( out_ $ out_ x z ) ( out_ y z ) } }
s :: forall a . Rec a
s = In { out : os }

readChurch :: forall a . Partial => Rec a -> Rec a
readChurch cn = cn `out_` suc `out_` Int { fromInt : 0 }

mkski :: forall a . String -> Rec a
mkski = fst <<< makeSKI

makeSKI :: forall a . String -> Tuple (Rec a) String
makeSKI ( '`' : rest )        = let
        (Tuple c rest')        = makeSKI rest
        (Tuple c' rest'')        = makeSKI rest' in case c of
                In f        -> Tuple (f c') rest''
                _        -> Tuple (Error "in makeSKI") rest''
--        (Tuple (out c c') rest'')
makeSKI ( 's' : rest ) = (Tuple s rest)
makeSKI ( 'k' : rest ) = (Tuple k rest)
makeSKI ( 'i' : rest ) = (Tuple ii rest)
makeSKI _ = error "makeSKI error"

sc :: Rec a
sc = mkski "`s``s`ks``s`kki"

mul :: Rec a -> Rec a -> Rec a
mul x y = (x.out (y.out sc)).out (k.out ii )

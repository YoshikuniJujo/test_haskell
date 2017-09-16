{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Elem t ts where
	Elem _ () = 'False
	Elem t (t, _) = 'True
	Elem t (_, ts) = Elem t ts

infixr 5 .:

(.:) :: Elem t ts ~ 'False => t -> ts -> (t, ts)
(.:) = (,)

class Get v vs where get :: vs -> v

instance Get v (v, vs) where
	get (x, _) = x
instance {-# OVERLAPPABLE #-} Get v vs => Get v (_w, vs) where
	get (_, xs) = get xs

sample :: (Int, (Double, (Bool, (Char, ()))))
sample = 3 .: 8.5 .: False .: 'c' .: ()

{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

class ShowType a where
	getType :: String

instance ShowType Int where
	getType = "Int"

instance ShowType Double where
	getType = "Double"

instance ShowType Char where
	getType = "Char"

instance ShowType a => ShowType [a] where
	getType = "[" ++ getType @a ++ "]"

typeOf :: forall t . ShowType t => t -> String
typeOf _ = getType @t

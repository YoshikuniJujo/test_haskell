module Object (
	Object(..),
	showObj
) where

data Object
	= OInt Integer
	| OVar String
	| OCons Object Object
	| ONil
	| OSubr String (Object -> Either String Object)

showObj :: Object -> String
showObj (OInt i) = show i
showObj (OVar v) = v
showObj c@(OCons _ _) = showCons False c
showObj ONil = "()"
showObj (OSubr n _) = "#<subr " ++ n ++ ">"

showCons :: Bool -> Object -> String
showCons l (OCons a d) = (if l then id else ("(" ++ ) . (++ ")")) $
	case d of
		OCons _ _ -> showObj a ++ " " ++ showCons True d
		ONil -> showObj a
		_ -> showObj a ++ " . " ++ showObj d
showCons l ONil = if l then "" else "()"
showCons _ _ = error "not cons"

{-# LANGUAGE ExistentialQuantification #-}

data Object = forall x . Object x

-- getObject :: Object -> x
-- getObject (Object x) = x

heteroList1 :: [Object]
heteroList1 = [Object (), Object True, Object 3, Object "hello"]

data ShowBox = forall s . Show s => ShowBox s

instance Show ShowBox where
	showsPrec d (ShowBox s) =
		showParen (d > 10) $ showString "ShowBox " . showsPrec 11 s
--	show = ("ShowBox (" ++) . (++ ")") . showSB

showSB :: ShowBox -> String
showSB (ShowBox s) = show s

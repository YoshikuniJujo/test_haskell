{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data ShowYou = forall s . Show s => ShowYou s

instance Show ShowYou where
	show (ShowYou s) = "(ShowYou " ++ show s ++ ")"

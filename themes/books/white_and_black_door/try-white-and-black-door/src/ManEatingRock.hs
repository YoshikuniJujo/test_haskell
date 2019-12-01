{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ManEatingRock where

import GHC.Stack (HasCallStack)
import GHC.Exts.Heap

-- 1st Ancient Lulu language

-- * * o
-- o * o o *
-- * o o *
-- o * * o
-- o o * *

data Ch = W | B deriving Show

data Ruin = Limbo | Room Bool Ruin Ruin

instance Show Ruin where
	show Limbo = "Limbo"
	show (Room e w b) = "(Room " ++ show e ++ " " ++ show w ++ " " ++ show b ++ ")"

showRuin :: Ruin -> IO String
showRuin r = do
	show <$> getClosureData r

tryRuin :: Ruin -> [Ch] -> Bool
tryRuin Limbo _ = False
tryRuin (Room True _ _) [] = True
tryRuin _ [] = False
tryRuin (Room _ w b) (c : cs) = tryRuin (case c of W -> w; B -> b) cs

room :: Ruin -> Ruin -> Ruin
room = Room False

exit :: Ruin -> Ruin -> Ruin
exit = Room True

manEatingRock :: Ruin
manEatingRock = room (room (room Limbo lastRoom) openEye) openEye
	where
	openEye = room (room lastRoom Limbo) (room exitRoom Limbo)
	lastRoom = room Limbo exitRoom
	exitRoom = exit Limbo Limbo

mkRoute :: HasCallStack => String -> [Ch]
mkRoute "" = []
mkRoute ('o' : cs) = W : mkRoute cs
mkRoute ('*' : cs) = B : mkRoute cs
mkRoute _ = error "bad input"

class Printable a where print' :: a -> IO ()

instance {-# OVERLAPPABLE #-} Show a => Printable a where
	print' = putStrLn . show

isEvaluated :: a -> IO Bool
isEvaluated x = (<$> getClosureData x) $ \case
	ConstrClosure {} -> True
	BlackholeClosure {} -> True
	_ -> False

Room _ _ some = manEatingRock

x = 1 : x

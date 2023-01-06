{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LinearType.Order (Begin, End, A, B, C, begin, funF, funG, funH) where

data Begin = Begin deriving Show
data End = End deriving Show

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show

begin :: (Begin %1 -> IO End) -> IO ()
begin f = do
	End <- f Begin
	pure ()

funF :: Begin %1 -> IO A
funF Begin = A <$ putStrLn "f"

funG :: A %1 -> IO B
funG A = B <$ putStrLn "g"

funH :: B %1 -> IO End
funH B = End <$ putStrLn "h"

{-# LANGUAGE GADTs #-}

data Cell where
	Cell :: Char -> Int -> Cell

module TDList (
	(!!!),
	(!!!=),
	show2D
) where

(!!!) :: [[a]] -> (Int, Int) -> a
lst !!! (i, j) = lst !! i !! j

(!!!=) :: [[a]] -> (Int, Int, a) -> [[a]]
lst !!!= (i, j, v) =
	take i lst ++ [take j (lst !! i) ++ [v] ++ drop (j + 1) (lst !! i)] ++
	drop (i + 1) lst

show2D :: Show a => Int -> [[a]] -> String
show2D l = unlines . map (unwords . map (show_l l))

show_l :: Show a => Int -> a -> String
show_l l x = replicate (l - length (show x)) ' ' ++ show x

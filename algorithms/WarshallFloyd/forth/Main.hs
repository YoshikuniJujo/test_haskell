import Control.Monad

test :: [[Int]]
test = [
	[  0, 18,999, 14,999],
	[ 18,  0, 17,999, 16],
	[999, 17,  0, 13,999],
	[ 14,999, 13,  0, 12],
	[999, 16,999, 12,  0]]

wf :: Int -> [[Int]] -> [[Int]]
wf s ps0 = for [0 .. s] ps0 $ \v ps ->
	collect [0 .. s] $ \f ->
		collect [0 .. s] $ \t ->
			min (ps !! f !! t) (ps !! f !! v + ps !! v !! t)

for :: [a] -> b -> (a -> b -> b) -> b
for is s body = foldr body s is

collect :: [a] -> (a -> b) -> [b]
collect = flip map

--------------------------------------------------------------------------------

wfdbg :: Int -> [[Int]] -> IO [[Int]]
wfdbg s ps0 = do
	ret <- forM' [0 .. s] ps0 $ \v ps -> do
		if v < 1 then putStrLn "初期値" else putStrLn $ "Via = " ++ show v
		putStrLn $ unlines $ map (unwords . map (keta 3 . show)) ps
		return $ collect [0 .. s] $ \f ->
			collect [0 .. s] $ \t ->
				min (ps !! f !! t) (ps !! f !! v + ps !! v !! t)
	putStrLn $ "Via = " ++ show (s + 1)
	putStrLn $ unlines $ map (unwords . map (keta 3 . show)) ret
	return ret

forM' :: Monad m => [a] -> b -> (a -> b -> m b) -> m b
forM' is s body = foldM (flip body) s is

next v s ps =
	collect [0 .. s] $ \f ->
		collect [0 .. s] $ \t ->
			min (ps !! f !! t) (ps !! f !! v + ps !! v !! t)

keta n s = replicate (n - length s) ' ' ++ s

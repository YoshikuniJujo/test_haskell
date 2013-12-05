
printNtoM :: Int -> Int -> IO ()
printNtoM n m
	| n <= m = print n >> printNtoM (n + 1) m
	| otherwise = return ()

printNtoM' :: Int -> Int -> IO ()
printNtoM' n m = mapM_ print [n .. m]

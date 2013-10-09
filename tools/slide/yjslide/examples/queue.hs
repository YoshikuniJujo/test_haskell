class Queue q where
	empty :: q a
	isEmpty :: q a -> Bool
	enqueue :: a -> q a -> q a
	dequeue :: q a -> (a, q a)

instance Queue [] where
	empty = []
	isEmpty = null
	enqueue = (:)
	dequeue xs = (last xs, init xs)

-- sendMessage :: Queue q => String -> q String -> q String
-- sendMessage = enqueue

-- readMessage :: Queue q => q String -> (String, q String)
-- readMessage = dequeue

getMessages' :: Queue q => IO (q String)
getMessages' = do
	m <- getLine
	if m == "."
		then return empty
		else do	ms <- getMessages'
			return $ enqueue m ms

getMessages :: Queue q => q String -> IO (q String)
getMessages q = do
	m <- getLine
	if m == "."
		then return q
		else getMessages $ enqueue m q

readMessages :: Queue q => q String -> IO ()
readMessages q = do
	if isEmpty q
		then return ()
		else do	let (m, q') = dequeue q
			putStrLn m
			readMessages q'

main :: IO ()
main = getMessages (empty :: [String]) >>= readMessages

data TwoList = TwoList [a] [a]

instance Queue q where
	empty = TwoList [] []
	isEmpty (TwoList [] []) = True
	isEmpty _ = False
	enqueue x (TwoList es ds) = TwoList (x : es) ds
	dequeue (TwoList es []) = dequeue (TwoList [] $ reverse es)
	dequeue (TwoList es (d : ds)) = (d, TwoList es ds)

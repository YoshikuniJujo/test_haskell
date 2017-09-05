{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Random

type Cont r a = (a -> r) -> r

ret :: a -> Cont r a
ret x = ($ x)

bind :: Cont r a -> (a -> Cont r b) -> Cont r b
(c `bind` f) k = c $ (`f` k)

data VE s w = V w | S (State s (VE s w))

data State s w = State (s -> s) (s -> w)
				-- State (s -> s) (s -> VE s w)

modifyGet :: (s -> s) -> Cont (VE s w) s
				--  (s -> s) -> (s -> VE s w) -> VE s w
modifyGet f = S . State f

modify :: (s -> s) -> Cont (VE s w) ()
modify f = modifyGet f `bind` \_ -> ret ()

get :: Cont (VE s w) s
get = modifyGet id

put :: s -> Cont (VE s w) ()
put = modify . const

runState :: Cont (VE s a) a -> s -> a
runState m = sloop (m V)

sloop :: VE s a -> s -> a
m `sloop` s = case m of
	V x -> x
	S (State f k) -> k s `sloop` f s

getAny :: (Random a) => Cont (VE StdGen w) a
getAny =
	get `bind` \g ->
	ret (random g) `bind` \(x, g') ->
	put g' `bind` \_ ->
	ret x

mkRandomValue :: StdGen -> (Int, Bool)
mkRandomValue = runState $
	getAny `bind` \n ->
	getAny `bind` \b ->
	ret (n, b)

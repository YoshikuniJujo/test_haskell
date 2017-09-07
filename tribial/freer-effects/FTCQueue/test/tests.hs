{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import MyEff
import MyEff.Coroutine

sampleCoroutine :: Eff '[Yield String Int, IO] Integer
sampleCoroutine = do
	s <- yield "hello" $ concat . (`replicate` "world")
	send $ putStrLn s
	i <- yield "hoge" (id :: Int -> Int)
	send $ print i
	return 123

runAll :: (Member IO effs, Show a) =>
	Eff (Yield a b ': effs) c -> [b] -> Eff effs c
runAll c is = do
	s <- runC c
	runS s is

runS :: (Member IO effs, Show a) =>
	Status effs a b c -> [b] -> Eff effs c
runS (Done r) _ = return r
runS (Continue x s) (i : is) = do
	send . putStrLn $ "OUT: " ++ show x
	(`runS` is) =<< s i
runS _ _ = error "tarinai"

module Lib where

import GHC.Stack (HasCallStack)
import System.IO.Unsafe

myHead :: HasCallStack => [a] -> a
myHead [] = error "myHead: bad"
myHead (x : _) = unsafePerformIO $ putStrLn "myHead called" >> pure x

timesDo :: Monad m => Int -> m () -> m ()
n `timesDo` _ | n < 1 = return ()
n `timesDo` act = act >> (n - 1) `timesDo` act

timesEvaluate :: Monad m => Int -> (a -> m ()) -> a -> m ()
timesEvaluate n _ _ | n < 1 = return ()
timesEvaluate n act x = act x >> timesEvaluate (n - 1) act x

evaluateOneTime :: IO ()
evaluateOneTime = 15 `timesDo` print (myHead "Hello")

evaluateManyTimes :: IO ()
evaluateManyTimes = timesEvaluate 15 (print . myHead) "Hello"

times :: Int -> a -> [a]
times n _ | n < 1 = []
times n x = x : times (n - 1) x

timesEvaluate' :: Int -> (a -> b) -> a -> [b]
timesEvaluate' n _ _ | n < 1 = []
timesEvaluate' n f x = f x : timesEvaluate' (n - 1) f x

evaluateOneTime' :: [Char]
evaluateOneTime' = 15 `times` myHead "Hello"

evaluateManyTimes' :: [Char]
evaluateManyTimes' = timesEvaluate' 15 myHead "Hello"

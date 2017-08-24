{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Test.HUnit
import System.IO

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)

tests :: Test
tests = TestList [
	"fact 1" ~: fact 1 ~?= 1,
	"fact 2" ~: fact 2 ~?= 2,
	"fact 3" ~: fact 3 ~?= 6,
	"fact 100" ~: fact 100 ~?= 100,
	TestCase $ putStrLn "hogera" >> error "hoge" ]

main :: IO ()
main = do
	_ <- runTestText (putTextToHandle stderr False) tests
	return ()

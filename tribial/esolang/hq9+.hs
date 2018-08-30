{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.IORef
import System.Environment

type Syntax = [Token]
data Token = H | Q | Nine | Plus deriving Show

syntax :: String -> Maybe Syntax
syntax ('H' : cs) = (H :) <$> syntax cs
syntax ('Q' : cs) = (Q :) <$> syntax cs
syntax ('9' : cs) = (Nine :) <$> syntax cs
syntax ('+' : cs) = (Plus :) <$> syntax cs
syntax ('\n' : cs) = syntax cs
syntax "" = Just []
syntax _ = Nothing

run :: String -> IORef Int -> Syntax -> IO ()
run src acc (H : ts) = putStr "Hello, world!" >> run src acc ts
run src acc (Q : ts) = putStr src >> run src acc ts
-- run _ _ (Nine : ts) = put
run src acc (Plus : ts) = modifyIORef acc succ >> run src acc ts
run _ _ [] = return ()

main :: IO ()
main = do
	fp : _ <- getArgs
	acc <- newIORef 0
	src <- readFile fp
	maybe (putStrLn "parse error") (run src acc) $ syntax src

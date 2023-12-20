{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (repeat, break, until, scanl)
import Control.Monad
import Control.Moffy
import Control.Moffy.Run
import Control.Exception (bracket)
import Data.Type.Set
import Data.Type.Flip
import Data.OneOrMoreApp
import Data.Bool
import Data.Char
import System.IO qualified as IO

data Key = KeyReq deriving (Show, Eq, Ord)
numbered [t| Key |]
instance Request Key where data Occurred Key = OccKey Char deriving Show

main :: IO ()
main = bar >> putStrLn ""

foo :: IO (Either String Int)
foo = withNoBuffering IO.stdin . interpretReact handle $
	pressOn 'a' >> digits

bar :: IO ()
bar = withNoBuffering IO.stdin $ interpret handle output do
	void $ (add <$%> sigX 'a' <*%> sigX 'b') `break` pressOn 'q'

add :: Either String Int -> Either String Int -> Either String (Int, Int, Int)
ea `add` eb = (\a b -> (a, b, a + b)) <$> ea <*> eb

output :: Either String (Int, Int, Int) -> IO ()
output = \case
	Left err -> putStrLn $ "Error: " ++ err
	Right (a, b, ab) -> putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show ab

key :: React s (Singleton Key) Char
key = await KeyReq \(OccKey c) -> c

pressOn :: Char -> React s (Singleton Key) ()
pressOn c = key >>= bool (pressOn c) (pure ()) . (c ==)

digit :: React s (Singleton Key) Int
digit = do
	c <- key
	bool digit (pure . read $ c : "") (isDigit c)

digits :: React s (Singleton Key) (Either String Int)
digits = atResult (const "error") (const "error") <$>
	scanl (\n -> (n * 10 +)) 0 (repeat digit) `at` pressOn '\n'

sigX :: Char -> Sig s (Singleton Key) (Either String Int) ()
sigX c = repeat $ pressOn c >> digits

handle :: Handle IO (Singleton Key)
handle = const $ Singleton . OccKey <$> getChar

withNoBuffering :: IO.Handle -> IO a -> IO a
withNoBuffering h act = bracket
	(IO.hGetBuffering h <* IO.hSetBuffering h IO.NoBuffering)
	(IO.hSetBuffering h)
	(const act)

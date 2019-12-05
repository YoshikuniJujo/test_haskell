{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ifs :: (Show b, Show c) => (forall a . Show a => a -> IO ()) -> Bool -> b -> c -> IO ()
ifs :: (Show b, Show c) => (forall a . Show a => a -> d) -> Bool -> b -> c -> d
ifs f True x _ = f x
ifs f False _ y = f y

main :: IO ()
main = do
	putStrLn "yes/no?"
	ans <- (<$> getLine) $ \case 'y' : _ -> True; _ -> False
	ifs print ans (123 :: Int) "hello"

dynamic :: IO ()
dynamic = do
	l <- getLine
	(\(f :: forall a . Show a => a -> IO ()) -> case l of
			"Int" -> f (123 :: Int); _ -> f "hello") $ \x -> do
		print x
		print x

ifs' :: (Show b, Show c) => Bool -> b -> c -> (forall a . Show a => a -> d) -> d
ifs' True x _ f = f x
ifs' False _ y f = f y

main' :: IO ()
main' = do
	putStrLn "yes/no?"
	ans <- (<$> getLine) $ \case 'y' : _ -> True; _ -> False
	ifs' ans (123 :: Int) "hello" $ \a -> do
	print a

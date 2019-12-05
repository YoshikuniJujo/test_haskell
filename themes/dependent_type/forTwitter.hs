{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

ifs :: (Show a, Show b) => Bool -> a -> b -> (forall x . Show x => x -> c) -> c
ifs True x _ f = f x
ifs False _ y f = f y

main :: IO ()
main = do
	ans <- (<$> getLine) $ \case 'y' : _ -> True; _ -> False
	ifs ans (123 :: Int) "hello" $ \a -> do
	print a

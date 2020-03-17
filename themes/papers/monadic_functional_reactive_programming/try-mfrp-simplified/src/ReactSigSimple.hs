{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ReactSigSimple where

import Control.Arrow

data React e a = Done a | Await (e -> React e a)

instance Functor (React e) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await k = Await $ (f <$>) . k

instance Applicative (React e) where
	pure = Done

data Sig e a r = Sig (React e (ISig e a r))
data ISig e a r = End r | a :| Sig e a r

apply :: React e a -> [e] -> Either (React e a) (a, [e])
Done x `apply` es = Right (x, es)
r `apply` [] = Left r
Await k `apply` (e : es) = k e `apply` es

applySig :: Sig e a r -> [e] -> ([a], Either (Sig e a r) r)
Sig s `applySig` es = case s `apply` es of
	Left s' -> ([], Left $ Sig s')
	Right (is, es') -> is `applyISig` es'

applyISig :: ISig e a r -> [e] -> ([a], Either (Sig e a r) r)
End r `applyISig` _ = ([], Right r)
(h :| t) `applyISig` [] = ([h], Left t)
(h :| t) `applyISig` occs = (h :) `first` (t `applySig` occs)

interpret :: Monad m => m [e] -> React e a -> m (a, [e])
interpret _ (Done x) = pure (x, [])
interpret p a@(Await _) = p >>= \occs -> case a `apply` occs of
	Left a' -> interpret p a'; Right r -> pure r

interpretSig :: Monad m => m [e] -> (a -> m ()) -> Sig e a r -> m r
interpretSig p d s = p >>= \occs -> do
	let	(xs, sr) = s `applySig` occs
	d `mapM_` xs
	case sr of
		Left s' -> interpretSig p d s'
		Right r -> pure r

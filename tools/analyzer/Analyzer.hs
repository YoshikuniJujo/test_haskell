module Analyzer (
	Analyzer, runAnalyzer, eof, spot, token, tokens, tokensWhile, listAll) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import qualified ListLike as LL

data Analyzer a b = Analyzer { runAnalyzer :: a -> Maybe (b, a) }

instance Monad (Analyzer a) where
	return = Analyzer . (Just .) . (,)
	a1 >>= a2 = Analyzer $ \x -> case runAnalyzer a1 x of
		Just (x', s) -> runAnalyzer (a2 x') s
		_ -> Nothing
	fail _ = Analyzer $ const Nothing

instance Functor (Analyzer a) where
	fmap = liftM

instance Applicative (Analyzer a) where
	pure = return
	mf <*> mx = do
		f <- mf
		x <- mx
		return $ f x

eof :: LL.ListLike a => Analyzer a Bool
eof = Analyzer $ \x -> Just (isNothing $ LL.uncons x, x)

spot :: LL.ListLike a => (LL.Element a -> Bool) -> Analyzer a (LL.Element a)
spot p = Analyzer $ \x -> case LL.uncons x of
	Just (h, t) | p h -> Just (h, t)
	_ -> Nothing

token :: LL.ListLike a => Analyzer a (LL.Element a)
token = spot $ const True

tokens :: LL.ListLike a => Integer -> Analyzer a a
tokens = Analyzer . (Just .) . LL.splitAt

tokensWhile :: LL.ListLike a => (LL.Element a -> Bool) -> Analyzer a a
tokensWhile = Analyzer . (Just .) . LL.span

listAll :: LL.ListLike a => Analyzer a b -> Analyzer a [b]
listAll a = do
	e <- eof
	if e then return [] else (:) <$> a <*> listAll a

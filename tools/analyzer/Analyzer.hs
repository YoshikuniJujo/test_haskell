module Analyzer (
	Analyzer, runAnalyzer,
	eof, spot, token, tokens, tokensWhile, listAll
	) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (liftM, ap)

import qualified ListLike as LL

data Analyzer a b =
	Analyzer { runAnalyzer :: a -> Maybe (b, a) }

instance Monad (Analyzer a) where
	return = Analyzer . (Just .) . (,)
	a1 >>= a2 = Analyzer $ \s ->
		case runAnalyzer a1 s of
			Just (x, s') -> runAnalyzer (a2 x) s'
			_ -> Nothing
	fail _ = Analyzer $ const Nothing
instance Functor (Analyzer a) where fmap = liftM
instance Applicative (Analyzer a) where pure = return; (<*>) = ap

eof :: LL.ListLike a => Analyzer a Bool
eof = Analyzer $ Just . ((,) <$> LL.null <*> id)

spot :: LL.ListLike a =>
	(LL.Element a -> Bool) -> Analyzer a (LL.Element a)
spot p = Analyzer $ \s -> case LL.uncons s of
	Just (h, t) | p h -> Just (h, t)
	_ -> Nothing

token :: LL.ListLike a => Analyzer a (LL.Element a)
token = spot $ const True

tokens :: LL.ListLike a => Integer -> Analyzer a a
tokens = Analyzer . (Just .) . LL.splitAt

tokensWhile :: LL.ListLike a =>
	(LL.Element a -> Bool) -> Analyzer a a
tokensWhile = Analyzer . (Just .) . LL.span

listAll :: LL.ListLike a => Analyzer a b -> Analyzer a [b]
listAll = loopWhile eof

loopWhile :: Monad m => m Bool -> m a -> m [a]
loopWhile p m = do
	e <- p
	if e then return [] else
		(:) `liftM` m `ap` loopWhile p m

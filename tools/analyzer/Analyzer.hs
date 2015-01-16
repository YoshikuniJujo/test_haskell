module Analyzer (
	Analyzer, runAnalyzer, eof, spot, token,
	tokens, tokensWhile, listAll, listMap ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (liftM, ap)

import qualified ListLike as LL

moduleName :: String
moduleName = "Analyzer"

data Analyzer s a = Analyzer {
	runAnalyzer :: s -> Either String (a, s) }

instance Monad (Analyzer s) where
	return = Analyzer . (Right .) . (,)
	a >>= b = Analyzer $ \s ->
		case runAnalyzer a s of
			Right (x, t) -> runAnalyzer (b x) t
			Left err -> Left err
	fail = Analyzer . const . Left

instance Functor (Analyzer s) where
	fmap = liftM

instance Applicative (Analyzer s) where
	pure = return; (<*>) = ap

eof :: LL.ListLike s => Analyzer s Bool
eof = Analyzer $ Right . ((,) <$> LL.null <*> id)

spot :: LL.ListLike s =>
	(LL.Element s -> Bool) -> Analyzer s (LL.Element s)
spot p = Analyzer $ \s -> case LL.uncons s of
	Just (h, t) | p h -> Right (h, t)
	_ -> Left $ moduleName ++ ".spot: parse error"

token :: LL.ListLike s => Analyzer s (LL.Element s)
token = spot $ const True

tokens :: LL.ListLike s => Integer -> Analyzer s s
tokens = Analyzer . (Right .) . LL.splitAt

tokensWhile :: LL.ListLike s =>
	(LL.Element s -> Bool) -> Analyzer s s
tokensWhile = Analyzer . (Right .) . LL.span

listAll :: LL.ListLike s => Analyzer s a -> Analyzer s [a]
listAll = loopUntil eof

loopUntil :: Monad m => m Bool -> m a -> m [a]
loopUntil p m = do
	e <- p
	if e then return [] else
		(:) `liftM` m `ap` loopUntil p m

listMap :: LL.ListLike b =>
	(a -> Analyzer b c) -> [a] -> Analyzer b [c]
listMap = mapUntil eof

mapUntil :: Monad m => m Bool -> (a -> m b) -> [a] -> m [b]
mapUntil _ _ [] = return []
mapUntil p m (x : xs) = do
	e <- p
	if e then return [] else
		(:) `liftM` m x `ap` mapUntil p m xs

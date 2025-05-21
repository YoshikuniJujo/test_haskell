{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe (

	-- * TYPE

	P,

	-- * GET/PUT

	isEmpty, isMore, await, awaitMaybe, yield,

	-- * PIPE

	(=$=), (=@=),

	-- * RUN

	run

	) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as F
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as Fn
import Data.FTCQueue qualified as Q
import Data.Bool

data P f i o a where
	IsMore :: forall f i o . P f i o Bool
	Await :: P f i o i
	Yield :: forall f i o . o -> P f i o ()
	(:=$=) :: forall f i x o r r' .
		f i x r -> f x o r' -> P f i o (f i x r, f x o r')
	(:=@=) :: forall f i x o r r' .
		f i x r -> f x o r' -> P f i o (f i x r, f x o r')

isEmpty, isMore :: U.Member P es => Eff.E es i o Bool
isEmpty = not <$> Eff.effh IsMore
isMore = Eff.effh IsMore

await :: U.Member P es => Eff.E es i o i
await = Eff.effh Await

awaitMaybe :: U.Member P es => Eff.E es i o (Maybe i)
awaitMaybe = isMore >>= bool (pure Nothing) (Just <$> await)

yield :: forall es i o . U.Member P es => o -> Eff.E es i o ()
yield = Eff.effh . Yield

(=$=), (=@=) :: forall es i x o r r' . U.Member P es =>
	Eff.E es i x r -> Eff.E es x o r' ->
	Eff.E es i o (Eff.E es i x r, Eff.E es x o r')
(=$=) = (Eff.effh .) . (:=$=); (=@=) = (Eff.effh .) . (:=@=)

run :: Fn.Tight (U.U es) => Eff.E (P ': es) i o a -> Eff.E es i o (Maybe a)
run = \case
	F.Pure x -> F.Pure $ Just x
	u F.:>>= q -> case U.decomp u of
		Left u' -> Fn.mapT run Just u' F.:>>=
			Q.singleton (maybe (pure Nothing) (run . (q F.$)))
		Right IsMore -> pure Nothing
		Right Await -> pure Nothing
		Right (Yield _) -> pure Nothing
		Right (o :=$= p) ->
			run (o =$=! p) >>= maybe (pure Nothing) (run F.. q)
		Right (o :=@= p) ->
			run (o =@=! p) >>= maybe (pure Nothing) (run F.. q)

(=$=!) :: forall es i x o r r' . Fn.Tight (U.U es) =>
	Eff.E (P ': es) i x r -> Eff.E (P ': es) x o r' ->
	Eff.E (P ': es) i o (Eff.E (P ': es) i x r, Eff.E (P ': es) x o r')
o =$=! p@(F.Pure _) = F.Pure (o, p)
o@(F.Pure _) =$=! p@(v F.:>>= r) = case U.decomp v of
	Left v' -> U.weaken (Fn.mapT (o =$=!) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$=! (r F.$ y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r F.$) =<< p')
			_ -> error "never occur"
	Right (o' :=$= p') -> o =$=! ((r F.$) =<< o' =$=! p')
	Right (o' :=@= p') -> o =$=! ((r F.$) =<< o' =@=! p')
	Right IsMore -> o =$=! (r F.$ False)
	Right Await -> F.Pure (o, p)
	Right (Yield ot) ->
		U.injh (Yield @_ @i ot) F.:>>= Q.singleton ((o =$=!) F.. r)
o@(u F.:>>= q) =$=! p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(_, Left v') -> U.weaken (Fn.mapT (o =$=!) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$=! (r F.$ y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r F.$) =<< p')
			_ -> error "never occur"
	(_, Right (o' :=$= p')) -> o =$=! ((r F.$) =<< (o' =$=! p'))
	(_, Right (o' :=@= p')) -> o =$=! ((r F.$) =<< (o' =@=! p'))
	(_, Right (Yield ot)) ->
		U.injh (Yield @_ @i ot) F.:>>= Q.singleton ((o =$=!) F.. r)
	(Right IsMore, _) ->
		U.injh (IsMore @_ @_ @o) F.:>>= Q.singleton ((=$=! p) F.. q)
	(Right Await, _) ->
		U.injh (Await @_ @_ @o) F.:>>= Q.singleton ((=$=! p) F.. q)
	(Right (Yield _), Right IsMore) -> o =$=! (r F.$ True)
	(Right (o' :=$= p'), Right IsMore) -> ((q F.$) =<< (o' =$=! p')) =$=! p
	(Right (o' :=@= p'), Right IsMore) -> ((q F.$) =<< (o' =@=! p')) =$=! p
	(Right (Yield ot), Right Await) -> (q F.$ ()) =$=! (r F.$ ot)
	(Right (o' :=$= p'), Right Await) -> ((q F.$) =<< (o' =$=! p')) =$=! p
	(Right (o' :=@= p'), Right Await) -> ((q F.$) =<< (o' =@=! p')) =$=! p
	(Left u', _) -> U.weaken (Fn.mapT (=$=!! p) ((, p) . F.Pure) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q F.$ x) =$=! p'
			(o', p'@(F.Pure _)) -> F.Pure ((q F.$) =<< o', p')
			_ -> error "never occur"

(=$=!!) :: forall es i x o r r' . Fn.Tight (U.U es) =>
	Eff.E (P ': es) i x r -> Eff.E (P ': es) x o r' ->
	Eff.E (P ': es) i o (Eff.E (P ': es) i x r, Eff.E (P ': es) x o r')
o =$=!! p@(F.Pure _) = F.Pure (o, p)
o@(F.Pure _) =$=!! p@(v F.:>>= r) = case U.decomp v of
	Left v' -> U.weaken (Fn.mapT (o =$=!!) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$=!! (r F.$ y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r F.$) =<< p')
			_ -> error "never occur"
	Right (o' :=$= p') -> o =$=!! ((r F.$) =<< o' =$=! p')
	Right (o' :=@= p') -> o =$=!! ((r F.$) =<< o' =@=! p')
	Right IsMore -> F.Pure (o, p)
	Right Await -> F.Pure (o, p)
	Right (Yield ot) ->
		U.injh (Yield @_ @i ot) F.:>>= Q.singleton ((o =$=!!) F.. r)
o@(u F.:>>= q) =$=!! p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(_, Left v') -> U.weaken (Fn.mapT (o =$=!!) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$=!! (r F.$ y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r F.$) =<< p')
			_ -> error "never occur"
	(_, Right (o' :=$= p')) -> o =$=!! ((r F.$) =<< (o' =$=! p'))
	(_, Right (o' :=@= p')) -> o =$=!! ((r F.$) =<< (o' =@=! p'))
	(_, Right (Yield ot)) ->
		U.injh (Yield @_ @i ot) F.:>>= Q.singleton ((o =$=!!) F.. r)
	(Right IsMore, _) ->
		U.injh (IsMore @_ @_ @o) F.:>>= Q.singleton ((=$=!! p) F.. q)
	(Right Await, _) ->
		U.injh (Await @_ @_ @o) F.:>>= Q.singleton ((=$=!! p) F.. q)
	(Right (Yield _), Right IsMore) -> o =$=!! (r F.$ True)
	(Right (o' :=$= p'), Right IsMore) -> ((q F.$) =<< (o' =$=! p')) =$=!! p
	(Right (o' :=@= p'), Right IsMore) -> ((q F.$) =<< (o' =@=! p')) =$=!! p
	(Right (Yield ot), Right Await) -> (q F.$ ()) =$=!! (r F.$ ot)
	(Right (o' :=$= p'), Right Await) -> ((q F.$) =<< (o' =$=! p')) =$=!! p
	(Right (o' :=@= p'), Right Await) -> ((q F.$) =<< (o' =@=! p')) =$=!! p
	(Left u', _) -> U.weaken (Fn.mapT (=$=!! p) ((, p) . F.Pure) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q F.$ x) =$=!! p'
			(o', p'@(F.Pure _)) -> F.Pure ((q F.$) =<< o', p')
			_ -> error "never occur"

(=@=!) :: forall es i x o r r' . Fn.Tight (U.U es) =>
	Eff.E (P ': es) i x r -> Eff.E (P ': es) x o r' ->
	Eff.E (P ': es) i o (Eff.E (P ': es) i x r, Eff.E (P ': es) x o r')
o@(F.Pure _) =@=! p = F.Pure (o, p)
o@(u F.:>>= q) =@=! p@(F.Pure _) = case U.decomp u of
	Left u' -> U.weaken (Fn.mapT (=@=! p) ((, p) . F.Pure) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q F.$ x) =@=! p'
			(o', p'@(F.Pure _)) -> F.Pure ((q F.$) =<< o', p')
			_ -> error "never occur"
	Right (o' :=$= p') -> ((q F.$) =<< (o' =$=! p')) =@=! p
	Right (o' :=@= p') -> ((q F.$) =<< (o' =@=! p')) =@=! p
	Right (Yield _) -> F.Pure (o, p)
	Right IsMore ->
		U.injh (IsMore @_ @_ @o) F.:>>= Q.singleton ((=@=! p) F.. q)
	Right Await ->
		U.injh (Await @_ @_ @o) F.:>>= Q.singleton ((=@=! p) F.. q)
o@(u F.:>>= q) =@=! p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(Left u', _) -> U.weaken (Fn.mapT (=@=! p) ((, p) . F.Pure) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q F.$ x) =@=! p'
			(o', p'@(F.Pure _)) -> F.Pure ((q F.$) =<< o', p')
			_ -> error "never occur"
	(Right (o' :=$= p'), _) -> ((q F.$) =<< (o' =$=! p')) =@=! p
	(Right (o' :=@= p'), _) -> ((q F.$) =<< (o' =@=! p')) =@=! p
	(Right IsMore, _) ->
		U.injh (IsMore @_ @_ @o) F.:>>= Q.singleton ((=@=! p) F.. q)
	(Right Await, _) ->
		U.injh (Await @_ @_ @o) F.:>>= Q.singleton ((=@=! p) F.. q)
	(_, Right (Yield ot)) ->
		U.injh (Yield @_ @i ot) F.:>>= Q.singleton ((o =@=!) F.. r)
	(Right (Yield _), Right IsMore) -> o =@=! (r F.$ True)
	(Right (Yield ot), Right Await) -> (q F.$ ()) =@=! (r F.$ ot)
	(_, Right (o' :=$= p')) -> o =@=! ((r F.$) =<< (o' =$=! p'))
	(_, Right (o' :=@= p')) -> o =@=! ((r F.$) =<< (o' =@=! p'))
	(_, Left v') -> U.weaken (Fn.mapT (o =$=!) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =@=! (r F.$ y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r F.$) =<< p')
			_ -> error "never occur"

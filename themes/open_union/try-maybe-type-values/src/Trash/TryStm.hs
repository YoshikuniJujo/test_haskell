{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trash.TryStm where

import Control.Monad.State
import Control.Concurrent (threadDelay)
import Data.Type.Set
import Data.Type.Flip
import Data.UnionSet hiding (merge)
import Data.Bool
import MonadicFrp
import MonadicFrp.Handle
import MonadicFrp.ThreadId
import Trials.Followbox.ThreadId

data StmIntVersion = StmIntVersion Int deriving (Show, Eq, Ord)

nextVersion :: StmIntVersion -> StmIntVersion
nextVersion (StmIntVersion n) = StmIntVersion $ n + 1

data StoreStmInt = StoreStmIntReq ThreadId Int deriving Show
numbered 8 [t| StoreStmInt |]
instance Mrgable StoreStmInt where i1 `mrg` _i2 = i1
instance Request StoreStmInt where
	data Occurred StoreStmInt = OccStoreStmInt ThreadId StmIntVersion
		deriving Show

storeStmInt :: Int -> React (GetThreadId :- StoreStmInt :- 'Nil) StmIntVersion
storeStmInt n = adjust getThreadId >>= \ti ->
	maybe (storeStmInt n) pure =<< adjust (await (StoreStmIntReq ti n)
		\(OccStoreStmInt ti' v) -> bool Nothing (Just v) $ ti == ti')

data LoadStmInt = LoadStmIntReq deriving (Show, Eq, Ord)
numbered 8 [t| LoadStmInt |]
instance Request LoadStmInt where
	data Occurred LoadStmInt = OccLoadStmInt StmIntVersion Int

loadStmInt :: React (Singleton LoadStmInt) (StmIntVersion, Int)
loadStmInt = await LoadStmIntReq \(OccLoadStmInt v n) -> (v, n)

data Rollback = RollbackReq StmIntVersion deriving (Show, Eq, Ord)
numbered 8 [t| Rollback |]
instance Request Rollback where
	data Occurred Rollback = OccRollback StmIntVersion deriving Show

rollback :: StmIntVersion -> React (Singleton Rollback) ()
rollback v = bool (rollback v) (pure ()) =<<
	await (RollbackReq v) \(OccRollback v') -> v == v'

type StmIntEv = StoreStmInt :- LoadStmInt :- Rollback :- 'Nil

class StmIntState s where
	getVersionStmInt :: s -> (StmIntVersion, Int)
	putVersionStmInt :: s -> (StmIntVersion, Int) -> s
	rollbackStmInt  :: s -> StmIntVersion -> Either String s

handleStoreStmInt :: (Monad m, StmIntState s) =>
	Handle (StateT s m) (Singleton StoreStmInt)
handleStoreStmInt reqs = gets (fst . getVersionStmInt) >>= \v ->
	singleton (OccStoreStmInt ti v)
		<$ modify (`putVersionStmInt` (nextVersion v, n))
	where StoreStmIntReq ti n = extract reqs

handleLoadStmInt :: (Monad m, StmIntState s) =>
	Handle (StateT s m) (Singleton LoadStmInt)
handleLoadStmInt _reqs =
	singleton . uncurry OccLoadStmInt <$> gets getVersionStmInt

handleRollback :: (Monad m, StmIntState s) =>
	Handle (StateT s m) (Singleton Rollback)
handleRollback reqs = singleton (OccRollback v)
	<$ modify (either error id . (`rollbackStmInt` v))
	where RollbackReq v = extract reqs

handleStmInt :: (Monad m, StmIntState s) => Handle' (StateT s m) StmIntEv
handleStmInt =
	(Just <$>) . handleStoreStmInt `merge`
	(Just <$>) . handleLoadStmInt `merge`
	(Just <$>) . handleRollback

handleStmInt' :: StmIntState s => Handle' (StateT s IO) StmIntEv
handleStmInt' reqs = do
	lift $ threadDelay 1000000
	handleStmInt reqs

atomicModifyStmInt :: (Int -> (a, Int)) -> React
	(GetThreadId :- StoreStmInt :- LoadStmInt :- Rollback :- 'Nil) a
atomicModifyStmInt f = adjust loadStmInt >>= \(v, n) -> do
	let	(x, n') = f n
	adjust (storeStmInt n') >>= \v' -> flip (uncurry bool) (v == v')
		(adjust (rollback v) >> atomicModifyStmInt f, pure x)

add1 :: Sig (GetThreadId :- StoreStmInt :- LoadStmInt :- Rollback :- 'Nil) Int ()
add1 = do
	n <- waitFor $ atomicModifyStmInt (\i -> (i, i + 1))
	emit n
	add1

add3 :: Sig (GetThreadId :- StoreStmInt :- LoadStmInt :- Rollback :- 'Nil) (Int, Int, Int) ()
add3 = (,,) <$%> add1 <*%> add1 <*%> add1

data StdStmIntState = StdStmIntState [(StmIntVersion, Int)] deriving Show

instance StmIntState StdStmIntState where
	getVersionStmInt (StdStmIntState (vn : _)) = vn
	getVersionStmInt (StdStmIntState []) = error "bad boy"
	putVersionStmInt (StdStmIntState vns) vn = StdStmIntState $ vn : vns
	rollbackStmInt (StdStmIntState (vn0 : vn1@(v, _) : vns)) v'
		| v == v' = Right . StdStmIntState $ vn1 : vns ++ [vn0]
		| otherwise = rollbackStmInt (StdStmIntState vns) v'
--		| otherwise = Left $ "bad version: " ++ show v  ++ " " ++ show vns ++ " " ++ show v'
	rollbackStmInt (StdStmIntState (vns@[(v, _)])) v'
		| v == v' = Right $ StdStmIntState vns
		| otherwise = Left $ "cannot rollback: " ++ show vns ++ " " ++ show v'
	rollbackStmInt (StdStmIntState []) v' = Left $ "cannot rollback: " ++ show v'

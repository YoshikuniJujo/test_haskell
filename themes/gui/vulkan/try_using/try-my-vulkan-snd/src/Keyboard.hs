{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Keyboard (

	-- * KEY EVENT

	KeyEvent(..), Envs, newChans', sendKeys,

	-- * KEY SETS

	hjkl, gf, isHjkl, isGf

	) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map qualified as M
import Data.Bool
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Key qualified as GlfwG

type Envs = (M.Map GlfwG.Key (TVar GlfwG.KeyState), M.Map GlfwG.Key (TChan ()))

newChans' :: [GlfwG.Key] -> IO (TChan KeyEvent, Envs)
newChans' ks =
	(\k -> (k ,) <$> atomically (newTVar GlfwG.KeyState'Released)) `mapM` ks >>= \prej ->
	newChans_ ks >>= \(oke, kcs) ->
	pure (oke, (M.fromList prej, kcs))

newChans_ :: [GlfwG.Key] -> IO (TChan KeyEvent, M.Map GlfwG.Key (TChan ()))
newChans_ ks =
	atomically newTChan >>= \oke ->
	keyFlows oke ks >>= \kcs ->
	pure (oke, kcs)

keyFlows :: TChan KeyEvent -> [GlfwG.Key] -> IO (M.Map GlfwG.Key (TChan ()))
keyFlows oke = \case
	[] -> pure M.empty
	GlfwG.Key'H : ks -> do
		ip <- keyFlow0 oke GlfwG.Key'H
		ips <- keyFlows oke ks
		pure $ M.insert GlfwG.Key'H ip ips
	GlfwG.Key'G : ks -> do
		ip <- keyFlow0 oke GlfwG.Key'G
		ips <- keyFlows oke ks
		pure $ M.insert GlfwG.Key'G ip ips
	k : ks -> do
		ip <- keyFlow oke k
		ips <- keyFlows oke ks
		pure $ M.insert k ip ips

keyFlow0 :: TChan KeyEvent -> GlfwG.Key -> IO (TChan ())
keyFlow0 oke k = do
	ip <- atomically newTChan
	_ <- forkIO $ flow ip oke (repeat (Key k))
	pure ip

keyFlow :: TChan KeyEvent -> GlfwG.Key -> IO (TChan ())
keyFlow oke k = do
	ip <- atomically newTChan
	_ <- forkIO $ flow ip oke (First k : repeat (Key k))
	pure ip

flow :: TChan () -> TChan a -> [a] -> IO ()
flow ip op = \case
	[] -> error "no more"
	x : xs -> do
		atomically do
			_ <- readTChan ip
			writeTChan op x
		flow ip op xs

isHjkl :: GlfwG.Key -> Bool
isHjkl = (`elem` hjkl)

hjkl :: [GlfwG.Key]
hjkl = [GlfwG.Key'H, GlfwG.Key'J, GlfwG.Key'K, GlfwG.Key'L]

isGf :: GlfwG.Key -> Bool
isGf = (`elem` gf)

gf :: [GlfwG.Key]
gf = [GlfwG.Key'G, GlfwG.Key'F]

sendKeys :: GlfwG.Win.W sw -> Envs -> IO ()
sendKeys win (prej, kcs) = sendKey prej win kcs `mapM_` ks
	where ks = M.keys prej

sendKey ::
	M.Map GlfwG.Key (TVar GlfwG.KeyState) -> GlfwG.Win.W sw ->
	M.Map GlfwG.Key (TChan ()) -> GlfwG.Key -> IO ()
sendKey pre win kcs k =
	keyDown pre win k >>= bool (pure ()) (atomically $ writeTChan (kcs M.! k) ())

keyDown :: M.Map GlfwG.Key (TVar GlfwG.KeyState) -> GlfwG.Win.W sw -> GlfwG.Key -> IO Bool
keyDown st w k = do
	now <- GlfwG.Win.getKey w k
	pre <- atomically
		$ (readTVar $ st M.! k)
			<* writeTVar (st M.! k) now
	pure case (pre, now) of
		(GlfwG.KeyState'Released, GlfwG.KeyState'Pressed) -> True
		_ -> False

data KeyEvent = First GlfwG.Key | Key GlfwG.Key deriving Show

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeyboardOld (

	-- * KEY EVENT

	KeyEvent(..), Envs, newChans', sendKeys,

	-- * KEY SETS

	hjkl, gf, isHjkl, isGf

	) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map qualified as M
import Data.Bool
import Graphics.UI.GLFW qualified as Glfw

type Envs = (M.Map Glfw.Key (TVar Glfw.KeyState), M.Map Glfw.Key (TChan ()))

newChans' :: [Glfw.Key] -> IO (TChan KeyEvent, Envs)
newChans' ks =
	(\k -> (k ,) <$> atomically (newTVar Glfw.KeyState'Released)) `mapM` ks >>= \prej ->
	newChans_ ks >>= \(oke, kcs) ->
	pure (oke, (M.fromList prej, kcs))

newChans_ :: [Glfw.Key] -> IO (TChan KeyEvent, M.Map Glfw.Key (TChan ()))
newChans_ ks =
	atomically newTChan >>= \oke ->
	keyFlows oke ks >>= \kcs ->
	pure (oke, kcs)

keyFlows :: TChan KeyEvent -> [Glfw.Key] -> IO (M.Map Glfw.Key (TChan ()))
keyFlows oke = \case
	[] -> pure M.empty
	Glfw.Key'H : ks -> do
		ip <- keyFlow0 oke Glfw.Key'H
		ips <- keyFlows oke ks
		pure $ M.insert Glfw.Key'H ip ips
	Glfw.Key'G : ks -> do
		ip <- keyFlow0 oke Glfw.Key'G
		ips <- keyFlows oke ks
		pure $ M.insert Glfw.Key'G ip ips
	k : ks -> do
		ip <- keyFlow oke k
		ips <- keyFlows oke ks
		pure $ M.insert k ip ips

keyFlow0 :: TChan KeyEvent -> Glfw.Key -> IO (TChan ())
keyFlow0 oke k = do
	ip <- atomically newTChan
	_ <- forkIO $ flow ip oke (repeat (Key k))
	pure ip

keyFlow :: TChan KeyEvent -> Glfw.Key -> IO (TChan ())
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

isHjkl :: Glfw.Key -> Bool
isHjkl = (`elem` hjkl)

hjkl :: [Glfw.Key]
hjkl = [Glfw.Key'H, Glfw.Key'J, Glfw.Key'K, Glfw.Key'L]

isGf :: Glfw.Key -> Bool
isGf = (`elem` gf)

gf :: [Glfw.Key]
gf = [Glfw.Key'G, Glfw.Key'F]

sendKeys :: Glfw.Window -> Envs -> IO ()
sendKeys win (prej, kcs) = sendKey prej win kcs `mapM_` ks
	where ks = M.keys prej

sendKey ::
	M.Map Glfw.Key (TVar Glfw.KeyState) -> Glfw.Window ->
	M.Map Glfw.Key (TChan ()) -> Glfw.Key -> IO ()
sendKey pre win kcs k =
	keyDown pre win k >>= bool (pure ()) (atomically $ writeTChan (kcs M.! k) ())

keyDown :: M.Map Glfw.Key (TVar Glfw.KeyState) -> Glfw.Window -> Glfw.Key -> IO Bool
keyDown st w k = do
	now <- Glfw.getKey w k
	pre <- atomically
		$ (readTVar $ st M.! k)
			<* writeTVar (st M.! k) now
	pure case (pre, now) of
		(Glfw.KeyState'Released, Glfw.KeyState'Pressed) -> True
		_ -> False

data KeyEvent = First Glfw.Key | Key Glfw.Key deriving Show

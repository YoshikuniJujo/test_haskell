{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Keyboard (

	-- * KEY EVENT

	KeyEvent(..), Envs, newChans, sendKeys,

	-- * KEY SETS

	hjkl, gf, isHjkl, isGf

	) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map qualified as M
import Data.Bool
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Key qualified as GlfwG

newChans :: [GlfwG.Key] -> IO (TChan KeyEvent, Envs)
newChans ks =
	(\k -> (k ,) <$> atomically (newTVar GlfwG.KeyState'Released))
		`mapM` ks >>= \pks ->
	atomically newTChan >>= \oke -> keyFlows oke ks >>= \kps ->
	pure (oke, (M.fromList pks, kps))

keyFlows :: TChan KeyEvent -> [GlfwG.Key] -> IO (M.Map GlfwG.Key Pressed)
keyFlows oke = \case
	[] -> pure M.empty
	k : ks -> (bool keyFlow keyFlowNoFirst $ isGh k) oke k >>= \ip ->
		M.insert k ip <$> keyFlows oke ks

keyFlowNoFirst :: TChan KeyEvent -> GlfwG.Key -> IO Pressed
keyFlowNoFirst oke k = atomically newTChan >>= \ip ->
	ip <$ forkIO (flow ip oke . repeat $ Key k)

keyFlow :: TChan KeyEvent -> GlfwG.Key -> IO Pressed
keyFlow oke k = atomically newTChan >>= \ip ->
	ip <$ forkIO (flow ip oke $ First k : repeat (Key k))

data KeyEvent = First GlfwG.Key | Key GlfwG.Key deriving Show

sendKeys :: GlfwG.Win.W sw -> Envs -> IO ()
sendKeys win (pks, kps) = sendKey pks win kps `mapM_` M.keys pks

type Envs = (PreKeys, M.Map GlfwG.Key Pressed)

sendKey :: PreKeys ->
	GlfwG.Win.W sw -> M.Map GlfwG.Key Pressed -> GlfwG.Key -> IO ()
sendKey pks w kcs k = keyDown pks w k >>=
	bool (pure ()) (atomically $ writeTChan (kcs M.! k) ())

type Pressed = TChan ()

keyDown :: PreKeys -> GlfwG.Win.W sw -> GlfwG.Key -> IO Bool
keyDown pks w k = GlfwG.Win.getKey w k >>= \now -> do
	pk <- atomically $ (readTVar $ pks M.! k) <* writeTVar (pks M.! k) now
	pure case (pk, now) of
		(GlfwG.KeyState'Released, GlfwG.KeyState'Pressed) -> True
		_ -> False

type PreKeys = M.Map GlfwG.Key (TVar GlfwG.KeyState)

---------------------------------------------------------------------------

flow :: TChan () -> TChan a -> [a] -> IO ()
flow ip op = \case
	[] -> error "no more"
	x : xs -> atomically (readTChan ip >> writeTChan op x) >> flow ip op xs

---------------------------------------------------------------------------

isHjkl :: GlfwG.Key -> Bool
isHjkl = (`elem` hjkl)

hjkl :: [GlfwG.Key]
hjkl = [GlfwG.Key'H, GlfwG.Key'J, GlfwG.Key'K, GlfwG.Key'L]

isGf :: GlfwG.Key -> Bool
isGf = (`elem` gf)

gf :: [GlfwG.Key]
gf = [GlfwG.Key'G, GlfwG.Key'F]

gh :: [GlfwG.Key]
gh = [GlfwG.Key'G, GlfwG.Key'H]

isGh :: GlfwG.Key -> Bool
isGh = (`elem` gh)

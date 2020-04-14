{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Event where

import Data.Type.Set
import Data.Bool
import Network.HTTP.Simple (Header)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import MonadicFrp

import Trials.Followbox.Aeson
import Trials.Followbox.XGlyphInfo

data Result = Failure | Succeed deriving Show

result :: a -> a -> Result -> a
result f _ Failure = f
result _ s Succeed = s

data LeftClick = LeftClickReq deriving (Show, Eq, Ord)
numbered [t| LeftClick |]
instance Request LeftClick where
	data Occurred LeftClick = OccLeftClick deriving Show

leftClick :: React (Singleton LeftClick) ()
leftClick = await LeftClickReq \OccLeftClick -> ()

data HttpGet = HttpGetReq Uri deriving (Show, Eq, Ord)
type Uri = String
numbered [t| HttpGet |]
instance Request HttpGet where
	data Occurred HttpGet = OccHttpGet Uri [Header] LBS.ByteString
		deriving Show

httpGet :: Uri -> React (Singleton HttpGet) ([Header], LBS.ByteString)
httpGet u = maybe (httpGet u) pure =<< await (HttpGetReq u)
	\(OccHttpGet u' hs c) -> bool Nothing (Just (hs, c)) $ u == u'

data StoreJsons = StoreJsons [Object] deriving (Show, Eq, Ord)
numbered [t| StoreJsons |]
instance Request StoreJsons where
	data Occurred StoreJsons = OccStoreJsons [Object]

storeJsons :: [Object] -> React (Singleton StoreJsons) ()
storeJsons os = result (storeJsons os) (pure ()) =<< await (StoreJsons os)
	\(OccStoreJsons os') -> bool Failure Succeed $ os == os'

data LoadJsons = LoadJsonsReq deriving (Show, Eq, Ord)
numbered [t| LoadJsons |]
instance Request LoadJsons where
	data Occurred LoadJsons = OccLoadJsons [Object]

loadJsons :: React (Singleton LoadJsons) [Object]
loadJsons = await LoadJsonsReq \(OccLoadJsons os) -> os

data CalcTextExtents = CalcTextExtentsReq String Double T.Text
	deriving (Show, Eq, Ord)
numbered [t| CalcTextExtents |]
instance Request CalcTextExtents where
	data Occurred CalcTextExtents =
		OccCalcTextExtents String Double T.Text XGlyphInfo

calcTextExtents :: String -> Double -> T.Text ->
	React (Singleton CalcTextExtents) XGlyphInfo
calcTextExtents fn fs t = maybe (calcTextExtents fn fs t) pure
	=<< await (CalcTextExtentsReq fn fs t)
		\(OccCalcTextExtents fn' fs' t' glp) ->
			bool Nothing (Just glp)
				$ fn == fn' && fs == fs' && t == t'

data Quit = QuitReq deriving (Show, Eq, Ord)
numbered [t| Quit |]
instance Request Quit where data Occurred Quit = OccQuit

checkQuit :: React (Singleton Quit) ()
checkQuit = await QuitReq $ const ()

data Error = NotJson | NoLoginName | CatchError deriving (Show, Eq, Ord)

data ErrorResult = Continue | Terminate deriving Show

data RaiseError = RaiseError Error String deriving (Show, Eq, Ord)
numbered [t| RaiseError |]
instance Request RaiseError where
	data Occurred RaiseError = OccRaiseError Error ErrorResult

raiseError :: Error -> String -> React (Singleton RaiseError) ()
raiseError e em = bool (raiseError e em) (pure ()) =<< await (RaiseError e em)
	\(OccRaiseError e' _er) -> e == e'

catchError :: React (Singleton RaiseError) ErrorResult
catchError = await (RaiseError CatchError "") \(OccRaiseError _ er) -> er

type SigF = Sig FollowboxEv
type ISigF = ISig FollowboxEv
type ReactF = React FollowboxEv

type FollowboxEv = LeftClick :- HttpGet :- StoreJsons :- LoadJsons :- Quit :- RaiseError :- 'Nil

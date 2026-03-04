{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr.Event where

import Prelude hiding (break, scanl)
import Control.Moffy
import Data.Type.Set
import Data.Bool
import Data.Text qualified as T
import Nostr.Event qualified as Event
import Nostr.Filter qualified as Filter

data Req = ReqReq T.Text Filter.Filter deriving (Show, Eq, Ord)
numbered [t| Req |]
instance Request Req where
	data Occurred Req = OccReq deriving Show

data Event = EventReq deriving (Show, Eq, Ord)
numbered [t| Event |]
instance Request Event where
	data Occurred Event = OccEvent T.Text Event.E deriving Show

data Eose = EoseReq deriving (Show, Eq, Ord)
numbered [t| Eose |]
instance Request Eose where
	data Occurred Eose = OccEose T.Text deriving Show

data Halt = HaltReq deriving (Show, Eq, Ord)
numbered [t| Halt |]
instance Request Halt where
	data Occurred Halt = OccHalt deriving Show

data End = EndReq deriving (Show, Eq, Ord)
numbered [t| End |]
instance Request End where
	data Occurred End = OccEnd deriving Show

type Events = Req :- Event :- Eose :- Halt :- End :- 'Nil

request :: T.Text -> Filter.Filter -> React s Events ()
request nm flt = adjust $ await (ReqReq nm flt) (const ())

awaitEvent :: React s Events (T.Text, Event.E)
awaitEvent = adjust $ await EventReq \(OccEvent nm ev) -> (nm, ev)

awaitNameEvent :: T.Text -> React s Events Event.E
awaitNameEvent nm0 = do
	(nm, ev) <- awaitEvent
	if nm == nm0 then pure ev else awaitNameEvent nm0

end :: React s (Singleton End) ()
end = await EndReq $ const ()

halt :: React s Events ()
halt = adjust . await HaltReq $ const ()

awaitEose :: React s Events T.Text
awaitEose = adjust $ await EoseReq \(OccEose nm) -> nm

awaitNameEose :: T.Text -> React s Events ()
awaitNameEose nm0 = bool (awaitNameEose nm0) (pure ()) . (== nm0) =<< awaitEose

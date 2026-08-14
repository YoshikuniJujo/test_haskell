{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AddSignature where

import Nostr.Event qualified as Event
import Nostr.Event.NoPub qualified as NoPub
import Nostr.Event.Signed qualified as Signed

signature :: Event.Secret -> Event.Pub -> NoPub.E -> IO Signed.E
signature sk pk e = Signed.signature sk $ e `NoPub.addPubKey` pk

signature' :: Event.Secret -> Event.Pub -> NoPub.E -> Maybe Signed.E
signature' sk pk e = Signed.signature' sk $ e `NoPub.addPubKey` pk

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Reader.Internal (Reader(..), runReader, ask) where

import MyEff (Eff, Member, send, handleRelayS)

data Reader e a where Reader :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = send Reader

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
runReader m e0 = handleRelayS e0 (const pure) (\e Reader f -> f e e) m

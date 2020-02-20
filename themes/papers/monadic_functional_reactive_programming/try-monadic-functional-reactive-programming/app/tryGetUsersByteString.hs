{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Data.Set as S

import React
import Event

import Followbox
import Check.Followbox.GetUsers

main :: IO ()
main = interpret handle getUsersByteString >>= print

handle :: EvReqs FollowboxEvent -> IO (EvOccs FollowboxEvent)
handle evs = handle1 $ S.findMin evs

handle1 :: FollowboxEvent -> IO (EvOccs FollowboxEvent)
handle1 (Http uri _) = S.singleton . Http uri . Occurred <$> getUsers

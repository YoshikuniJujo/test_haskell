{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.TH.Sample where

import qualified Data.ByteString as BS

import Shaderc.TH

foo :: BS.ByteString
foo = [test| abc |]

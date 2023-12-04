{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Moffy.Samples.Followbox
import Control.Moffy.Samples.Followbox.RunGtkField

main :: IO ()
main = runFollowbox "firefox" followbox

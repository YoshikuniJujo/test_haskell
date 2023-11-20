{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyException where

import Control.Exception.Hierarchy

exceptionHierarchy Nothing $ ExNode "MyException" []

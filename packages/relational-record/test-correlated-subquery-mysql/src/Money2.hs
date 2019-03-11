{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Money2 where

import Control.Monad.IO.Class

import Lib

do	(hst, usr, pwd, db) <- liftIO $ do
		[h, u, d] <- lines <$> readFile "db_connect.info"
		p <- getPassword "Enter password: "
		return (h, u, p, d)
	defineTable hst usr pwd db "monies2"

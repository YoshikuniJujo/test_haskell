{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw.Viewable where

import Data.Type.Set
import Data.Time

newtype Message = Message String deriving Show
numbered [t| Message |]

putMessage :: Message -> IO ()
putMessage (Message msg) = do
	putStr "Message: "
	putStr msg
	putStr "("
	putStr . show =<< getCurrentTime
	putStrLn ")"

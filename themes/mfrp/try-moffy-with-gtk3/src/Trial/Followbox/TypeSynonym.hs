{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.TypeSynonym (
	-- * Field
	WindowTitle, Avatar,
	-- * GitHub
	GithubNameToken, GithubUserName, GithubToken,
	-- * Others
	Uri, Browser, ErrorMessage ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Codec.Picture (Image, PixelRGBA8)

---------------------------------------------------------------------------
-- * FIELD
-- * GITHUB
-- * OTHERS
---------------------------------------------------------------------------

-- FIELD

type WindowTitle = String
type Avatar = Image PixelRGBA8

-- GITHUB

type GithubNameToken = (GithubUserName, GithubToken)
type GithubUserName = ByteString
type GithubToken = ByteString

-- OTHERS

type Uri = Text
type Browser = FilePath
type ErrorMessage = String

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.TypeSynonym (
	-- * Field
	WindowTitle, Position, LineWidth, FontName, FontSize, Avatar,
	-- * GitHub
	GithubNameToken, GithubUserName, GithubToken,
	-- * Others
	Uri, Browser, ErrorMessage ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Codec.Picture (Image, PixelRGBA8)

---------------------------------------------------------------------------

-- FIELD

type WindowTitle = String
type Position = (Integer, Integer)
type LineWidth = Integer
type FontName = String
type FontSize = Double

-- GITHUB

type GithubNameToken = (GithubUserName, GithubToken)
type GithubUserName = ByteString
type GithubToken = ByteString

-- OTHERS

type Uri = Text
type Avatar = Image PixelRGBA8
type Browser = FilePath
type ErrorMessage = String

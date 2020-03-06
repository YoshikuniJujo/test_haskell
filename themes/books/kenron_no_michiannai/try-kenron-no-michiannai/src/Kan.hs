{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Kan where

data State
	= Sake | HinataKan | HitohadaKan | NuruKan
	| JouKan | AtsuKan | TobikiriKan | ChoutobikiriKan
	deriving Show

kanWoTsukeru :: State -> State
kanWoTsukeru = \case
	Sake -> HinataKan
	HinataKan -> HitohadaKan
	HitohadaKan -> NuruKan
	NuruKan -> JouKan
	JouKan -> AtsuKan
	AtsuKan -> TobikiriKan
	TobikiriKan -> ChoutobikiriKan

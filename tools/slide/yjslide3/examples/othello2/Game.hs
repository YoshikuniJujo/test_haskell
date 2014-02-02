module Game (
	Game,
	Turn(..),
	Stone(..),
	initGame,
	nextGame,
	turn,
	stones,
 ) where

data Game = Game Turn Board

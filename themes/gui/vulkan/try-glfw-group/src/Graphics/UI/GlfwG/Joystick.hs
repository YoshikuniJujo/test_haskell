{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.UI.GlfwG.Joystick (
	Joystick(..), JoystickState(..), JoystickButtonState(..),
	joystickPresent, joystickIsGamepad, getJoystickAxes, getJoystickButtons,
	getJoystickHats, JoystickHatState(..), getJoystickName, getJoystickGUID,
	setJoystickCallback, JoystickCallback
	) where

import Graphics.UI.GLFW

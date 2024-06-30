{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.UI.GlfwG.Gamepad (
	GamepadButton(..), GamepadAxis(..), GamepadButtonState(..),
	GamepadState(..), joystickIsGamepad, getGamepadName, getGamepadState,
	updateGamepadMappings
	) where

import Graphics.UI.GLFW

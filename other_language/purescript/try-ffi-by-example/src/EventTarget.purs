module EventTarget (
        addEventListener,
        EventLoad,
        class Eventable,
        class IsEvent, eventType, fromEvent, Event, class IsEventTarget, EventTarget, eventTarget,
        Options, optionsZero,
        Callback(..)
        ) where

import Control.Monad
import Data.Function
import Data.Unit
import Effect

import Body
import Window

addEventListener :: forall evtg ev . Eventable evtg ev =>
        evtg -> Options -> Callback ev -> Effect Unit
addEventListener evtg opts cb =
        addEventListenerRaw (eventTarget evtg) (eventType @ev) opts cb

addEventListenerRaw :: forall ev . IsEvent ev =>
        EventTarget -> String -> Options -> Callback ev -> Effect Unit
addEventListenerRaw evtg evtp opts (CallbackFunction fn) =
        js_addEventListenerFn evtg evtp opts $ handleEventToRaw fn
addEventListenerRaw evtg evtp opts (Foo hhe) =
        js_addEventListenerHhe evtg evtp opts $ handleEventToRaw' hhe

{-
addEventListenerRawFn :: EventTarget ->
        String -> Options -> (Event -> Effect Unit) -> Effect Unit
addEventListenerRawFn
-}

class (IsEventTarget evtg, IsEvent ev) <= Eventable evtg ev -- where

data Callback ev
        = CallbackFunction (ev -> Effect Unit)
        | Foo (forall r . { handleEvent :: ev -> Effect Unit | r })

type Options = {
        capture :: Boolean,
        once :: Boolean,
        passive :: Boolean }
--        signal :: Boolean }

optionsZero = { capture : false, once : false, passive : false} -- , signal : false }

data Event

data EventTarget

class IsEventTarget evtg where eventTarget :: evtg -> EventTarget

class IsEvent ev where
        eventType :: String
        fromEvent :: Event -> ev

foreign import js_addEventListenerFn ::
        EventTarget -> String -> Options -> (Event -> Effect Unit) -> Effect Unit

foreign import js_addEventListenerHhe :: forall r .
        EventTarget -> String -> Options -> { handleEvent :: Event -> Effect Unit | r } -> Effect Unit

data EventLoad

handleEventToRaw :: forall ev . IsEvent ev =>
        (ev -> Effect Unit) -> Event -> Effect Unit
handleEventToRaw h = h <<< fromEvent

handleEventToRaw' :: forall ev r . IsEvent ev =>
        { handleEvent :: ev -> Effect Unit | r } -> { handleEvent :: Event -> Effect Unit | r }
handleEventToRaw' he = he { handleEvent = handleEventToRaw he.handleEvent }

instance IsEvent EventLoad where
        eventType = "load"
        fromEvent = js_fromEvent

foreign import js_fromEvent :: forall ev . Event -> ev

instance IsEventTarget Body where eventTarget = eventTargetBody

foreign import eventTargetBody :: Body -> EventTarget

instance Eventable Body EventLoad
instance Eventable Window EventLoad

foreign import eventTargetWindow :: Window -> EventTarget

instance IsEventTarget Window where eventTarget = eventTargetWindow

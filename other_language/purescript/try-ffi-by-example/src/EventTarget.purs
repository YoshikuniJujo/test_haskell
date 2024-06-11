module EventTarget (
        addEventListener,
        EventLoad,
        class Eventable,
        class IsEvent, eventType, fromEvent, Event, class IsEventTarget, EventTarget, eventTarget,
        Options, optionsZero,
        Callback(..), currentTarget
        ) where

import Control.Monad
import Data.Function
import Data.Unit
import Effect

import Body
import Window

addEventListener :: forall evtg ev . Eventable evtg ev =>
        evtg -> Options -> Callback ev evtg -> Effect Unit
addEventListener evtg opts cb =
        addEventListenerRaw (eventTarget evtg) (eventType @ev) opts cb

addEventListenerRaw :: forall ev evtg . IsEvent ev =>
        EventTarget -> String -> Options -> Callback ev evtg -> Effect Unit
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

data Callback ev evtg
        = CallbackFunction (ev evtg -> Effect Unit)
        | Foo (forall r . { handleEvent :: ev evtg -> Effect Unit | r })

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
        fromEvent :: forall evtg . Event -> ev evtg
        currentTarget :: forall evtg . ev evtg -> evtg

foreign import js_addEventListenerFn ::
        EventTarget -> String -> Options -> (Event -> Effect Unit) -> Effect Unit

foreign import js_addEventListenerHhe :: forall r .
        EventTarget -> String -> Options -> { handleEvent :: Event -> Effect Unit | r } -> Effect Unit

data EventLoad evtg

handleEventToRaw :: forall ev evtg . IsEvent ev =>
        (ev evtg -> Effect Unit) -> Event -> Effect Unit
handleEventToRaw h = h <<< fromEvent

handleEventToRaw' :: forall ev evtg r . IsEvent ev =>
        { handleEvent :: ev evtg -> Effect Unit | r } -> { handleEvent :: Event -> Effect Unit | r }
handleEventToRaw' he = he { handleEvent = handleEventToRaw he.handleEvent }

instance IsEvent EventLoad where
        eventType = "load"
        fromEvent = js_fromEvent
        currentTarget = js_currentTarget

foreign import js_fromEvent :: forall ev evtg . Event -> ev evtg
foreign import js_currentTarget :: forall ev evtg . ev evtg -> evtg

instance IsEventTarget Body where eventTarget = eventTargetBody

foreign import eventTargetBody :: Body -> EventTarget

instance Eventable Body EventLoad
instance Eventable Window EventLoad

foreign import eventTargetWindow :: Window -> EventTarget

instance IsEventTarget Window where
        eventTarget = eventTargetWindow

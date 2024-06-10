module EventTarget (
        addEventListener,
        EventLoad,
        class Eventable,
        class IsEvent, eventType, class IsEventTarget, EventTarget, eventTarget,
        Options, optionsZero,
        Callback(..), HandleEvent
        ) where

import Data.Unit
import Effect

import Body

addEventListener :: forall evtg ev . Eventable evtg ev =>
        evtg -> Options -> Callback ev -> Effect Unit
addEventListener evtg opts cb =
        addEventListenerRaw (eventTarget evtg) (eventType @ev) opts cb

addEventListenerRaw :: forall ev .
        EventTarget -> String -> Options -> Callback ev -> Effect Unit
addEventListenerRaw evtg evtp opts (CallbackFunction fn) =
        js_addEventListenerFn evtg evtp opts fn
addEventListenerRaw evtg evtp opts (HasHandleEvent hhe) =
        js_addEventListenerHhe evtg evtp opts hhe

class (IsEventTarget evtg, IsEvent ev) <= Eventable evtg ev -- where

data Callback ev
        = CallbackFunction (ev -> Effect Unit)
        | HasHandleEvent (HandleEvent ev)

type Options = {
        capture :: Boolean,
        once :: Boolean,
        passive :: Boolean }
--        signal :: Boolean }

optionsZero = { capture : false, once : false, passive : false} -- , signal : false }

data EventTarget

class IsEventTarget evtg where eventTarget :: evtg -> EventTarget
class IsEvent ev where eventType :: String

data HandleEvent ev

foreign import js_addEventListenerFn :: forall ev .
        EventTarget -> String -> Options -> (ev -> Effect Unit) -> Effect Unit

foreign import js_addEventListenerHhe :: forall ev .
        EventTarget -> String -> Options -> HandleEvent ev -> Effect Unit

data EventLoad

instance IsEvent EventLoad where eventType = "load"
instance IsEventTarget Body where eventTarget = eventTargetBody

foreign import eventTargetBody :: Body -> EventTarget

instance Eventable Body EventLoad

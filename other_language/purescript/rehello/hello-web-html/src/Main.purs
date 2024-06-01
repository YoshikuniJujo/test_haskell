module Main where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Console (log, error) as Console

import Web.DOM.Document (createElement) as DOM
import Web.DOM.Element (toNode) as DOM
import Web.DOM.Node (appendChild, setTextContent) as DOM
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.HTML.HTMLElement (toNode) as HTML
import Web.HTML.Window (document) as HTML

import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML.HTMLElement as HTMLElement

main :: Effect Unit
main = do
        Console.log "begin"
        hdoc <- HTML.window >>= HTML.document
        mbd <- HTML.body hdoc
        let     doc = HTML.toDocument hdoc

        case mbd of
                Nothing -> Console.error "no body"
                Just bd -> do
                        pn <- DOM.toNode <$> DOM.createElement "p" doc
                        DOM.appendChild pn (HTML.toNode bd)
                        DOM.setTextContent "Hello, world!" pn
                        btn <- DOM.toNode <$> DOM.createElement "button" doc
                        let     mbtn = HTMLElement.fromNode btn
                        DOM.appendChild btn pn
                        DOM.setTextContent "button" btn
                        lstn <- EventTarget.eventListener $ \_ -> Console.log "clicked"
                        case mbtn of
                                Nothing -> Console.log "bad"
                                Just btn' -> EventTarget.addEventListener
                                        (Event.EventType "click") lstn
                                        false
                                        (HTMLElement.toEventTarget btn')

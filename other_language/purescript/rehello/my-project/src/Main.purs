module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console
import Graphics.Canvas (CanvasElement)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Halogen.Query.Event (eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.TouchEvent as TE
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.Touch as TE
import Web.TouchEvent.TouchList as TEL
import Web.TouchEvent.EventTypes as TET

canvasId :: String
canvasId = "myCanvas"

main :: Effect Unit
main = HA.runHalogenAff do
        body <- HA.awaitBody
        runUI component unit body

data Action =
        Initialize |
        Increment | Decrement |
        HandleKeyDown KE.KeyboardEvent |
        HandleMouseDown ME.MouseEvent |
        HandleMouseUp ME.MouseEvent |
        HandleTouchStart TE.TouchEvent

component = H.mkComponent {
        initialState,
        render,
        eval: H.mkEval $ H.defaultEval {
                handleAction = handleAction,
                initialize = Just Initialize
                } }
        where
        initialState _ = Tuple 0 0
        render (Tuple state state') =
                HH.canvas [
                        HP.width 480,
                        HP.height 800,
                        HP.id canvasId,
--                        HE.onMouseUp HandleMouseUp
                        HE.onTouchStart HandleTouchStart
                        ]
                {- HH.div_ [
                HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ],
                HH.div_ [ HH.text $ show state ],
                HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ] ] -}
        handleAction = case _ of
                Initialize -> do
                        H.liftEffect $ log "Initialize"
                        document <- H.liftEffect $ Window.document =<< window
                        H.subscribe' \_ ->
                                eventListener
                                        KET.keydown
                                        (HTMLDocument.toEventTarget document)
                                        (map HandleKeyDown <<< KE.fromEvent)
                                        {-
                        H.subscribe' \_ ->
                                eventListener
                                        MET.mousedown
                                        (HTMLDocument.toEventTarget document)
                                        (map HandleMouseDown <<< ME.fromEvent)
                                        -}
                        maybeCanvas <- H.liftEffect $ Canvas.getCanvasElementById canvasId
                        H.liftEffect
                                $ case maybeCanvas of
                                        Nothing -> pure unit
                                        Just canvas -> draw canvas 20 30
                Increment -> do
                        H.liftEffect $ log "Increment"
                        H.modify_ \(Tuple state state') -> Tuple (state + 1) state'
                Decrement -> do
                        H.liftEffect $ log "Decrement"
                        H.modify_ \(Tuple state state') -> Tuple (state - 1) state'
                HandleKeyDown ke -> do
                        H.liftEffect $ log "HandleKeyDown"
                        H.liftEffect $ log $ KE.key ke
                HandleMouseDown me -> do
                        H.liftEffect $ log "HandleMouseDown"
                        H.liftEffect $ log $ show $ ME.clientX me
                        H.liftEffect $ log $ show $ ME.clientY me
                        H.modify_ \_ -> Tuple (ME.clientX me) (ME.clientY me)
                        maybeCanvas <- H.liftEffect $ Canvas.getCanvasElementById canvasId
                        H.liftEffect
                                $ case maybeCanvas of
                                        Nothing -> pure unit
                                        Just canvas -> draw canvas (ME.clientX me) (ME.clientY me)
                HandleMouseUp me -> do
                        H.liftEffect $ log "HandleMouseUp"
                        H.liftEffect $ log $ show $ ME.clientX me
                        H.liftEffect $ log $ show $ ME.clientY me
                HandleTouchStart te -> do
                        H.liftEffect $ log "HandleTouchStart"
                        let     tl = TE.touches te
                                mt = TEL.item 0 tl
                        case mt of
                                Nothing -> pure unit
                                Just t -> H.modify_ \_ -> Tuple (TE.clientX t) (TE.clientY t)

draw :: CanvasElement -> Int -> Int -> Effect Unit
draw canvas x y = do
        ctx <- Canvas.getContext2D canvas
        Canvas.beginPath ctx
        Canvas.rect ctx {
                x : toNumber x,
                y : toNumber y,
                width : 20.0,
                height : 20.0 }
        Canvas.setFillStyle ctx "#FF0000"
        Canvas.fill ctx
        Canvas.closePath ctx

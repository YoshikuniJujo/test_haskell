module AbortSignal where

data AbortSignal

foreign import js_timeout :: Number -> AbortSignal

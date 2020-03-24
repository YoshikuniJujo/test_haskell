module Followbox.Component (followbox) where

followbox = mkComponent {
        initialState: \_ -> {},
        render: \_ -> text "Hello, world!",
        eval: mkEval $ defaultEval { handleAction = \(Identity a) -> pure a } }

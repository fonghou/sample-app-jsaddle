{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
    ( jsaddleApp, jsaddleOr )
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif

import Control.Lens
import Data.Generics.Product
import GHC.Generics
import Miso
import Miso.String

-- Type synonym for an application model
-- type Model = Int
data Model = Model {_counter :: Int}
  deriving (Show, Eq, Generic)

--  Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

counter :: Lens' Model Int
counter = field' @"_counter"

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = Model 0 -- initial model
    update = fromTransition . updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = error "logLeval Off" -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Action Model ()
updateModel AddOne = counter += 1
updateModel SubtractOne = counter -= 1
updateModel NoOp = pure ()
updateModel SayHelloWorld =
  scheduleIO_ (consoleLog "Hello World")

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
  div_
    []
    [ button_ [onClick AddOne] [text "+"],
      text (ms $ x ^. counter),
      button_ [onClick SubtractOne] [text "-"]
    ]

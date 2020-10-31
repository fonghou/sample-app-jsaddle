{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
    ( jsaddleApp, jsaddleOr )
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif

#ifndef __GHCJS__
import Servant.Client
#else
import Servant.Client.Ghcjs
#endif

import qualified Button
import Control.Lens
import Miso
import Miso.String

data Model = Model
  { _mLeftButton :: Button.Model,
    _mValue :: Int,
    _mRightButton :: Button.Model
  }
  deriving (Eq)

makeLenses ''Model

data Action
  = LeftButtonAction Button.Action
  | RightButtonAction Button.Action
  | SubtractOne
  | AddOne
  | ManyClicksWarning !Int
  | NoOp
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { _mLeftButton = Button.initialModel "-",
      _mValue = 0,
      _mRightButton = Button.initialModel "+"
    }

updateModel :: Action -> Transition Action Model ()
updateModel action = case action of
  LeftButtonAction act -> do
    -- Update the component's model, with whatever side effects it may have
    zoom mLeftButton $ Button.updateModel iLeftButton act
    pure ()
  RightButtonAction act -> do
    zoom mRightButton $ Button.updateModel iRightButton act
    pure ()
  SubtractOne -> do
    mValue -= 1
  AddOne -> do
    mValue += 1
  ManyClicksWarning i -> Miso.scheduleIO_ $ do
    Miso.consoleLog "Ouch! You're clicking too many times!"
    Miso.consoleLog (toMisoString i <> " is way too much for me to handle!")
  NoOp -> pure ()

-- Call the component's `viewModel` where you want it to be drawn
viewModel :: Model -> View Action
viewModel m =
  div_
    []
    [ Button.viewModel iLeftButton $ m ^. mLeftButton,
      text $ m ^. mValue . to show . to toMisoString,
      Button.viewModel iRightButton $ m ^. mRightButton
    ]

-- Filling in the Interface values for both buttons
iLeftButton :: Button.Interface Action
iLeftButton =
  Button.Interface
    { Button.dispatch = LeftButtonAction,
      Button.click = SubtractOne,
      Button.manyClicks = ManyClicksWarning
    }

iRightButton :: Button.Interface Action
iRightButton =
  Button.Interface
    { Button.dispatch = RightButtonAction,
      Button.click = AddOne,
      Button.manyClicks = ManyClicksWarning
    }

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 9090 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model = initialModel
    update = fromTransition . updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = error "logLeval Off" -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

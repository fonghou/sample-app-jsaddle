module Imports where

import Data.Aeson as JSON

jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions{fieldLabelModifier = Prelude.drop 1}

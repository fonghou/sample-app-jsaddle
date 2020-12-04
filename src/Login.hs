{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Login where

import Control.Lens as Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson.TH
import Data.Char
import Data.List.NonEmpty
import Imports
import Miso
import Miso.String as S
import Validation

data Form' a b c = Form
  { _username :: a,
    _password :: b,
    _rememberMe :: c
  }
  deriving (Eq, Show)

makeLenses ''Form'
deriveJSON jsonOptions ''Form'

data FormValidationError
  = EmptyName
  | ShortPassword
  | NoDigitPassword
  deriving (Show)

newtype Username = Username MisoString
  deriving (Show)

newtype Password = Password MisoString
  deriving (Show)

type Form = Form' MisoString MisoString Checked

type FormParsed = Form' Username Password Checked

type Validated a = Validation (NonEmpty FormValidationError) a

validateName :: MisoString -> Validated Username
validateName name =
  Username name <$ failureIf (S.null (S.strip name)) EmptyName

validateShortPassword :: MisoString -> Validated Password
validateShortPassword password =
  Password password <$ failureIf (S.length (S.strip password) < 8) ShortPassword

validatePasswordDigit :: MisoString -> Validated Password
validatePasswordDigit password =
  Password password <$ failureUnless (S.any isDigit password) NoDigitPassword

validatePassword :: MisoString -> Validated Password
validatePassword p = validatePasswordDigit p *> validateShortPassword p

-- validatePassword = fmap Password . validateAll
--     [ (`failureIf`     ShortPassword)   . (< 8) . T.length
--     , (`failureUnless` NoDigitPassword) . T.any isDigit
--     ]

validateForm :: Form -> Validated FormParsed
validateForm Form {..} =
  Form
    <$> validateName _username
    <*> validatePassword _password
    <*> Success _rememberMe

-- $> validateForm form1
form1 :: Form
form1 = Form "\t  \n\r  " "xyz" (Checked False)

data Action
  = forall a. Show a => Update (Lens' Form a) a
  | Submit

notEmpty :: (MonadError MisoString m) => MisoString -> m MisoString
notEmpty str =
  if S.null str
    then throwError "This field cannot be empty"
    else return str

viewModule :: Form -> View Action
viewModule form = do
  div_
    []
    [ form_
        [class_ "ui form"]
        [ label_ [] [text "Username"],
          input_
            [ class_ "field",
              type_ "text",
              placeholder_ "Name",
              defaultValue_ $ _username form,
              onChange $ Update username,
              onInput $ Update username
            ],
          label_ [] [text "Password"],
          input_
            [ class_ "field",
              type_ "text",
              defaultValue_ $ _password form,
              placeholder_ "Password",
              onChange $ Update password,
              onInput $ Update password
            ],
          label_ [] [text "Remember Me"],
          input_
            [ class_ "ui checkbox",
              type_ "checkbox",
              onChecked $ Update rememberMe
            ],
          button_
            [ class_ "ui button",
              type_ "button",
              onClick Submit
            ] [text "Login"]
        ]
    ]


{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

#ifndef __GHCJS__
import qualified Servant.Client as Test
import Network.HTTP.Client (newManager, defaultManagerSettings)
#endif

import Control.Monad.Catch
import Data.Aeson as JSON
import Data.Text as T
import GHC.Generics (Generic)
import Miso
import Servant.API
import Servant.API.Generic
import Servant.Client.Generic
import Servant.Client.JSaddle
import Servant.Links (AsLink, allFieldLinks)
import qualified Network.HTTP.Types as HTTP

-- import Servant.API.NamedArgs
-- import Servant.Client.NamedArgs ()

-- data NamedApi route = NamedApi
--   { _namedGet :: route
--         :- Capture "table" String
--         :> OptionalNamedParam "select" String
--         :> OptionalNamedParam "and" String
--         :> OptionalNamedParam "or" String
--         :> OptionalNamedParam "order" String
--         :> OptionalNamedParam "limit" Int
--         :> OptionalNamedParam "offset" Int
--         :> Get '[JSON] Value,
--     _namedPagenate :: route
--         :- Capture "table" String
--         :> NamedHeader "prefer" Count
--         :> OptionalNamedParam "select" String
--         :> OptionalNamedParam "and" String
--         :> OptionalNamedParam "or" String
--         :> OptionalNamedParam "order" String
--         :> OptionalNamedParam "limit" Int
--         :> OptionalNamedParam "offset" Int
--         :> Get '[JSON] (Headers '[Header "Content-Range" String] Value),
--     _namedPut :: route
--         :- Capture "table" String
--         :> QueryParam "pkey" PKey
--         :> ReqBody '[JSON] Value
--         :> Put '[JSON] Value
--   }
--   deriving (Generic)

data Api route = Api
  { _get :: route
        :- Capture "table" String
        :> Header "Prefer" Count
        :> QueryParam "select" String
        :> QueryParam "and" String
        :> QueryParam "or" String
        :> QueryParam "order" String
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> Get '[JSON] (Headers '[Header "Content-Range" String] JSON.Value),
    _put :: route
        :- Capture "table" String
        :> QueryParam' [Required, Strict] "pkey" PKey
        :> ReqBody '[JSON] JSON.Value
        :> Put '[JSON] JSON.Value
  }
  deriving (Generic)

data Count = Exact | Planned | Estimated

instance ToHttpApiData Count where
  toQueryParam Exact = "count=exact"
  toQueryParam Planned = "count=planned"
  toQueryParam Estimated = "count=estimated"

instance ToHttpApiData PKey where
  toQueryParam x = "eq." <> T.pack (show x)

newtype PKey = PKey Integer deriving (Eq, Show)

data QueryArgs = QueryArgs
  { select :: Maybe String,
    and :: Maybe String,
    or :: Maybe String,
    order :: Maybe String,
    limit :: Maybe Int,
    offset :: Maybe Int,
    count :: Maybe Count
  }

defaults :: QueryArgs
defaults =
  QueryArgs
    { select = Nothing,
      and = Nothing,
      or = Nothing,
      order = Nothing,
      limit = Nothing,
      offset = Nothing,
      count = Just Estimated
    }

query :: String -> QueryArgs -> JSM ([HTTP.Header], JSON.Value)
query table QueryArgs {..} = do
  x <- _get apiClient table count select and or order limit offset
  return (getHeaders x, getResponse x)

upsert :: String -> PKey -> JSON.Value -> JSM JSON.Value
upsert = _put apiClient

-- $> :set -XOverloadedLabels

apiLink :: Api (AsLink Link)
apiLink = allFieldLinks

apiClient :: Api (AsClientT JSM)
apiClient = genericClientHoist $ \m -> runClient m >>= either throwM return

url :: BaseUrl
url = BaseUrl Http "localhost" 3000 ""

runClient :: ClientM a -> JSM (Either ClientError a)
runClient route = runClientM route $ mkClientEnv url

#ifndef __GHCJS__

apiClientIO :: Api (AsClientT IO)
apiClientIO = genericClientHoist $ \m -> runClientIO m >>= either throwM return

runClientIO :: Test.ClientM a -> IO (Either ClientError a)
runClientIO route = do
  manager <- newManager defaultManagerSettings
  Test.runClientM route $ Test.mkClientEnv manager url

#endif

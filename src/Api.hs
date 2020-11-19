{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

#ifdef __GHCJS__
import JavaScript.Web.XMLHttpRequest
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient (runClientMOrigin)
#else
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
#endif

import Miso
import Control.Exception
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)
import Servant.API
import Servant.API.Generic
import Servant.Client.Generic
import Servant.Links

data Api route = Api
  { _get :: route :- Capture "table" String :> Get '[JSON] Value,
    _put :: route :- ReqBody '[JSON] Value :> Put '[JSON] Value
  }
  deriving (Generic)

-- $> _get apiLink "actor"
--
apiLink :: Api (AsLink Link)
apiLink = allFieldLinks

-- $> _get apiClient "film_list"
--
apiClient :: Api (AsClientT IO)
apiClient = genericClientHoist $ \m -> runClient m >>= either throwIO return

url :: BaseUrl
url = BaseUrl Http "localhost" 3000 ""

#ifdef __GHCJS__

runClient :: ClientM a -> IO (Either ClientError a)
runClient route = runClientMOrigin route $ ClientEnv url

#else

runClient :: ClientM a -> IO (Either ClientError a)
runClient route = do
  manager <- newManager defaultManagerSettings
  runClientM route $ mkClientEnv manager url

#endif

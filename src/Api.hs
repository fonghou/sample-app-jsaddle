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

#ifndef __GHCJS__
import qualified Servant.Client as Test
import Network.HTTP.Client (newManager, defaultManagerSettings)
#endif

import Miso
import Control.Monad.Catch
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)
import Servant.API
import Servant.API.Generic
import Servant.Client.Generic
import Servant.Links
import Servant.Client.JSaddle

data Api route = Api
  { _get :: route :- Capture "table" String :> Get '[JSON] Value,
    _put :: route :- ReqBody '[JSON] Value :> Put '[JSON] Value
  }
  deriving (Generic)

-- > _get apiLink "actor"
--
apiLink :: Api (AsLink Link)
apiLink = allFieldLinks

-- > _get apiClient "film_list"
--
apiClient :: Api (AsClientT JSM)
apiClient = genericClientHoist $ \m -> runClient m >>= either throwM return

url :: BaseUrl
url = BaseUrl Http "localhost" 3000 ""

runClient :: ClientM a -> JSM (Either ClientError a)
runClient route = runClientM route $ mkClientEnv url

#ifndef __GHCJS__

-- $> _get apiClientIO "actor"
--

apiClientIO :: Api (AsClientT IO)
apiClientIO = genericClientHoist $ \m -> runClientIO m >>= either throwM return

runClientIO :: Test.ClientM a -> IO (Either ClientError a)
runClientIO route = do
  manager <- newManager defaultManagerSettings
  Test.runClientM route $ Test.mkClientEnv manager url

#endif

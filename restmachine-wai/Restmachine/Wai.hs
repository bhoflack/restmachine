module Restmachine.Wai
  (wrap)
  where
import Control.Lens ((^.))
import Control.Monad.State (runState)
import Network.Wai (Application, httpVersion, pathInfo, lazyRequestBody, requestMethod, requestHeaders, responseLBS)
import Restmachine.Core (run')
import Restmachine.Core.Routing (route)
import Restmachine.Core.Types (Resource (..), Request (..), responseStatus, responseHeaders, responseBody)

import qualified Network.Wai as W
import qualified Restmachine.Core.Types as R

-- | Wrap a restmachine 'Application' in a Wai Application.
wrap :: R.Application -> W.Application
wrap app req respond = do
  reqBody <- lazyRequestBody req
  let req' = InitialRequest (requestMethod req) (httpVersion req) (requestHeaders req) reqBody (pathInfo req)
      (res, req'')  = runState (route app) req'
  resp <- run' req'' res
  let resp' = responseLBS (resp ^. responseStatus) (resp ^. responseHeaders) (resp ^. responseBody)
  respond resp'
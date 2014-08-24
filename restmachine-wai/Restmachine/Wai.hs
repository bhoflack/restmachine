module Restmachine.Wai 
  (application)
  where
import Control.Lens ((^.))
import Network.Wai (Application, requestBody, requestMethod, requestHeaders, responseLBS)
import Restmachine.Core (run)
import Restmachine.Core.Types (Resource (..), Request (..), responseStatus, responseHeaders, responseBody)


-- | Transform a Restmachine 'Resource' to a WAI 'Application'.
application :: Resource -> Application
application res req respond = do
  reqBody <- requestBody req
  let req' = Request (requestMethod req) (requestHeaders req) reqBody
  (resp, _) <- run res req'
  let resp' = responseLBS (resp ^. responseStatus) (resp ^. responseHeaders) (resp ^. responseBody)
  respond resp'

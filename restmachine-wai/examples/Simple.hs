{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Warp (run)

import Control.Lens
import Restmachine.Core.Types
import Restmachine.Wai (wrap)

import qualified Network.HTTP.Types.Status as H

hello = defaultResource & (serviceAvailable .~ static True)
                        & (knownMethod      .~ static True)
                        & (methodAllowed    .~ static True)
                        & (forbidden        .~ static False)
                        & (response         .~ (static $ Response H.status200 [] "<html><body>foo</body></html>"))

notFound = defaultResource & (serviceAvailable .~ static True)
                           & (knownMethod      .~ static True)
                           & (methodAllowed    .~ static True)
                           & (forbidden        .~ static False)
                           & (response         .~ (static $ Response H.status404 [] "not found"))


app = Application [(["foo"], hello)] notFound

main = run 3000 $ wrap app

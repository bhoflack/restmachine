{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Warp (run)

import Control.Lens
import Restmachine.Core.Types
import Restmachine.Wai (application)

import qualified Network.HTTP.Types.Status as H

hello = defaultResource & (serviceAvailable .~ static True)
                        & (knownMethod      .~ static True)
                        & (methodAllowed    .~ static True)
                        & (forbidden        .~ static False)
                        & (response         .~ (static $ Response H.status200 [] "<html><body>foo</body></html>"))


main = run 3000 $ application hello

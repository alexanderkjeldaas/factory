{- |
    JavaScript code for consuming the API.
-}
module Factory.JavaScript where

import qualified Factory.API as API
import Servant.JS
import Data.Text

{- |
    Generate some JavaScript that will use jQuery to talk to the API.
-}
javaScriptJQuery :: Text
javaScriptJQuery = jsForAPI API.documentedAPI jquery

javaScriptVanilla :: Text
javaScriptVanilla = jsForAPI API.documentedAPI jquery

javaScriptAngular :: Text
javaScriptAngular = jsForAPI API.documentedAPI (angular defAngularOptions)

javaScriptAngularService :: Text
javaScriptAngularService = jsForAPI API.documentedAPI (angularService defAngularOptions)


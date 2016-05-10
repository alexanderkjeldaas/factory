{- |
    Swagger information about the API.
-}
module Factory.Swagger where

import           Data.Aeson           as Aeson
import qualified Factory.API          as API

import           Servant.Swagger      as Swagger

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text


swaggerDoc :: Text.Text
swaggerDoc = encodeJsonText doc
  where
    doc = Swagger.toSwagger API.documentedAPI
    encodeJsonText = Text.decodeLatin1 . LBS.toStrict . Aeson.encode

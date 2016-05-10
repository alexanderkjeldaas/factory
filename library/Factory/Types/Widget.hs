{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
    The core data type for this example application.
-}
module Factory.Types.Widget where

import Data.Aeson as Aeson

import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import           Data.Swagger.Schema  as Swagger


{- |
    A widget. Who knows what it does.
-}
data Widget = Widget
    { name :: Text.Text -- ^ The name of this widget.
    } deriving (Aeson.FromJSON, Aeson.ToJSON, Eq,
                GHC.Generic, Read, Show, Swagger.ToSchema)

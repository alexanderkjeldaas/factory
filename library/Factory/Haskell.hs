{- |
    Haskell functions for consuming the API.
-}
module Factory.Haskell where

import Servant

import qualified Control.Monad.Trans.Except as Except
import qualified Factory.API as API
import qualified Factory.Types.Widget as Widget
import qualified Servant.Client as Servant
import qualified Network.HTTP.Client as HTTP

{- |
    Run an action. If it's successful, return the value. If it fails, 'error'
    with the message.
-}
run :: Action a -> IO a
run action = do
    result <- Except.runExceptT action
    case result of
        Left message -> error (show message)
        Right x -> return x

{- |
    A convenient type alias for API consumers. This looks the same as
    'Factory.Server.Action', but the 'Servant.ServantErr' comes from
    @Servant.Client@ instead of @Servant@.
-}
type Action a = Except.ExceptT Servant.ServantError IO a

{- |
    Get all of the widgets. See 'API.ListWidgets'.
-}
listWidgets :: HTTP.Manager -> Servant.BaseUrl -> Action [Widget.Widget]

{- |
    Create a new widget. See 'API.CreateWidget'.
-}
createWidget :: Widget.Widget -> HTTP.Manager -> Servant.BaseUrl -> Action Widget.Widget

{- |
    Try to get a particular widget. See 'API.ShowWidget'.
-}
showWidget :: Int -> HTTP.Manager -> Servant.BaseUrl -> Action Widget.Widget

{- |
    Update an existing widget. See 'API.UpdateWidget'.
-}
updateWidget :: Int -> Widget.Widget -> HTTP.Manager -> Servant.BaseUrl -> Action Widget.Widget

{- |
    Destroy an existing widget. See 'API.DestroyWidget'.
-}
destroyWidget :: Int -> HTTP.Manager -> Servant.BaseUrl -> Action Widget.Widget

(   listWidgets
    :<|> createWidget
    :<|> showWidget
    :<|> updateWidget
    :<|> destroyWidget
    ) = Servant.client API.documentedAPI

{- |
    The default server location.
-}
host :: Servant.BaseUrl
host = Servant.BaseUrl Servant.Http "localhost" 8080 ""

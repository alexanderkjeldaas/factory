{- |
    The server that implements the API.
-}
module Factory.Server where


import           Servant
import           Servant.Utils.StaticFiles  as StaticFiles

import qualified Control.Monad.IO.Class     as IO
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Text                  as Text
import qualified Factory.API                as API
import qualified Factory.Database           as DB
import qualified Factory.JavaScript         as JavaScript
import qualified Factory.Markdown           as Markdown
import qualified Factory.Swagger            as Swagger
import qualified Factory.Types.Widget       as Widget
import           Network.Wai                as Wai

{- |
    This is the actual implementation of the server.
-}
server :: Servant.Server API.API
server
    = getPresentation
    :<|> getCode
    :<|> getMarkdown
    :<|> getJQuery
    :<|> getAngular
    :<|> getAngularService
    :<|> getSwagger
    :<|> listWidgets
    :<|> createWidget
    :<|> showWidget
    :<|> updateWidget
    :<|> destroyWidget

{- |
    A convenient type alias for API endpoints. This looks the same as
    'Factory.Haskell.Handler', but the 'Servant.ServantErr' comes from
    @Servant@ instead of @Servant.Client@.
-}
type Handler a = Except.ExceptT Servant.ServantErr IO a

{- |
    Get the Markdown documentation. See 'Markdown.markdown'.
-}
getPresentation :: Wai.Application
getPresentation = StaticFiles.serveDirectory "/var/www"
getCode :: Wai.Application
getCode = StaticFiles.serveDirectory "/var/code"

{- |
    Get the Markdown documentation. See 'Markdown.markdown'.
-}
getMarkdown :: Handler Text.Text
getMarkdown = return (Text.pack Markdown.markdown)

{- |
    Get the JavaScript client code. See 'JavaScript.javaScript'.
-}
getJQuery :: Handler Text.Text
getJQuery = return JavaScript.javaScriptJQuery

getAngular :: Handler Text.Text
getAngular = return JavaScript.javaScriptAngular

getAngularService :: Handler Text.Text
getAngularService = return JavaScript.javaScriptAngularService

getSwagger :: Handler Text.Text
getSwagger = return Swagger.swaggerDoc

{- |
    Get all of the widgets. See 'API.ListWidgets'.
-}
listWidgets :: Handler [Widget.Widget]
listWidgets = do
    widgets <- IO.liftIO DB.getWidgets
    return widgets

{- |
    Create a new widget. See 'API.CreateWidget'.
-}
createWidget :: Widget.Widget -> Handler Widget.Widget
createWidget widget = do
    IO.liftIO (DB.createWidget widget)
    return widget

{- |
    Try to get a particular widget. See 'API.ShowWidget'.
-}
showWidget :: Int -> Handler Widget.Widget
showWidget number = withWidget number $ \ widget -> do
    return widget

{- |
    Update an existing widget. See 'API.UpdateWidget'.
-}
updateWidget :: Int -> Widget.Widget -> Handler Widget.Widget
updateWidget number newWidget = withWidget number $ \ _widget -> do
    IO.liftIO (DB.updateWidget number newWidget)
    return newWidget

{- |
    Destroy an existing widget. See 'API.DestroyWidget'.
-}
destroyWidget :: Int -> Handler Widget.Widget
destroyWidget number = withWidget number $ \ widget -> do
    IO.liftIO (DB.destroyWidget number)
    return widget

-- * Helper functions

{- |
    Try to get a widget. If it's found, use it. If it can't be found, return a
    404.
-}
withWidget :: Int -> (Widget.Widget -> Handler a) -> Handler a
withWidget number go = do
    maybeWidget <- IO.liftIO (DB.getWidget number)
    case maybeWidget of
        Nothing -> Except.throwE Servant.err404
        Just widget -> go widget

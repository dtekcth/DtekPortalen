{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Foundation
    ( App (..)
    , Route (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    -- own exports
 -- , AppMessage (..) -- We don't want i18n
    , module Settings.StaticFiles
    , setDtekTitle
    , CachedValues(..)
    , setSuccessMessage
    , setErrorMessage
    , routePrivileges
    , adminRoutes
    , routeDescription
    , documentFromDB
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Message (swedishMessage)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)

-- Imports we want to IGNORE in this sire (therfor commented)
-- import Yesod.Auth.BrowserId
-- import Yesod.Auth.GoogleEmail

-- Imports specific for this site (not scaffolded)
import Data.Monoid
import Data.IORef
import Scrapers.Einstein
import Scrapers.CalendarFeed
import qualified Data.Text as T
import Data.Text (Text)
import Text.Hamlet (shamlet)
import Yesod.Auth.Kerberos
import Yesod.Form.I18n.Swedish
import Data.Maybe (fromMaybe)
import Yesod.Markdown (Markdown)


data CachedValues = CachedValues {
    einstein :: IORef EinsteinScrapResult
  , calendar :: IORef CalendarScrapResult
}

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    -- Below is my own nonscaffolded entries
    , cache :: CachedValues
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype DtekRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Dtek = DtekRoute
-- * Creates the value resourcesDtek which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Dtek. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the DtekRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- $(widgetFile "normalize")
            $(widgetFile "default-layout")
            addStylesheet $ StaticR blueprint_screen_css
            addStylesheet $ StaticR blueprint_print_css
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    isAuthorized route _isWrite = do
        let mreqs = routePrivileges route
        case mreqs of
          Nothing -> return Authorized
          Just fs -> do
            mu <- maybeAuth
            case mu of
              Nothing               -> return AuthenticationRequired
              Just (entityVal -> u) -> do
                res <- liftIO $ checkMemberships fs u
                return $ case res of
                  Left errMsg -> Unauthorized $ "auth error: " `mappend` T.pack errMsg
                  Right True  -> Authorized
                  Right False -> Unauthorized $
                                   "Åtkomst nekad. Måste va medlem i nån av: "
                         `mappend` T.pack (show fs)


    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (entityKey -> uid) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    authPlugins = const
                    [ genericAuthKerberos
                        defaultKerberosConfig { usernameModifier =
                                                  (`mappend` ("/net" :: T.Text))}
                    ]

    authHttpManager = httpManager

    renderAuthMessage _ _ = swedishMessage

    loginHandler = defaultLayout $ do
        -- setDtekTitle "Inloggning" -- TODO: fix my generalize setDtekTitle
        toWidget [hamlet|
<p>Logga in med ditt Chalmers-ID och /net-lösenord, dvs samma lösenord som du använder för trådlöst nätverk.
   \ Alla chalmerister kan logga in. Du ska <b>inte</b> ha /net i slutet av username
<p>Tillbaka till #
    <a href=@{RootR}>startsidan
|]
        tm <- lift getRouteToMaster
        master <- lift getYesod
        mapM_ (flip apLogin tm) (authPlugins master)

setDtekTitle :: Html -> Widget
setDtekTitle = setTitle . (mappend "Dtekportalen - ")

-- The message types below assumes blueprint or similiar CSS framework
setSuccessMessage, setErrorMessage :: Html -> Handler ()
setSuccessMessage t = setMessage [shamlet|<div .success>#{t}|]
setErrorMessage   t = setMessage [shamlet|<div .error>#{t}|]

instance RenderMessage App FormMessage where
    renderMessage _ _ = swedishFormMessage

-- | The exposed function that adds Webredax automatically
--
-- Since this is the only function exposed, Webredax should always have
-- full privileges.
routePrivileges :: Route App -> Maybe [Forening]
routePrivileges route = fmap (Webredax:) $ routePrivileges' route

-- | Privilege control for the pages. Warning! by default pages are
--   unrestricted!
--
--   It is not neccesary to include Webredax in lists
routePrivileges' :: Route App -> Maybe [Forening]
routePrivileges' ManagePostsR = Just editors
routePrivileges' EditPostR {} = Just editors
routePrivileges' DelPostR {}  = Just editors
routePrivileges' (DocumentR (flip lookup documentPrivileges -> Just fs)) = Just fs
routePrivileges' _ = Nothing

-- | Administrative routes. These are only for visual significance
--   when displaying the admin page.
adminRoutes :: [Route App]
adminRoutes = [ManagePostsR] ++ map DocumentR specialDocTids

routeDescription :: Route App -> Text
routeDescription ManagePostsR = "Redigera nyheter"
routeDescription (DocumentR (flip lookup documentDescriptions -> Just x)) = x
routeDescription _ = "Beskrivning saknas"

editors :: [Forening]
editors = [Styret, DAG]

documentFromDB :: Text -> Handler Markdown
documentFromDB tid =
    let extract = fromMaybe "" . fmap (documentContent . entityVal)
    in  fmap extract $ runDB $ getBy $ UniqueDocument tid

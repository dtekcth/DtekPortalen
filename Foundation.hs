{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Foundation
    ( Dtek (..)
    , DtekRoute (..)
    , resourcesDtek
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    -- own exports
    , module Settings.StaticFiles
    , setDtekTitle
    , CachedValues(..)
    , setSuccessMessage
    , setErrorMessage
    , routePrivileges
    , adminRoutes
    , routeDescription
    , Form
    , documentFromDB
    ) where

import Prelude
import Yesod hiding (Form, AppConfig (..), withYamlEnvironment)
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText, logLazyText)
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Base
import Database.Persist.GenericSql
import Settings (widgetFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif
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
import Config
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
data Dtek = Dtek
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Base.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
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
mkYesodData "Dtek" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Dtek Dtek (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Dtek where
    approot = appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    isAuthorized route _isWrite = do
        let mreqs = routePrivileges route
        case mreqs of
          Nothing -> return Authorized
          Just ((Webredax:) -> fs) -> do
            mu <- maybeAuth
            case mu of
              Nothing      -> return AuthenticationRequired
              Just (_, u)  -> do
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

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist Dtek where
    type YesodPersistBackend Dtek = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Database.Persist.Base.runPool (undefined :: Settings.PersistConfig) f

instance YesodAuth Dtek where
    type AuthId Dtek = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    authPlugins = [ genericAuthKerberos
                      defaultKerberosConfig { usernameModifier =
                                                (`mappend` ("/net" :: T.Text))}
                  ]

    loginHandler = defaultLayout $ do
        setDtekTitle "Inloggning"
        addHamlet [hamlet|
<p>Logga in med ditt Chalmers-ID och /net-lösenord, dvs samma lösenord som du använder för trådlöst nätverk.
   \ Alla chalmerister kan logga in. Du ska <b>inte</b> ha /net i slutet av username
<p>Tillbaka till #
    <a href=@{RootR}>startsidan
|]
        tm <- lift getRouteToMaster
        mapM_ (flip apLogin tm) authPlugins

-- Sends off your mail. Requires sendmail in production!
deliver :: Dtek -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

setDtekTitle :: Monad m => Html -> GGWidget master m ()
setDtekTitle = setTitle . (mappend "Dtekportalen - ")

-- The message types below assumes blueprint or similiar CSS framework
setSuccessMessage, setErrorMessage :: Html -> Handler ()
setSuccessMessage t = setMessage [shamlet|<div .success>#{t}|]
setErrorMessage   t = setMessage [shamlet|<div .error>#{t}|]

instance RenderMessage Dtek FormMessage where
    renderMessage _ _ = swedishFormMessage

-- | Privilege control for the pages. Warning! by default pages are
--   unrestricted!
--
--   It is not neccesary to include Webredax in lists
routePrivileges :: DtekRoute -> Maybe [Forening]
routePrivileges ManagePostsR = Just editors
routePrivileges EditPostR {} = Just editors
routePrivileges DelPostR {}  = Just editors
routePrivileges (DocumentR (flip lookup documentPrivileges -> Just fs)) = Just fs
routePrivileges _ = Nothing

-- | Administrative routes. These are only for visual significance
--   when displaying the admin page.
adminRoutes :: [DtekRoute]
adminRoutes = [ManagePostsR]

routeDescription :: DtekRoute -> Text
routeDescription ManagePostsR = "Redigera nyheter"
routeDescription _ = "Beskrivning saknas"

editors :: [Forening]
editors = [Styret, DAG]

documentFromDB :: Text -> Handler Markdown
documentFromDB tid =
    let extract = fromMaybe "" . fmap (documentContent . snd)
    in  fmap extract $ runDB $ getBy $ UniqueDocument tid

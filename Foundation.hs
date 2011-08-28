{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
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
    , module Forening
    , module Settings.StaticFiles
    , setDtekTitle
    ) where

import Yesod
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.Kerberos
import Yesod.Logger (Logger, logLazyText)
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, luciusFile, juliusFile, widgetFile)
import Model
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import qualified Data.Text as T
import Web.ClientSession (getKey)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)

-- Arash imports
import Forening
import Data.Monoid(mappend)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Dtek = Dtek
    { settings :: Settings.AppConfig
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
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

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Dtek where
    approot = Settings.appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogMessage loc level msg >>= logLazyText (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticDir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])


-- How to run database actions.
instance YesodPersist Dtek where
    type YesodPersistBackend Dtek = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Settings.runConnectionPool f

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
                fmap Just $ insert $ User (credsIdent creds)

    authPlugins = [ genericAuthKerberos 
                      defaultKerberosConfig { usernameModifier = 
                                                (`mappend` ("/net" :: T.Text))}
                  ]

-- Sends off your mail. Requires sendmail in production!
deliver :: Dtek -> L.ByteString -> IO ()
#ifdef PRODUCTION
deliver _ = sendmail
#else
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#endif

setDtekTitle = setTitle . (mappend "Dtekportalen - ")

instance RenderMessage Dtek FormMessage where
    renderMessage _ _ = defaultFormMessage

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | In-built kerberos authentication for Yesod.
--
-- Please note that all configuration should have been done
-- manually on the machine prior to running the code.
--
-- On linux machines the configuration might be in /etc/krb5.conf.
-- It's worth checking if the Kerberos service provider (e.g. your university)
-- already provide a complete configuration file.
--
-- Be certain that you can manually login from a shell by typing
--
-- > kinit username
--
-- If you fill in your password and the program returns no error code,
-- then your kerberos configuration is setup properly.
-- Only then can this module be of any use.
module Yesod.Auth.Kerberos
    ( authKerberos,
      genericAuthKerberos,
      KerberosConfig(..),
      defaultKerberosConfig
    ) where

import Yesod.Auth
import Yesod.Auth.Message
import Web.Authenticate.Kerberos
import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet
import Yesod.Core
import Yesod.Form
import Control.Applicative ((<$>), (<*>))

data KerberosConfig = KerberosConfig {
    -- | When a user gives username x, f(x) will be passed to Kerberos
    usernameModifier :: Text -> Text
    -- | When a user gives username x, f(x) will be passed to Yesod
  , identifierModifier :: Text -> Text
  }

-- | A configuration where the username the user provides is the one passed
-- to both kerberos and yesod
defaultKerberosConfig :: KerberosConfig
defaultKerberosConfig = KerberosConfig id id

-- | A configurable version of 'authKerberos'
genericAuthKerberos :: YesodAuth m => KerberosConfig -> AuthPlugin m
genericAuthKerberos config = AuthPlugin "kerberos" dispatch $ \tm -> toWidget
    [hamlet|$newline never
    <div id="header">
        <h1>Login

    <div id="login">
        <form method="post" action="@{tm login}">
            <table>
                <tr>
                    <th>Username:
                    <td>
                        <input id="x" name="username" autofocus="" required>
                <tr>
                    <th>Password:
                    <td>
                        <input type="password" name="password" required>
                <tr>
                    <td>&nbsp;
                    <td>
                        <input type="submit" value="Login">

            <script>
                if (!("autofocus" in document.createElement("input"))) {
                    document.getElementById("x").focus();
                }
|]
  where
    dispatch "POST" ["login"] = postLoginR config >>= sendResponse
    dispatch _ _              = notFound

login :: AuthRoute
login = PluginR "kerberos" ["login"]

-- | Kerberos with 'defaultKerberosConfig'
authKerberos :: YesodAuth m => AuthPlugin m
authKerberos = genericAuthKerberos defaultKerberosConfig

-- | Handle the login form
postLoginR :: (YesodAuth master) => KerberosConfig -> HandlerT Auth (HandlerT master IO) ()
postLoginR config = do
    (mu,mp) <- lift $ runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    let errorMessage (message :: Text) = do
        lift $ setMessage [shamlet|$newline never
            Error: #{message}
          |]
        redirect LoginR

    case (mu,mp) of
        (Nothing, _      ) -> do
            mr <- lift getMessageRender
            errorMessage $ mr PleaseProvideUsername
        (_      , Nothing) -> do
            mr <- lift getMessageRender
            errorMessage $ mr PleaseProvidePassword
        (Just u , Just p ) -> do
          result <- liftIO $ loginKerberos (usernameModifier config u) p
          case result of
            Ok -> do
                let creds = Creds
                      { credsIdent  = identifierModifier config u
                      , credsPlugin = "Kerberos"
                      , credsExtra  = []
                      }
                lift $ setCreds True creds
            kerberosError -> errorMessage (T.pack $ show kerberosError)


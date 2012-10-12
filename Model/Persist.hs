{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Persist where

import Prelude
import Yesod
import Yesod.Form.Functions
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Time (UTCTime)
import Text.Markdown (Markdown (Markdown))
import Database.Persist ()
import Database.Persist.Store
import Yesod.Text.Markdown ()

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

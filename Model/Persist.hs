{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Persist where

import Prelude
import Yesod
import Data.Typeable (Typeable)
import Database.Persist.Quasi (upperCaseSettings)

-- Imports for data types in config/models
import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
      $(persistFileWith upperCaseSettings "config/models")

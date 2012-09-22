{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Persist where

import Prelude
import Data.Monoid
import Data.String
import Yesod
import Yesod.Form.Functions
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Text.Markdown (Markdown (Markdown))
import Database.Persist ()
import Database.Persist.Store

-- TODO: Create a convenience package for this
instance PersistField Markdown where
  toPersistValue (Markdown t)      = PersistText $ toStrict t
  fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
  fromPersistValue _               = Left "Not a PersistText value"
  sqlType _                        = SqlString
  isNullable _                     = False

-- TODO: Put in the real markdown library
instance Monoid Markdown where
  mempty = Markdown ""
  mappend (Markdown t1) (Markdown t2) = Markdown $ t1 `mappend` t2

-- TODO: Put in the real markdown library
instance IsString Markdown where
  fromString s = Markdown $ TL.pack s

-- TODO: Put in right place, preferably in external library
markdownField :: RenderMessage master FormMessage => Field sub master Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . fromStrict
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
     }

     where
        unMarkdown :: Markdown -> Text
        unMarkdown (Markdown lt) = toStrict lt

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

module Import
    ( module Prelude
    , module Foundation
    , standardLayout
    , (<>)
    , Text
    , module Yesod
    , module Data.Monoid
    , module Control.Applicative
    , module Data.Maybe
    ) where

import Prelude hiding (writeFile, readFile)
import Yesod
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import StandardLayout (standardLayout)
import Yesod.Content (RepHtml)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

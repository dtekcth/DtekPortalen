module Import
    ( module Prelude
    , module Foundation
    , standardLayout
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    , module Data.Maybe
    ) where

import Prelude hiding (writeFile, readFile)
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import StandardLayout (standardLayout)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

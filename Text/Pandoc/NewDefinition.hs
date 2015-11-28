{-# LANGUAGE GADTs, DeriveDataTypeable, DeriveGeneric, FlexibleContexts,
     FlexibleInstances, StandaloneDeriving #-}

{-
Copyright (C) 2006-2013 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.NewDefinition (

) where

import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson.Types as Aeson
import Control.Monad (guard)
import qualified Data.Map as M
import GHC.Generics (Generic, Rep (..))
import Data.String
import Data.Char (toLower)
import Data.Monoid
import Control.DeepSeq.Generics

data Inl = Inl
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Blk = Blk
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

newtype Attr = Attr (String, [String], [(String, String)])
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Node a b where
     Str      :: String -> Node () Inl
     Space    :: Node () Inl
     Para     :: [Inline] -> Node () Blk
     WithAttr :: Attr -> Node () a -> Node Attr a
    deriving (Typeable)

deriving instance (Show a, Show b) => Show (Node a b)

type Inline = Node Attr Inl
type Block = Node Attr Blk


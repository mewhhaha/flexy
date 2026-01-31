module Flexy.Layout.Types
  ( Layout(..)
  , LayoutNode(..)
  ) where

import Flexy.Style (Style)
import Flexy.Types (EdgeValues)

-- | Computed layout for a node.
data Layout = Layout
  { left :: Float
  , top :: Float
  , width :: Float
  , height :: Float
  , margin :: EdgeValues Float
  , padding :: EdgeValues Float
  , border :: EdgeValues Float
  } deriving (Eq, Show)

-- | Layout tree with computed bounds.
data LayoutNode = LayoutNode
  { layout :: Layout
  , nodeStyle :: Style
  , nodeKey :: Maybe String
  , children :: [LayoutNode]
  } deriving (Eq, Show)

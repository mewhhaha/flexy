module Flexy.Layout
  ( Layout(..)
  , LayoutNode(..)
  , computeLayout
  , layoutBounds
  , layoutChildren
  , layoutKey
  ) where

import Flexy.Config (LayoutConfig)
import Flexy.Layout.Types (Layout(..), LayoutNode(..))
import Flexy.Internal.Compute (computeLayoutInternal)
import Flexy.Node (Node)
import Flexy.Types (Size)

-- | Compute layout for a node tree.
computeLayout :: LayoutConfig -> Size -> Node -> LayoutNode
computeLayout = computeLayoutInternal

layoutBounds :: LayoutNode -> (Float, Float, Float, Float)
layoutBounds n =
  let l = layout n
  in (left l, top l, width l, height l)

layoutChildren :: LayoutNode -> [LayoutNode]
layoutChildren = children

layoutKey :: LayoutNode -> Maybe String
layoutKey = nodeKey

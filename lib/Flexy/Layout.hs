-- | Layout computation and inspection helpers.
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

-- | Compute layout for a node tree using the provided configuration and root size.
computeLayout :: LayoutConfig -> Size -> Node -> LayoutNode
computeLayout = computeLayoutInternal

-- | Extract the (left, top, width, height) bounds from a layout node.
layoutBounds :: LayoutNode -> (Float, Float, Float, Float)
layoutBounds n =
  let l = layout n
  in (left l, top l, width l, height l)

-- | Read the children of a layout node.
layoutChildren :: LayoutNode -> [LayoutNode]
layoutChildren = children

-- | Read the optional debug key for a layout node.
layoutKey :: LayoutNode -> Maybe String
layoutKey = nodeKey

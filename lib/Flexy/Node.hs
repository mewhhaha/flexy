-- | Node construction helpers for building layout trees.
module Flexy.Node
  ( Node(..)
  , node
  , withChildren
  , withMeasure
  , withBaseline
  , withKey
  ) where

import Flexy.Measure (MeasureFunc, BaselineFunc)
import Flexy.Style (Style)

-- | Layout tree node.
data Node = Node
  { style :: Style -- ^ Style for this node.
  , children :: [Node] -- ^ Child nodes (empty for leaves).
  , measure :: Maybe MeasureFunc -- ^ Optional measurement callback.
  , baseline :: Maybe BaselineFunc -- ^ Optional baseline callback.
  , nodeKey :: Maybe String -- ^ Optional debug key.
  }

-- | Construct a leaf node with the provided style.
node :: Style -> Node
node s = Node
  { style = s
  , children = []
  , measure = Nothing
  , baseline = Nothing
  , nodeKey = Nothing
  }

-- | Replace the children of a node.
withChildren :: [Node] -> Node -> Node
withChildren kids n = n { children = kids }

-- | Attach a measurement callback to a node.
withMeasure :: MeasureFunc -> Node -> Node
withMeasure f n = n { measure = Just f }

-- | Attach a baseline callback to a node.
withBaseline :: BaselineFunc -> Node -> Node
withBaseline f n = n { baseline = Just f }

-- | Attach a debug key to a node, used in tests and inspection.
withKey :: String -> Node -> Node
withKey k n = n { nodeKey = Just k }

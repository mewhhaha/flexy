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
  { style :: Style
  , children :: [Node]
  , measure :: Maybe MeasureFunc
  , baseline :: Maybe BaselineFunc
  , nodeKey :: Maybe String
  }

node :: Style -> Node
node s = Node
  { style = s
  , children = []
  , measure = Nothing
  , baseline = Nothing
  , nodeKey = Nothing
  }

withChildren :: [Node] -> Node -> Node
withChildren kids n = n { children = kids }

withMeasure :: MeasureFunc -> Node -> Node
withMeasure f n = n { measure = Just f }

withBaseline :: BaselineFunc -> Node -> Node
withBaseline f n = n { baseline = Just f }

withKey :: String -> Node -> Node
withKey k n = n { nodeKey = Just k }

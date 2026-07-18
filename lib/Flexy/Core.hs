module Flexy.Core
  ( Size(..)
  , Rect(..)
  , Constraints(..)
  , Edges(..)
  , allEdges
  , axisEdges
  , edges
  , Style(..)
  , Length(..)
  , Direction(..)
  , Wrap(..)
  , Justify(..)
  , Align(..)
  , width
  , height
  , minWidth
  , minHeight
  , maxWidth
  , maxHeight
  , direction
  , wrapping
  , justify
  , align
  , alignSelf
  , grow
  , shrink
  , basis
  , flex
  , gap
  , padding
  , margin
  , Measure
  , Node(..)
  , leaf
  , sized
  , measured
  , row
  , column
  , styled
  , Layout(..)
  ) where

import Control.Applicative ((<|>))

-- | A concrete size in layout units.
data Size = Size
  { sizeWidth :: !Float
  , sizeHeight :: !Float
  } deriving (Eq, Show)

-- | A rectangle in absolute coordinates.
data Rect = Rect
  { rectX :: !Float
  , rectY :: !Float
  , rectWidth :: !Float
  , rectHeight :: !Float
  } deriving (Eq, Show)

-- | Maximum dimensions offered to an intrinsic measurement.
-- 'Nothing' means that the dimension is unconstrained.
data Constraints = Constraints
  { availableWidth :: !(Maybe Float)
  , availableHeight :: !(Maybe Float)
  } deriving (Eq, Show)

-- | Physical edges ordered by their names, independent of layout direction.
data Edges = Edges
  { edgeTop :: !Float
  , edgeRight :: !Float
  , edgeBottom :: !Float
  , edgeLeft :: !Float
  } deriving (Eq, Show)

-- | Put the same value on every edge.
allEdges :: Float -> Edges
allEdges value = Edges value value value value

-- | Set horizontal and vertical edges respectively.
axisEdges :: Float -> Float -> Edges
axisEdges horizontal vertical = Edges vertical horizontal vertical horizontal

-- | Set top, right, bottom, and left edges.
edges :: Float -> Float -> Float -> Float -> Edges
edges = Edges

-- | A dimension resolved against the corresponding parent dimension.
-- Percentages are fractions, so @Percent 0.5@ means fifty percent.
data Length
  = Auto
  | Points !Float
  | Percent !Float
  deriving (Eq, Show)

-- | Main-axis orientation and progression.
data Direction
  = Row -- ^ Lay children left to right.
  | RowReverse -- ^ Lay children right to left.
  | Column -- ^ Lay children top to bottom.
  | ColumnReverse -- ^ Lay children bottom to top.
  deriving (Eq, Show)

-- | Whether children stay on one line or form additional lines.
data Wrap = NoWrap | Wrap
  deriving (Eq, Show)

-- | Distribution of unused space on the main axis.
data Justify
  = Start
  | Center
  | End
  | SpaceBetween
  | SpaceAround
  | SpaceEvenly
  deriving (Eq, Show)

-- | Placement on the cross axis.
data Align = AlignStart | AlignCenter | AlignEnd | Stretch
  deriving (Eq, Show)

-- | A partial style. The right-hand style wins when styles are combined.
-- 'mempty' is the default flex style.
data Style = Style
  { styleWidth :: !(Maybe Length)
  , styleHeight :: !(Maybe Length)
  , styleMinWidth :: !(Maybe Length)
  , styleMinHeight :: !(Maybe Length)
  , styleMaxWidth :: !(Maybe Length)
  , styleMaxHeight :: !(Maybe Length)
  , styleDirection :: !(Maybe Direction)
  , styleWrap :: !(Maybe Wrap)
  , styleJustify :: !(Maybe Justify)
  , styleAlign :: !(Maybe Align)
  , styleAlignSelf :: !(Maybe Align)
  , styleGrow :: !(Maybe Float)
  , styleShrink :: !(Maybe Float)
  , styleBasis :: !(Maybe Length)
  , styleGap :: !(Maybe Float)
  , stylePadding :: !(Maybe Edges)
  , styleMargin :: !(Maybe Edges)
  } deriving (Eq, Show)

instance Semigroup Style where
  earlier <> later =
    Style
      { styleWidth = styleWidth later <|> styleWidth earlier
      , styleHeight = styleHeight later <|> styleHeight earlier
      , styleMinWidth = styleMinWidth later <|> styleMinWidth earlier
      , styleMinHeight = styleMinHeight later <|> styleMinHeight earlier
      , styleMaxWidth = styleMaxWidth later <|> styleMaxWidth earlier
      , styleMaxHeight = styleMaxHeight later <|> styleMaxHeight earlier
      , styleDirection = styleDirection later <|> styleDirection earlier
      , styleWrap = styleWrap later <|> styleWrap earlier
      , styleJustify = styleJustify later <|> styleJustify earlier
      , styleAlign = styleAlign later <|> styleAlign earlier
      , styleAlignSelf = styleAlignSelf later <|> styleAlignSelf earlier
      , styleGrow = styleGrow later <|> styleGrow earlier
      , styleShrink = styleShrink later <|> styleShrink earlier
      , styleBasis = styleBasis later <|> styleBasis earlier
      , styleGap = styleGap later <|> styleGap earlier
      , stylePadding = stylePadding later <|> stylePadding earlier
      , styleMargin = styleMargin later <|> styleMargin earlier
      }

instance Monoid Style where
  mempty =
    Style
      { styleWidth = Nothing
      , styleHeight = Nothing
      , styleMinWidth = Nothing
      , styleMinHeight = Nothing
      , styleMaxWidth = Nothing
      , styleMaxHeight = Nothing
      , styleDirection = Nothing
      , styleWrap = Nothing
      , styleJustify = Nothing
      , styleAlign = Nothing
      , styleAlignSelf = Nothing
      , styleGrow = Nothing
      , styleShrink = Nothing
      , styleBasis = Nothing
      , styleGap = Nothing
      , stylePadding = Nothing
      , styleMargin = Nothing
      }

-- | Set the preferred outer width.
width :: Length -> Style
width value = mempty { styleWidth = Just value }

-- | Set the preferred outer height.
height :: Length -> Style
height value = mempty { styleHeight = Just value }

-- | Set the lower width bound.
minWidth :: Length -> Style
minWidth value = mempty { styleMinWidth = Just value }

-- | Set the lower height bound.
minHeight :: Length -> Style
minHeight value = mempty { styleMinHeight = Just value }

-- | Set the upper width bound.
maxWidth :: Length -> Style
maxWidth value = mempty { styleMaxWidth = Just value }

-- | Set the upper height bound.
maxHeight :: Length -> Style
maxHeight value = mempty { styleMaxHeight = Just value }

-- | Set the main axis and its physical direction.
direction :: Direction -> Style
direction value = mempty { styleDirection = Just value }

-- | Set line wrapping.
wrapping :: Wrap -> Style
wrapping value = mempty { styleWrap = Just value }

-- | Distribute unused main-axis space.
justify :: Justify -> Style
justify value = mempty { styleJustify = Just value }

-- | Align every child on the cross axis.
align :: Align -> Style
align value = mempty { styleAlign = Just value }

-- | Override cross-axis alignment for one node.
alignSelf :: Align -> Style
alignSelf value = mempty { styleAlignSelf = Just value }

-- | Set the share of positive free space assigned to a node.
grow :: Float -> Style
grow value = mempty { styleGrow = Just value }

-- | Set the share of overflow removed from a node, weighted by its basis.
shrink :: Float -> Style
shrink value = mempty { styleShrink = Just value }

-- | Set the size used before free space is distributed.
basis :: Length -> Style
basis value = mempty { styleBasis = Just value }

-- | Set grow, shrink, and basis together.
flex :: Float -> Float -> Length -> Style
flex growFactor shrinkFactor basisLength =
  grow growFactor <> shrink shrinkFactor <> basis basisLength

-- | Set the spacing between adjacent children and flex lines.
gap :: Float -> Style
gap value = mempty { styleGap = Just value }

-- | Set space inside a node's bounds.
padding :: Edges -> Style
padding value = mempty { stylePadding = Just value }

-- | Set space outside a node's bounds.
margin :: Edges -> Style
margin value = mempty { styleMargin = Just value }

-- | Pure intrinsic measurement under parent constraints.
type Measure = Constraints -> Size

-- | An immutable layout tree carrying an application-defined value at every node.
data Node a = Node
  { nodeStyle :: !Style
  , nodeMeasure :: !(Maybe Measure)
  , nodeValue :: a
  , nodeChildren :: ![Node a]
  } deriving (Functor, Foldable, Traversable)

-- | A zero-intrinsic-size leaf.
leaf :: a -> Node a
leaf value = Node mempty Nothing value []

-- | A leaf with a fixed intrinsic size. Style dimensions may override it.
sized :: Size -> a -> Node a
sized intrinsicSize = measured (const intrinsicSize)

-- | A leaf measured under the space offered by its parent.
measured :: Measure -> a -> Node a
measured measureContent value = Node mempty (Just measureContent) value []

-- | A row container carrying a value in its own layout node.
row :: a -> [Node a] -> Node a
row value children' = Node (direction Row) Nothing value children'

-- | A column container carrying a value in its own layout node.
column :: a -> [Node a] -> Node a
column value children' = Node (direction Column) Nothing value children'

-- | Overlay a style on a node. The overlay wins over properties already set.
styled :: Style -> Node a -> Node a
styled overlay node = node { nodeStyle = nodeStyle node <> overlay }

-- | A laid-out tree with the same shape and values as its input.
data Layout a = Layout
  { bounds :: !Rect
  , value :: a
  , children :: ![Layout a]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

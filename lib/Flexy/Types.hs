-- | Core enums and value types used by the layout engine.
module Flexy.Types
  ( Size(..)
  , Dimension(..)
  , Value(..)
  , Edge(..)
  , EdgeValues(..)
  , Align(..)
  , Justify(..)
  , FlexDirection(..)
  , FlexWrap(..)
  , PositionType(..)
  , BoxSizing(..)
  , Overflow(..)
  , WritingMode(..)
  , Direction(..)
  , edgeValues
  , mapEdgeValues
  ) where

-- | Size with possibly unresolved dimensions.
data Size = Size
  { width :: Dimension -- ^ Width dimension.
  , height :: Dimension -- ^ Height dimension.
  } deriving (Eq, Show)

-- | Dimension for sizing.
data Dimension
  = DimAuto -- ^ Automatic sizing.
  | DimUndefined -- ^ Unspecified size (treated like auto).
  | DimPoints Float -- ^ Absolute points/pixels.
  | DimPercent Float -- ^ Percentage of the owner size.
  | DimMinContent -- ^ Min-content intrinsic size.
  | DimMaxContent -- ^ Max-content intrinsic size.
  | DimFitContent (Maybe Float) -- ^ Fit-content with an optional clamp.
  | DimContent -- ^ Content size (alias of max-content for now).
  deriving (Eq, Show)

-- | Value for edges and positions.
data Value
  = ValAuto -- ^ Automatic value.
  | ValPoints Float -- ^ Absolute points/pixels.
  | ValPercent Float -- ^ Percentage of the owner size.
  deriving (Eq, Show)

-- | Logical edges.
data Edge
  = EdgeLeft
  | EdgeTop
  | EdgeRight
  | EdgeBottom
  | EdgeStart
  | EdgeEnd
  | EdgeHorizontal
  | EdgeVertical
  | EdgeAll
  deriving (Eq, Show)

-- | Edge values with CSS-like fallbacks.
data EdgeValues a = EdgeValues
  { leftE :: a -- ^ Physical left edge.
  , topE :: a -- ^ Physical top edge.
  , rightE :: a -- ^ Physical right edge.
  , bottomE :: a -- ^ Physical bottom edge.
  , startE :: a -- ^ Logical start edge.
  , endE :: a -- ^ Logical end edge.
  , horizontalE :: a -- ^ Horizontal shorthand.
  , verticalE :: a -- ^ Vertical shorthand.
  , allE :: a -- ^ All-edges shorthand.
  } deriving (Eq, Show)

-- | Fill all edges with the same value.
edgeValues :: a -> EdgeValues a
edgeValues v = EdgeValues v v v v v v v v v

-- | Map a function over each edge slot.
mapEdgeValues :: (a -> b) -> EdgeValues a -> EdgeValues b
mapEdgeValues f e =
  EdgeValues
    (f (leftE e))
    (f (topE e))
    (f (rightE e))
    (f (bottomE e))
    (f (startE e))
    (f (endE e))
    (f (horizontalE e))
    (f (verticalE e))
    (f (allE e))

-- | Alignment options.
data Align
  = AlignAuto -- ^ Use the container's align-items.
  | AlignFlexStart -- ^ Pack toward the start edge.
  | AlignCenter -- ^ Center along the axis.
  | AlignFlexEnd -- ^ Pack toward the end edge.
  | AlignStretch -- ^ Stretch to fill the available space.
  | AlignBaseline -- ^ Align baselines (first baseline).
  | AlignFirstBaseline -- ^ Align first baselines (alias of baseline).
  | AlignLastBaseline -- ^ Align last baselines.
  | AlignSpaceBetween -- ^ Distribute with equal space between lines/items.
  | AlignSpaceAround -- ^ Distribute with space around lines/items.
  | AlignSpaceEvenly -- ^ Distribute with equal space everywhere.
  deriving (Eq, Show)

-- | Justification options.
data Justify
  = JustifyFlexStart -- ^ Pack toward the start edge.
  | JustifyCenter -- ^ Center along the main axis.
  | JustifyFlexEnd -- ^ Pack toward the end edge.
  | JustifySpaceBetween -- ^ Distribute with equal space between items.
  | JustifySpaceAround -- ^ Distribute with space around items.
  | JustifySpaceEvenly -- ^ Distribute with equal space everywhere.
  deriving (Eq, Show)

-- | Main axis direction.
data FlexDirection
  = Row -- ^ Horizontal main axis, start -> end.
  | RowReverse -- ^ Horizontal main axis, end -> start.
  | Column -- ^ Vertical main axis, start -> end.
  | ColumnReverse -- ^ Vertical main axis, end -> start.
  deriving (Eq, Show)

-- | Wrapping behavior.
data FlexWrap
  = NoWrap -- ^ Single line.
  | Wrap -- ^ Wrap onto multiple lines.
  | WrapReverse -- ^ Wrap with reversed cross axis.
  deriving (Eq, Show)

-- | Positioning.
data PositionType
  = PositionRelative -- ^ Participate in normal flex layout.
  | PositionAbsolute -- ^ Positioned relative to the container padding box.
  deriving (Eq, Show)

-- | Box sizing model.
data BoxSizing
  = ContentBox -- ^ Dimensions apply to the content box.
  | BorderBox -- ^ Dimensions apply to the border box.
  deriving (Eq, Show)

-- | Overflow behavior (affects auto min sizing).
data Overflow
  = OverflowVisible -- ^ Auto min sizes are content-based.
  | OverflowHidden -- ^ Auto min sizes may shrink to padding/border.
  | OverflowScroll -- ^ Treated like hidden for min sizing.
  | OverflowClip -- ^ Treated like hidden for min sizing.
  deriving (Eq, Show)

-- | Writing mode used to resolve inline/block axes.
data WritingMode
  = HorizontalTB -- ^ Inline axis is horizontal.
  | VerticalRL -- ^ Inline axis is vertical, block progression right-to-left.
  | VerticalLR -- ^ Inline axis is vertical, block progression left-to-right.
  deriving (Eq, Show)

-- | Text/layout direction.
data Direction
  = LTR -- ^ Left-to-right.
  | RTL -- ^ Right-to-left.
  | Inherit -- ^ Inherit from the parent.
  deriving (Eq, Show)

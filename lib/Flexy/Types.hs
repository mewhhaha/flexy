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
  { width :: Dimension
  , height :: Dimension
  } deriving (Eq, Show)

-- | Dimension for sizing.
data Dimension
  = DimAuto
  | DimUndefined
  | DimPoints Float
  | DimPercent Float
  | DimMinContent
  | DimMaxContent
  | DimFitContent (Maybe Float)
  | DimContent
  deriving (Eq, Show)

-- | Value for edges and positions.
data Value
  = ValAuto
  | ValPoints Float
  | ValPercent Float
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
  { leftE :: a
  , topE :: a
  , rightE :: a
  , bottomE :: a
  , startE :: a
  , endE :: a
  , horizontalE :: a
  , verticalE :: a
  , allE :: a
  } deriving (Eq, Show)

edgeValues :: a -> EdgeValues a
edgeValues v = EdgeValues v v v v v v v v v

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
  = AlignAuto
  | AlignFlexStart
  | AlignCenter
  | AlignFlexEnd
  | AlignStretch
  | AlignBaseline
  | AlignFirstBaseline
  | AlignLastBaseline
  | AlignSpaceBetween
  | AlignSpaceAround
  | AlignSpaceEvenly
  deriving (Eq, Show)

-- | Justification options.
data Justify
  = JustifyFlexStart
  | JustifyCenter
  | JustifyFlexEnd
  | JustifySpaceBetween
  | JustifySpaceAround
  | JustifySpaceEvenly
  deriving (Eq, Show)

-- | Main axis direction.
data FlexDirection
  = Row
  | RowReverse
  | Column
  | ColumnReverse
  deriving (Eq, Show)

-- | Wrapping behavior.
data FlexWrap
  = NoWrap
  | Wrap
  | WrapReverse
  deriving (Eq, Show)

-- | Positioning.
data PositionType
  = PositionRelative
  | PositionAbsolute
  deriving (Eq, Show)

-- | Box sizing model.
data BoxSizing
  = ContentBox
  | BorderBox
  deriving (Eq, Show)

data Overflow
  = OverflowVisible
  | OverflowHidden
  | OverflowScroll
  | OverflowClip
  deriving (Eq, Show)

data WritingMode
  = HorizontalTB
  | VerticalRL
  | VerticalLR
  deriving (Eq, Show)

-- | Text/layout direction.
data Direction
  = LTR
  | RTL
  | Inherit
  deriving (Eq, Show)

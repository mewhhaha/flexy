-- | Style definitions and setters.
module Flexy.Style
  ( Style(..)
  , defaultStyle
  , setSize
  , setWidth
  , setHeight
  , setMinWidth
  , setMinHeight
  , setMaxWidth
  , setMaxHeight
  , setFlexDirection
  , setFlexWrap
  , setJustifyContent
  , setAlignItems
  , setAlignSelf
  , setAlignContent
  , setFlexGrow
  , setFlexShrink
  , setFlexBasis
  , setPositionType
  , setOrder
  , setAspectRatio
  , setBoxSizing
  , setOverflow
  , setDisplay
  , setWritingMode
  , setFlex
  , setFlexFlow
  , setGaps
  , setDirection
  , setPaddingAll
  , setPaddingLTRB
  , setMarginAll
  , setMarginLTRB
  , setMarginLR
  , setBorderAll
  , setBorderStartEnd
  , setPositionLTRB
  ) where

import Flexy.Types

-- | Style properties for a node.
data Style = Style
  { direction :: Direction -- ^ Text/layout direction (LTR/RTL/inherit).
  , writingMode :: WritingMode -- ^ Writing mode for axis resolution.
  , flexDirection :: FlexDirection -- ^ Main axis direction.
  , flexWrap :: FlexWrap -- ^ Wrapping behavior.
  , justifyContent :: Justify -- ^ Main-axis distribution.
  , alignItems :: Align -- ^ Cross-axis alignment of items.
  , alignSelf :: Align -- ^ Per-item alignment override.
  , alignContent :: Align -- ^ Cross-axis line alignment.
  , positionType :: PositionType -- ^ Relative vs absolute positioning.
  , order :: Int -- ^ Sorting order for flex items.
  , flexGrow :: Float -- ^ Flex grow factor.
  , flexShrink :: Float -- ^ Flex shrink factor.
  , flexBasis :: Dimension -- ^ Flex base size.
  , boxSizing :: BoxSizing -- ^ Content-box or border-box sizing.
  , overflow :: Overflow -- ^ Overflow behavior (affects auto min sizing).
  , display :: Display -- ^ Display participation in layout.
  , width :: Dimension -- ^ Width dimension.
  , height :: Dimension -- ^ Height dimension.
  , minWidth :: Dimension -- ^ Minimum width dimension.
  , minHeight :: Dimension -- ^ Minimum height dimension.
  , maxWidth :: Dimension -- ^ Maximum width dimension.
  , maxHeight :: Dimension -- ^ Maximum height dimension.
  , aspectRatio :: Maybe Float -- ^ Optional aspect ratio (width / height).
  , gapRow :: Float -- ^ Gap between flex lines (row).
  , gapColumn :: Float -- ^ Gap between flex items (column).
  , margin :: EdgeValues Value -- ^ Margin values (logical edges supported).
  , padding :: EdgeValues Value -- ^ Padding values (logical edges supported).
  , border :: EdgeValues Float -- ^ Border widths (logical edges supported).
  , position :: EdgeValues Value -- ^ Offsets for absolute positioning.
  } deriving (Eq, Show)

-- | Defaults aligned with CSS flexbox defaults where possible.
defaultStyle :: Style
defaultStyle =
  Style
    { direction = Inherit
    , writingMode = HorizontalTB
    , flexDirection = Row
    , flexWrap = NoWrap
    , justifyContent = JustifyFlexStart
    , alignItems = AlignStretch
    , alignSelf = AlignAuto
    , alignContent = AlignStretch
    , positionType = PositionRelative
    , order = 0
    , flexGrow = 0
    , flexShrink = 1
    , flexBasis = DimAuto
    , boxSizing = ContentBox
    , overflow = OverflowVisible
    , display = DisplayFlex
    , width = DimAuto
    , height = DimAuto
    , minWidth = DimUndefined
    , minHeight = DimUndefined
    , maxWidth = DimUndefined
    , maxHeight = DimUndefined
    , aspectRatio = Nothing
    , gapRow = 0
    , gapColumn = 0
    , margin = edgeValues (ValPoints 0)
    , padding = edgeValues (ValPoints 0)
    , border = edgeValues 0
    , position = edgeValues ValAuto
    }

-- | Set both width and height dimensions.
setSize :: Dimension -> Dimension -> Style -> Style
setSize w h s = s { width = w, height = h }

-- | Set the width dimension.
setWidth :: Dimension -> Style -> Style
setWidth w s = s { width = w }

-- | Set the height dimension.
setHeight :: Dimension -> Style -> Style
setHeight h s = s { height = h }

-- | Set the minimum width dimension.
setMinWidth :: Dimension -> Style -> Style
setMinWidth w s = s { minWidth = w }

-- | Set the minimum height dimension.
setMinHeight :: Dimension -> Style -> Style
setMinHeight h s = s { minHeight = h }

-- | Set the maximum width dimension.
setMaxWidth :: Dimension -> Style -> Style
setMaxWidth w s = s { maxWidth = w }

-- | Set the maximum height dimension.
setMaxHeight :: Dimension -> Style -> Style
setMaxHeight h s = s { maxHeight = h }

-- | Set the flex direction (main axis).
setFlexDirection :: FlexDirection -> Style -> Style
setFlexDirection d s = s { flexDirection = d }

-- | Set the flex wrapping behavior.
setFlexWrap :: FlexWrap -> Style -> Style
setFlexWrap w s = s { flexWrap = w }

-- | Set justification along the main axis.
setJustifyContent :: Justify -> Style -> Style
setJustifyContent j s = s { justifyContent = j }

-- | Set alignment of items along the cross axis.
setAlignItems :: Align -> Style -> Style
setAlignItems a s = s { alignItems = a }

-- | Set alignment for a single item (overrides align-items).
setAlignSelf :: Align -> Style -> Style
setAlignSelf a s = s { alignSelf = a }

-- | Set alignment for flex lines (multi-line containers).
setAlignContent :: Align -> Style -> Style
setAlignContent a s = s { alignContent = a }

-- | Set the flex grow factor.
setFlexGrow :: Float -> Style -> Style
setFlexGrow g s = s { flexGrow = g }

-- | Set the flex shrink factor.
setFlexShrink :: Float -> Style -> Style
setFlexShrink sh s = s { flexShrink = sh }

-- | Set the flex basis dimension.
setFlexBasis :: Dimension -> Style -> Style
setFlexBasis b s = s { flexBasis = b }

-- | Set the positioning model.
setPositionType :: PositionType -> Style -> Style
setPositionType p s = s { positionType = p }

-- | Set the order used for flex item sorting.
setOrder :: Int -> Style -> Style
setOrder o s = s { order = o }

-- | Set an explicit aspect ratio (width / height).
setAspectRatio :: Maybe Float -> Style -> Style
setAspectRatio r s = s { aspectRatio = r }

-- | Set the box sizing model.
setBoxSizing :: BoxSizing -> Style -> Style
setBoxSizing bs s = s { boxSizing = bs }

-- | Set overflow behavior (affects auto min sizing).
setOverflow :: Overflow -> Style -> Style
setOverflow ov s = s { overflow = ov }

-- | Set display participation in layout.
setDisplay :: Display -> Style -> Style
setDisplay d s = s { display = d }

-- | Set the writing mode for axis resolution.
setWritingMode :: WritingMode -> Style -> Style
setWritingMode wm s = s { writingMode = wm }

-- | Set flex grow, shrink, and basis in one call.
setFlex :: Float -> Float -> Dimension -> Style -> Style
setFlex grow shrink basis s = s { flexGrow = grow, flexShrink = shrink, flexBasis = basis }

-- | Set flex direction and wrapping in one call.
setFlexFlow :: FlexDirection -> FlexWrap -> Style -> Style
setFlexFlow dir wrap s = s { flexDirection = dir, flexWrap = wrap }

-- | Set row and column gaps.
setGaps :: Float -> Float -> Style -> Style
setGaps rowGap colGap s = s { gapRow = rowGap, gapColumn = colGap }

-- | Set the text/layout direction (LTR/RTL).
setDirection :: Direction -> Style -> Style
setDirection d s = s { direction = d }

-- | Set all padding edges to the same value.
setPaddingAll :: Value -> Style -> Style
setPaddingAll v s = s { padding = edgeValues v }

-- | Set padding for left, top, right, bottom edges.
setPaddingLTRB :: Value -> Value -> Value -> Value -> Style -> Style
setPaddingLTRB l t r b s =
  s { padding = (padding s) { leftE = l, topE = t, rightE = r, bottomE = b } }

-- | Set all margin edges to the same value.
setMarginAll :: Value -> Style -> Style
setMarginAll v s = s { margin = edgeValues v }

-- | Set margin for left, top, right, bottom edges.
setMarginLTRB :: Value -> Value -> Value -> Value -> Style -> Style
setMarginLTRB l t r b s =
  s { margin = (margin s) { leftE = l, topE = t, rightE = r, bottomE = b } }

-- | Set margin for left and right edges.
setMarginLR :: Value -> Value -> Style -> Style
setMarginLR l r s =
  s { margin = (margin s) { leftE = l, rightE = r } }

-- | Set all border edges to the same width.
setBorderAll :: Float -> Style -> Style
setBorderAll v s = s { border = edgeValues v }

-- | Set the logical start/end border widths.
setBorderStartEnd :: Float -> Float -> Style -> Style
setBorderStartEnd startVal endVal s =
  s { border = (border s) { startE = startVal, endE = endVal } }

-- | Set position offsets for left, top, right, bottom edges.
setPositionLTRB :: Value -> Value -> Value -> Value -> Style -> Style
setPositionLTRB l t r b s =
  s { position = (position s) { leftE = l, topE = t, rightE = r, bottomE = b } }

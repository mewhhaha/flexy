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
  { direction :: Direction
  , writingMode :: WritingMode
  , flexDirection :: FlexDirection
  , flexWrap :: FlexWrap
  , justifyContent :: Justify
  , alignItems :: Align
  , alignSelf :: Align
  , alignContent :: Align
  , positionType :: PositionType
  , order :: Int
  , flexGrow :: Float
  , flexShrink :: Float
  , flexBasis :: Dimension
  , boxSizing :: BoxSizing
  , overflow :: Overflow
  , width :: Dimension
  , height :: Dimension
  , minWidth :: Dimension
  , minHeight :: Dimension
  , maxWidth :: Dimension
  , maxHeight :: Dimension
  , aspectRatio :: Maybe Float
  , gapRow :: Float
  , gapColumn :: Float
  , margin :: EdgeValues Value
  , padding :: EdgeValues Value
  , border :: EdgeValues Float
  , position :: EdgeValues Value
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

setSize :: Dimension -> Dimension -> Style -> Style
setSize w h s = s { width = w, height = h }

setWidth :: Dimension -> Style -> Style
setWidth w s = s { width = w }

setHeight :: Dimension -> Style -> Style
setHeight h s = s { height = h }

setMinWidth :: Dimension -> Style -> Style
setMinWidth w s = s { minWidth = w }

setMinHeight :: Dimension -> Style -> Style
setMinHeight h s = s { minHeight = h }

setMaxWidth :: Dimension -> Style -> Style
setMaxWidth w s = s { maxWidth = w }

setMaxHeight :: Dimension -> Style -> Style
setMaxHeight h s = s { maxHeight = h }

setFlexDirection :: FlexDirection -> Style -> Style
setFlexDirection d s = s { flexDirection = d }

setFlexWrap :: FlexWrap -> Style -> Style
setFlexWrap w s = s { flexWrap = w }

setJustifyContent :: Justify -> Style -> Style
setJustifyContent j s = s { justifyContent = j }

setAlignItems :: Align -> Style -> Style
setAlignItems a s = s { alignItems = a }

setAlignSelf :: Align -> Style -> Style
setAlignSelf a s = s { alignSelf = a }

setAlignContent :: Align -> Style -> Style
setAlignContent a s = s { alignContent = a }

setFlexGrow :: Float -> Style -> Style
setFlexGrow g s = s { flexGrow = g }

setFlexShrink :: Float -> Style -> Style
setFlexShrink sh s = s { flexShrink = sh }

setFlexBasis :: Dimension -> Style -> Style
setFlexBasis b s = s { flexBasis = b }

setPositionType :: PositionType -> Style -> Style
setPositionType p s = s { positionType = p }

setOrder :: Int -> Style -> Style
setOrder o s = s { order = o }

setAspectRatio :: Maybe Float -> Style -> Style
setAspectRatio r s = s { aspectRatio = r }

setBoxSizing :: BoxSizing -> Style -> Style
setBoxSizing bs s = s { boxSizing = bs }

setOverflow :: Overflow -> Style -> Style
setOverflow ov s = s { overflow = ov }

setWritingMode :: WritingMode -> Style -> Style
setWritingMode wm s = s { writingMode = wm }

setFlex :: Float -> Float -> Dimension -> Style -> Style
setFlex grow shrink basis s = s { flexGrow = grow, flexShrink = shrink, flexBasis = basis }

setFlexFlow :: FlexDirection -> FlexWrap -> Style -> Style
setFlexFlow dir wrap s = s { flexDirection = dir, flexWrap = wrap }

setGaps :: Float -> Float -> Style -> Style
setGaps rowGap colGap s = s { gapRow = rowGap, gapColumn = colGap }

setDirection :: Direction -> Style -> Style
setDirection d s = s { direction = d }

setPaddingAll :: Value -> Style -> Style
setPaddingAll v s = s { padding = edgeValues v }

setPaddingLTRB :: Value -> Value -> Value -> Value -> Style -> Style
setPaddingLTRB l t r b s =
  s { padding = (padding s) { leftE = l, topE = t, rightE = r, bottomE = b } }

setMarginAll :: Value -> Style -> Style
setMarginAll v s = s { margin = edgeValues v }

setMarginLTRB :: Value -> Value -> Value -> Value -> Style -> Style
setMarginLTRB l t r b s =
  s { margin = (margin s) { leftE = l, topE = t, rightE = r, bottomE = b } }

setMarginLR :: Value -> Value -> Style -> Style
setMarginLR l r s =
  s { margin = (margin s) { leftE = l, rightE = r } }

setBorderAll :: Float -> Style -> Style
setBorderAll v s = s { border = edgeValues v }

setBorderStartEnd :: Float -> Float -> Style -> Style
setBorderStartEnd startVal endVal s =
  s { border = (border s) { startE = startVal, endE = endVal } }

setPositionLTRB :: Value -> Value -> Value -> Value -> Style -> Style
setPositionLTRB l t r b s =
  s { position = (position s) { leftE = l, topE = t, rightE = r, bottomE = b } }

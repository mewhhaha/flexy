module Flexy.Internal.Layout (layout) where

import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)

import Flexy.Core

data Axis = Horizontal | Vertical
  deriving (Eq)

data Prepared a = Prepared
  { preparedNode :: Node a
  , preparedBaseMain :: !Float
  , preparedCross :: !Float
  , preparedMinMain :: !Float
  , preparedMaxMain :: !(Maybe Float)
  , preparedMinCross :: !Float
  , preparedMaxCross :: !(Maybe Float)
  , preparedGrow :: !Float
  , preparedShrink :: !Float
  , preparedMargin :: !Edges
  , preparedCrossIsAuto :: !Bool
  , preparedAlign :: !Align
  }

data Allocation = Allocation
  { allocationBase :: !Float
  , allocationSize :: !Float
  , allocationMinimum :: !Float
  , allocationMaximum :: !(Maybe Float)
  , allocationFactor :: !Float
  , allocationFrozen :: !Bool
  }

-- | Lay out the root at @(0, 0)@ and make it fill the supplied viewport.
layout :: Size -> Node a -> Layout a
layout viewport = layoutNode (Rect 0 0 viewportWidth viewportHeight)
  where
    viewportWidth = nonNegative (sizeWidth viewport)
    viewportHeight = nonNegative (sizeHeight viewport)

layoutNode :: Rect -> Node a -> Layout a
layoutNode rectangle node =
  Layout
    { bounds = rectangle
    , value = nodeValue node
    , children = layoutChildren rectangle node
    }

layoutChildren :: Rect -> Node a -> [Layout a]
layoutChildren rectangle node
  | null (nodeChildren node) = []
  | otherwise = concat positionedLines
  where
    style = nodeStyle node
    direction' = fromMaybe Row (styleDirection style)
    axis = mainAxis direction'
    padding' = cleanBoxEdges (fromMaybe (allEdges 0) (stylePadding style))
    contentWidth = nonNegative (rectWidth rectangle - horizontalEdges padding')
    contentHeight = nonNegative (rectHeight rectangle - verticalEdges padding')
    contentSize = Size contentWidth contentHeight
    constraints = Constraints (Just contentWidth) (Just contentHeight)
    gap' = nonNegative (fromMaybe 0 (styleGap style))
    prepared = map (prepareChild axis constraints style) (nodeChildren node)
    lines' = splitLines (fromMaybe NoWrap (styleWrap style)) direction' (axisSize axis contentSize) gap' prepared
    lineSizes = crossSizes axis contentSize lines'
    (_, positionedLines) = mapAccumL (layoutLine rectangle contentSize direction' gap' style) 0 (zip lines' lineSizes)

prepareChild :: Axis -> Constraints -> Style -> Node a -> Prepared a
prepareChild axis constraints parentStyle child =
  Prepared
    { preparedNode = child
    , preparedBaseMain = clampLength mainMinimum mainMaximum basisSize
    , preparedCross = clampLength crossMinimum crossMaximum preferredCross
    , preparedMinMain = mainMinimum
    , preparedMaxMain = mainMaximum
    , preparedMinCross = crossMinimum
    , preparedMaxCross = crossMaximum
    , preparedGrow = nonNegative (fromMaybe 0 (styleGrow childStyle))
    , preparedShrink = nonNegative (fromMaybe 1 (styleShrink childStyle))
    , preparedMargin = cleanSignedEdges (fromMaybe (allEdges 0) (styleMargin childStyle))
    , preparedCrossIsAuto = lengthIsAuto crossLength
    , preparedAlign = fromMaybe parentAlignment (styleAlignSelf childStyle)
    }
  where
    childStyle = nodeStyle child
    preferred = preferredSize constraints child
    preferredMain = axisSize axis preferred
    preferredCross = crossAxisSize axis preferred
    mainLimit = constraintFor axis constraints
    crossLimit = crossConstraintFor axis constraints
    crossLength = crossDimensionFor axis childStyle
    basisSize = fromMaybe preferredMain (styleBasis childStyle >>= resolveLength mainLimit)
    mainMinimum = resolvedMinimum mainLimit (minimumFor axis childStyle)
    mainMaximum = maximumFor axis childStyle >>= resolveLength mainLimit
    crossMinimum = resolvedMinimum crossLimit (crossMinimumFor axis childStyle)
    crossMaximum = crossMaximumFor axis childStyle >>= resolveLength crossLimit
    parentAlignment = fromMaybe Stretch (styleAlign parentStyle)

preferredSize :: Constraints -> Node a -> Size
preferredSize constraints node = Size resolvedWidth resolvedHeight
  where
    style = nodeStyle node
    padding' = cleanBoxEdges (fromMaybe (allEdges 0) (stylePadding style))
    innerConstraints =
      Constraints
        (subtractEdgeSpace (availableWidth constraints) (horizontalEdges padding'))
        (subtractEdgeSpace (availableHeight constraints) (verticalEdges padding'))
    intrinsic = intrinsicSize innerConstraints node
    intrinsicWidth = nonNegative (sizeWidth intrinsic) + horizontalEdges padding'
    intrinsicHeight = nonNegative (sizeHeight intrinsic) + verticalEdges padding'
    preferredWidth = fromMaybe intrinsicWidth (styleWidth style >>= resolveLength (availableWidth constraints))
    preferredHeight = fromMaybe intrinsicHeight (styleHeight style >>= resolveLength (availableHeight constraints))
    minimumWidth = resolvedMinimum (availableWidth constraints) (styleMinWidth style)
    minimumHeight = resolvedMinimum (availableHeight constraints) (styleMinHeight style)
    maximumWidth = styleMaxWidth style >>= resolveLength (availableWidth constraints)
    maximumHeight = styleMaxHeight style >>= resolveLength (availableHeight constraints)
    resolvedWidth = clampLength minimumWidth maximumWidth preferredWidth
    resolvedHeight = clampLength minimumHeight maximumHeight preferredHeight

intrinsicSize :: Constraints -> Node a -> Size
intrinsicSize constraints node =
  case nodeMeasure node of
    Just measureContent -> cleanSize (measureContent constraints)
    Nothing -> intrinsicChildren constraints node

intrinsicChildren :: Constraints -> Node a -> Size
intrinsicChildren constraints node =
  case childSizes of
    [] -> Size 0 0
    _
      | mainAxis direction' == Horizontal -> Size mainTotal crossMaximum
      | otherwise -> Size crossMaximum mainTotal
  where
    style = nodeStyle node
    direction' = fromMaybe Row (styleDirection style)
    axis = mainAxis direction'
    gap' = nonNegative (fromMaybe 0 (styleGap style))
    childSizes = map measureChild (nodeChildren node)
    measureChild child =
      let childSize = preferredSize constraints child
          childMargin = cleanSignedEdges (fromMaybe (allEdges 0) (styleMargin (nodeStyle child)))
      in (axisSize axis childSize + mainMargins direction' childMargin, crossAxisSize axis childSize + crossMargins axis childMargin)
    mainTotal = sum (map fst childSizes) + gapsTotal gap' (length childSizes)
    crossMaximum = foldl' (\largest (_, childCross) -> max largest childCross) 0 childSizes

splitLines :: Wrap -> Direction -> Float -> Float -> [Prepared a] -> [[Prepared a]]
splitLines NoWrap _ _ _ children' = [children' | not (null children')]
splitLines Wrap direction' availableMain gap' children' = reverse (finish folded)
  where
    folded = foldl' addChild ([], [], 0) children'

    addChild (completed, current, used) child
      | null current = (completed, [child], outerMain child)
      | used + gap' + outerMain child <= availableMain =
          (completed, child : current, used + gap' + outerMain child)
      | otherwise = (reverse current : completed, [child], outerMain child)

    finish (completed, [], _) = completed
    finish (completed, current, _) = reverse current : completed

    outerMain child = preparedBaseMain child + mainMargins direction' (preparedMargin child)

crossSizes :: Axis -> Size -> [[Prepared a]] -> [Float]
crossSizes _ _ [] = []
crossSizes axis contentSize [singleLine] = [crossAxisSize axis contentSize | not (null singleLine)]
crossSizes axis _ lines' = map naturalLineCross lines'
  where
    naturalLineCross = foldl' (\largest child -> max largest (preparedCross child + crossMargins axis (preparedMargin child))) 0

layoutLine
  :: Rect
  -> Size
  -> Direction
  -> Float
  -> Style
  -> Float
  -> ([Prepared a], Float)
  -> (Float, [Layout a])
layoutLine parentRectangle contentSize direction' baseGap parentStyle crossOffset (line, lineCross) =
  (crossOffset + lineCross + baseGap, layouts)
  where
    axis = mainAxis direction'
    availableMain = axisSize axis contentSize
    assignedMain = distributeMain direction' availableMain baseGap line
    occupiedMain =
      sum assignedMain
        + sum (map (mainMargins direction' . preparedMargin) line)
        + gapsTotal baseGap (length line)
    remainingMain = nonNegative (availableMain - occupiedMain)
    (leading, between) = justifySpacing (fromMaybe Start (styleJustify parentStyle)) baseGap remainingMain (length line)
    (_, layouts) = mapAccumL positionChild leading (zip line assignedMain)

    positionChild cursor (child, childMain) =
      (nextCursor, layoutNode childRectangle (preparedNode child))
      where
        margin' = preparedMargin child
        beforeMain = mainBefore direction' margin'
        afterMain = mainAfter direction' margin'
        logicalMain = cursor + beforeMain
        physicalMain
          | isReverse direction' = availableMain - logicalMain - childMain
          | otherwise = logicalMain
        crossBefore = crossBeforeMargin axis margin'
        crossAfter = crossAfterMargin axis margin'
        availableCross = nonNegative (lineCross - crossBefore - crossAfter)
        childCross
          | preparedAlign child == Stretch && preparedCrossIsAuto child =
              clampLength (preparedMinCross child) (preparedMaxCross child) availableCross
          | otherwise = preparedCross child
        crossFree = nonNegative (availableCross - childCross)
        childCrossOffset = crossOffset + crossBefore + alignOffset (preparedAlign child) crossFree
        contentOriginX = rectX parentRectangle + edgeLeft parentPadding
        contentOriginY = rectY parentRectangle + edgeTop parentPadding
        childRectangle
          | axis == Horizontal = Rect (contentOriginX + physicalMain) (contentOriginY + childCrossOffset) childMain childCross
          | otherwise = Rect (contentOriginX + childCrossOffset) (contentOriginY + physicalMain) childCross childMain
        nextCursor = logicalMain + childMain + afterMain + between

    parentPadding = cleanBoxEdges (fromMaybe (allEdges 0) (stylePadding parentStyle))

distributeMain :: Direction -> Float -> Float -> [Prepared a] -> [Float]
distributeMain direction' availableMain gap' children' = map allocationSize (settle allocations)
  where
    target = nonNegative
      (availableMain
        - gapsTotal gap' (length children')
        - sum (map (mainMargins direction' . preparedMargin) children'))
    initialFree = target - sum (map preparedBaseMain children')
    growing = initialFree > 0
    allocations = map makeAllocation children'
    makeAllocation child =
      Allocation
        { allocationBase = preparedBaseMain child
        , allocationSize = preparedBaseMain child
        , allocationMinimum = preparedMinMain child
        , allocationMaximum = preparedMaxMain child
        , allocationFactor =
            if growing
              then preparedGrow child
              else preparedShrink child * preparedBaseMain child
        , allocationFrozen = False
        }

    settle current
      | abs freeSpace < 0.0001 = current
      | totalFactor <= 0 = current
      | newlyFrozen current proposed = settle proposed
      | otherwise = proposed
      where
        freeSpace = target - sum (map occupiedSize current)
        occupiedSize allocation
          | allocationFrozen allocation = allocationSize allocation
          | otherwise = allocationBase allocation
        totalFactor = sum [allocationFactor allocation | allocation <- current, not (allocationFrozen allocation)]
        proposed = map resize current
        resize allocation
          | allocationFrozen allocation = allocation
          | otherwise =
              allocation
                { allocationSize = clamped
                , allocationFrozen = abs (clamped - candidate) > 0.0001
                }
          where
            candidate = allocationBase allocation + freeSpace * allocationFactor allocation / totalFactor
            clamped = clampLength (allocationMinimum allocation) (allocationMaximum allocation) candidate
        newlyFrozen before after = or (zipWith becameFrozen before after)
        becameFrozen old new = not (allocationFrozen old) && allocationFrozen new

justifySpacing :: Justify -> Float -> Float -> Int -> (Float, Float)
justifySpacing justification baseGap freeSpace count =
  case justification of
    Start -> (0, baseGap)
    Center -> (freeSpace / 2, baseGap)
    End -> (freeSpace, baseGap)
    SpaceBetween
      | count > 1 -> (0, baseGap + freeSpace / fromIntegral (count - 1))
      | otherwise -> (0, baseGap)
    SpaceAround
      | count > 0 ->
          let distributed = freeSpace / fromIntegral count
          in (distributed / 2, baseGap + distributed)
      | otherwise -> (0, baseGap)
    SpaceEvenly
      | count > 0 ->
          let distributed = freeSpace / fromIntegral (count + 1)
          in (distributed, baseGap + distributed)
      | otherwise -> (0, baseGap)

alignOffset :: Align -> Float -> Float
alignOffset alignment freeSpace =
  case alignment of
    AlignStart -> 0
    AlignCenter -> freeSpace / 2
    AlignEnd -> freeSpace
    Stretch -> 0

mainAxis :: Direction -> Axis
mainAxis Row = Horizontal
mainAxis RowReverse = Horizontal
mainAxis Column = Vertical
mainAxis ColumnReverse = Vertical

isReverse :: Direction -> Bool
isReverse RowReverse = True
isReverse ColumnReverse = True
isReverse _ = False

axisSize :: Axis -> Size -> Float
axisSize Horizontal = sizeWidth
axisSize Vertical = sizeHeight

crossAxisSize :: Axis -> Size -> Float
crossAxisSize Horizontal = sizeHeight
crossAxisSize Vertical = sizeWidth

constraintFor :: Axis -> Constraints -> Maybe Float
constraintFor Horizontal = availableWidth
constraintFor Vertical = availableHeight

crossConstraintFor :: Axis -> Constraints -> Maybe Float
crossConstraintFor Horizontal = availableHeight
crossConstraintFor Vertical = availableWidth

crossDimensionFor :: Axis -> Style -> Maybe Length
crossDimensionFor Horizontal = styleHeight
crossDimensionFor Vertical = styleWidth

minimumFor :: Axis -> Style -> Maybe Length
minimumFor Horizontal = styleMinWidth
minimumFor Vertical = styleMinHeight

maximumFor :: Axis -> Style -> Maybe Length
maximumFor Horizontal = styleMaxWidth
maximumFor Vertical = styleMaxHeight

crossMinimumFor :: Axis -> Style -> Maybe Length
crossMinimumFor Horizontal = styleMinHeight
crossMinimumFor Vertical = styleMinWidth

crossMaximumFor :: Axis -> Style -> Maybe Length
crossMaximumFor Horizontal = styleMaxHeight
crossMaximumFor Vertical = styleMaxWidth

lengthIsAuto :: Maybe Length -> Bool
lengthIsAuto Nothing = True
lengthIsAuto (Just Auto) = True
lengthIsAuto _ = False

resolveLength :: Maybe Float -> Length -> Maybe Float
resolveLength _ Auto = Nothing
resolveLength _ (Points value') = Just (nonNegative value')
resolveLength available (Percent fraction) = nonNegative . (* finiteOrZero fraction) <$> available

resolvedMinimum :: Maybe Float -> Maybe Length -> Float
resolvedMinimum available = maybe 0 (fromMaybe 0 . resolveLength available)

clampLength :: Float -> Maybe Float -> Float -> Float
clampLength minimumValue maximumValue value' =
  max minimumValue (maybe sanitized (`min` sanitized) maximumValue)
  where
    sanitized = nonNegative value'

mainMargins :: Direction -> Edges -> Float
mainMargins direction' margins = mainBefore direction' margins + mainAfter direction' margins

mainBefore :: Direction -> Edges -> Float
mainBefore Row = edgeLeft
mainBefore RowReverse = edgeRight
mainBefore Column = edgeTop
mainBefore ColumnReverse = edgeBottom

mainAfter :: Direction -> Edges -> Float
mainAfter Row = edgeRight
mainAfter RowReverse = edgeLeft
mainAfter Column = edgeBottom
mainAfter ColumnReverse = edgeTop

crossMargins :: Axis -> Edges -> Float
crossMargins axis margins = crossBeforeMargin axis margins + crossAfterMargin axis margins

crossBeforeMargin :: Axis -> Edges -> Float
crossBeforeMargin Horizontal = edgeTop
crossBeforeMargin Vertical = edgeLeft

crossAfterMargin :: Axis -> Edges -> Float
crossAfterMargin Horizontal = edgeBottom
crossAfterMargin Vertical = edgeRight

horizontalEdges :: Edges -> Float
horizontalEdges values = edgeLeft values + edgeRight values

verticalEdges :: Edges -> Float
verticalEdges values = edgeTop values + edgeBottom values

gapsTotal :: Float -> Int -> Float
gapsTotal gap' count = gap' * fromIntegral (max 0 (count - 1))

subtractEdgeSpace :: Maybe Float -> Float -> Maybe Float
subtractEdgeSpace available edgeSpace = nonNegative . subtract edgeSpace <$> available

cleanSize :: Size -> Size
cleanSize measuredSize = Size (nonNegative (sizeWidth measuredSize)) (nonNegative (sizeHeight measuredSize))

cleanBoxEdges :: Edges -> Edges
cleanBoxEdges values =
  Edges
    (nonNegative (edgeTop values))
    (nonNegative (edgeRight values))
    (nonNegative (edgeBottom values))
    (nonNegative (edgeLeft values))

cleanSignedEdges :: Edges -> Edges
cleanSignedEdges values =
  Edges
    (finiteOrZero (edgeTop values))
    (finiteOrZero (edgeRight values))
    (finiteOrZero (edgeBottom values))
    (finiteOrZero (edgeLeft values))

nonNegative :: Float -> Float
nonNegative = max 0 . finiteOrZero

finiteOrZero :: Float -> Float
finiteOrZero value'
  | isNaN value' || isInfinite value' = 0
  | otherwise = value'

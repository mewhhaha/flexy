module Flexy.Internal.Layout (layout) where

import Control.Applicative ((<|>))
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
  , allocationFactor :: !Double
  , allocationFrozen :: !Bool
  }

data ResolvedChild a = ResolvedChild
  { resolvedPrepared :: Prepared a
  , resolvedMain :: !Float
  , resolvedCross :: !Float
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
    { bounds = sanitizedRectangle
    , value = nodeValue node
    , children = layoutChildren sanitizedRectangle node
    }
  where
    sanitizedRectangle = cleanRect rectangle

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
    preparedLines = splitLines (fromMaybe NoWrap (styleWrap style)) direction' (axisSize axis contentSize) gap' prepared
    resolvedLines = map (resolveLine axis constraints direction' (axisSize axis contentSize) gap') preparedLines
    lineSizes = crossSizes axis contentSize resolvedLines
    (_, positionedLines) =
      mapAccumL
        (layoutLine rectangle contentSize direction' gap' style)
        0
        (zip resolvedLines lineSizes)

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
    specifiedWidth = styleWidth style >>= resolveLength (availableWidth constraints)
    specifiedHeight = styleHeight style >>= resolveLength (availableHeight constraints)
    innerConstraints =
      Constraints
        (subtractEdgeSpace (specifiedWidth <|> availableWidth constraints) (horizontalEdges padding'))
        (subtractEdgeSpace (specifiedHeight <|> availableHeight constraints) (verticalEdges padding'))
    intrinsic = intrinsicSize innerConstraints node
    intrinsicWidth = nonNegative (sizeWidth intrinsic) + horizontalEdges padding'
    intrinsicHeight = nonNegative (sizeHeight intrinsic) + verticalEdges padding'
    preferredWidth = fromMaybe intrinsicWidth specifiedWidth
    preferredHeight = fromMaybe intrinsicHeight specifiedHeight
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
  case preparedLines of
    [] -> Size 0 0
    _
      | axis == Horizontal -> Size intrinsicMain intrinsicCross
      | otherwise -> Size intrinsicCross intrinsicMain
  where
    style = nodeStyle node
    direction' = fromMaybe Row (styleDirection style)
    axis = mainAxis direction'
    gap' = nonNegative (fromMaybe 0 (styleGap style))
    availableMain = constraintFor axis constraints
    prepared = map (prepareChild axis constraints style) (nodeChildren node)
    preparedLines =
      case (fromMaybe NoWrap (styleWrap style), availableMain) of
        (Wrap, Just mainLimit) -> splitLines Wrap direction' mainLimit gap' prepared
        _ -> [prepared | not (null prepared)]
    resolvedLines =
      case availableMain of
        Just mainLimit -> map (resolveLine axis constraints direction' mainLimit gap') preparedLines
        Nothing -> map (map resolveAtBase) preparedLines
    resolveAtBase child =
      ResolvedChild
        { resolvedPrepared = child
        , resolvedMain = preparedBaseMain child
        , resolvedCross = resolveCrossSize axis constraints (preparedBaseMain child) child
        }
    intrinsicMain = foldl' (\largest line -> max largest (preparedLineMain line)) 0 preparedLines
    intrinsicCross = sum (map resolvedLineCross resolvedLines) + gapsTotal gap' (length resolvedLines)
    preparedLineMain line =
      sum (map preparedOuterMain line) + gapsTotal gap' (length line)
    preparedOuterMain child =
      preparedBaseMain child + mainMargins direction' (preparedMargin child)
    resolvedLineCross = foldl' (\largest child -> max largest (resolvedOuterCross child)) 0
    resolvedOuterCross child =
      resolvedCross child
        + crossMargins axis (preparedMargin (resolvedPrepared child))

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

resolveLine :: Axis -> Constraints -> Direction -> Float -> Float -> [Prepared a] -> [ResolvedChild a]
resolveLine axis constraints direction' availableMain gap' prepared =
  zipWith resolveChild prepared assignedMain
  where
    assignedMain = distributeMain direction' availableMain gap' prepared
    resolveChild child childMain =
      ResolvedChild
        { resolvedPrepared = child
        , resolvedMain = childMain
        , resolvedCross = resolveCrossSize axis constraints childMain child
        }

resolveCrossSize :: Axis -> Constraints -> Float -> Prepared a -> Float
resolveCrossSize axis constraints assignedMain child
  | not (preparedCrossIsAuto child) = preparedCross child
  | otherwise = clampLength (preparedMinCross child) (preparedMaxCross child) intrinsicCross
  where
    node = preparedNode child
    padding' = cleanBoxEdges (fromMaybe (allEdges 0) (stylePadding (nodeStyle node)))
    measurementConstraints =
      case axis of
        Horizontal ->
          Constraints
            (Just (nonNegative (assignedMain - horizontalEdges padding')))
            (subtractEdgeSpace (availableHeight constraints) (verticalEdges padding'))
        Vertical ->
          Constraints
            (subtractEdgeSpace (availableWidth constraints) (horizontalEdges padding'))
            (Just (nonNegative (assignedMain - verticalEdges padding')))
    intrinsicCross =
      crossAxisSize axis (intrinsicSize measurementConstraints node)
        + crossEdges axis padding'

crossSizes :: Axis -> Size -> [[ResolvedChild a]] -> [Float]
crossSizes _ _ [] = []
crossSizes axis contentSize [singleLine] = [crossAxisSize axis contentSize | not (null singleLine)]
crossSizes axis _ lines' = map naturalLineCross lines'
  where
    naturalLineCross = foldl' (\largest child -> max largest (outerCross child)) 0
    outerCross child =
      resolvedCross child
        + crossMargins axis (preparedMargin (resolvedPrepared child))

layoutLine
  :: Rect
  -> Size
  -> Direction
  -> Float
  -> Style
  -> Float
  -> ([ResolvedChild a], Float)
  -> (Float, [Layout a])
layoutLine parentRectangle contentSize direction' baseGap parentStyle crossOffset (line, lineCross) =
  (crossOffset + lineCross + baseGap, layouts)
  where
    axis = mainAxis direction'
    availableMain = axisSize axis contentSize
    occupiedMain =
      sum (map resolvedMain line)
        + sum (map (mainMargins direction' . preparedMargin . resolvedPrepared) line)
        + gapsTotal baseGap (length line)
    freeMain = availableMain - occupiedMain
    (leading, between) = justifySpacing (fromMaybe Start (styleJustify parentStyle)) baseGap freeMain (length line)
    (_, layouts) = mapAccumL positionChild leading line

    positionChild cursor resolvedChild =
      (nextCursor, layoutNode childRectangle (preparedNode child))
      where
        child = resolvedPrepared resolvedChild
        childMain = resolvedMain resolvedChild
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
          | otherwise = resolvedCross resolvedChild
        crossFree = availableCross - childCross
        childCrossOffset = crossOffset + crossBefore + alignOffset (preparedAlign child) crossFree
        contentOriginX = rectX parentRectangle + edgeLeft parentPadding
        contentOriginY = rectY parentRectangle + edgeTop parentPadding
        childRectangle
          | axis == Horizontal =
              Rect
                (contentOriginX + physicalMain)
                (contentOriginY + childCrossOffset)
                childMain
                childCross
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
              then realToFrac (preparedGrow child)
              else realToFrac (preparedShrink child) * realToFrac (preparedBaseMain child)
        , allocationFrozen = False
        }

    settle current
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
                , allocationFrozen = clamped /= candidate
                }
          where
            candidate =
              realToFrac
                ( realToFrac (allocationBase allocation)
                    + realToFrac freeSpace * allocationFactor allocation / totalFactor
                )
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
      | count > 1 -> (0, baseGap + distributableSpace / fromIntegral (count - 1))
      | otherwise -> (0, baseGap)
    SpaceAround
      | count > 0 ->
          let distributed = distributableSpace / fromIntegral count
          in (distributed / 2, baseGap + distributed)
      | otherwise -> (0, baseGap)
    SpaceEvenly
      | count > 0 ->
          let distributed = distributableSpace / fromIntegral (count + 1)
          in (distributed, baseGap + distributed)
      | otherwise -> (0, baseGap)
  where
    distributableSpace = nonNegative freeSpace

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

crossEdges :: Axis -> Edges -> Float
crossEdges Horizontal = verticalEdges
crossEdges Vertical = horizontalEdges

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

cleanRect :: Rect -> Rect
cleanRect rectangle =
  Rect
    (finiteOrZero (rectX rectangle))
    (finiteOrZero (rectY rectangle))
    (nonNegative (rectWidth rectangle))
    (nonNegative (rectHeight rectangle))

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

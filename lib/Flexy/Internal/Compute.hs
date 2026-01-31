module Flexy.Internal.Compute
  ( computeLayoutInternal
  ) where

import Control.Applicative ((<|>))
import Data.List (scanl', sortOn, zip4)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)

import Flexy.Config (LayoutConfig(..))
import Flexy.Layout.Types (Layout, LayoutNode)
import qualified Flexy.Layout.Types as LT
import Flexy.Measure
import Flexy.Node (Node)
import qualified Flexy.Node as N
import Flexy.Style (Style)
import qualified Flexy.Style as S
import Flexy.Types

-- Axis helpers

data Axis = AxisRow | AxisColumn deriving (Eq, Show)

data AxisInfo = AxisInfo
  { mainAxis :: Axis
  , crossAxis :: Axis
  , mainIsReverse :: Bool
  , crossIsReverse :: Bool
  } deriving (Eq, Show)

data Intrinsic = IntrinsicMin | IntrinsicMax deriving (Eq, Show)

intrinsicMode :: Dimension -> Maybe Intrinsic
intrinsicMode dim = case dim of
  DimMinContent -> Just IntrinsicMin
  DimMaxContent -> Just IntrinsicMax
  DimContent -> Just IntrinsicMax
  _ -> Nothing

fitContentSpec :: Dimension -> Maybe (Maybe Float)
fitContentSpec dim = case dim of
  DimFitContent limit -> Just limit
  _ -> Nothing

computeLayoutInternal :: LayoutConfig -> Size -> Node -> LayoutNode
computeLayoutInternal cfg rootSize =
  computeNodeLayout cfg rootSize Nothing LTR

computeNodeLayout :: LayoutConfig -> Size -> Maybe (Float, Float) -> Direction -> Node -> LayoutNode
computeNodeLayout cfg ownerSize allocated parentDir n =
  let s = N.style n
  in if S.display s == DisplayNone
      then
        let layout0 = LT.Layout
              { LT.left = 0
              , LT.top = 0
              , LT.width = 0
              , LT.height = 0
              , LT.margin = edgeValues 0
              , LT.padding = edgeValues 0
              , LT.border = edgeValues 0
              }
        in LT.LayoutNode
            { LT.layout = layout0
            , LT.nodeStyle = s
            , LT.nodeKey = N.nodeKey n
            , LT.children = []
            }
      else
        let dir = resolveDirection parentDir s
            ownerW = dimensionToMaybe (width ownerSize)
            ownerH = dimensionToMaybe (height ownerSize)
            widthDim = S.width s
            heightDim = S.height s
            widthIntrinsic = intrinsicMode widthDim
            heightIntrinsic = intrinsicMode heightDim
            sizeOwnerW = if isJust widthIntrinsic then Nothing else ownerW
            sizeOwnerH = if isJust heightIntrinsic then Nothing else ownerH
            allocatedW = fmap fst allocated
            allocatedH = fmap snd allocated
            paddingE = resolveEdgesValue (S.padding s) dir (S.writingMode s) ownerW ownerH
            borderE = resolveEdgesFloat (S.border s) dir (S.writingMode s)
            marginE = resolveEdgesValue (S.margin s) dir (S.writingMode s) ownerW ownerH
            padBorderW = edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
            padBorderH = edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
            widthDef = resolveDimensionWith widthDim ownerW
            heightDef = resolveDimensionWith heightDim ownerH
            borderWFromStyle = resolveBorderSize (S.boxSizing s) padBorderW widthDef
            borderHFromStyle = resolveBorderSize (S.boxSizing s) padBorderH heightDef
            resolvedBorderW = allocatedW <|> borderWFromStyle <|> sizeOwnerW
            resolvedBorderH = allocatedH <|> borderHFromStyle <|> sizeOwnerH
            resolvedContentW = fmap (\w -> max 0 (w - padBorderW)) resolvedBorderW
            resolvedContentH = fmap (\h -> max 0 (h - padBorderH)) resolvedBorderH
            borderFixedW = isJust allocatedW || (isNothing allocatedW && isNothing borderWFromStyle && isJust sizeOwnerW)
            borderFixedH = isJust allocatedH || (isNothing allocatedH && isNothing borderHFromStyle && isJust sizeOwnerH)
            innerW = resolvedContentW
            innerH = resolvedContentH
            (finalW, finalH, childLayouts) =
              if null (N.children n) && isJust (N.measure n)
                then layoutLeaf n resolvedBorderW resolvedBorderH innerW innerH ownerW ownerH padBorderW padBorderH borderFixedW borderFixedH
                else layoutContainer cfg n resolvedBorderW resolvedBorderH ownerW ownerH innerW innerH paddingE borderE dir borderFixedW borderFixedH
            layout0 = LT.Layout
              { LT.left = 0
              , LT.top = 0
              , LT.width = finalW
              , LT.height = finalH
              , LT.margin = marginE
              , LT.padding = paddingE
              , LT.border = borderE
              }
        in LT.LayoutNode
            { LT.layout = applyRounding cfg (applyRelativeOffset s dir ownerW ownerH layout0)
            , LT.nodeStyle = s
            , LT.nodeKey = N.nodeKey n
            , LT.children = childLayouts
            }

layoutLeaf :: Node -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Float -> Float -> Bool -> Bool -> (Float, Float, [LayoutNode])
layoutLeaf n resolvedBorderW resolvedBorderH resolvedContentW resolvedContentH ownerW ownerH padBorderW padBorderH borderFixedW borderFixedH =
  let s = N.style n
      fitW = fitContentLeaf AxisRow resolvedContentW resolvedContentH ownerW ownerH padBorderW s n
      fitH = fitContentLeaf AxisColumn resolvedContentW resolvedContentH ownerW ownerH padBorderH s n
      resolvedContentW' = fitW <|> resolvedContentW
      resolvedContentH' = fitH <|> resolvedContentH
      widthOverride = fmap intrinsicOverride (intrinsicMode (S.width s))
      heightOverride = fmap intrinsicOverride (intrinsicMode (S.height s))
      (mw, mh, _) = measureIfNeeded n resolvedContentW' resolvedContentH' ownerW ownerH widthOverride heightOverride
      (w0, h0) = applyAspectRatio (S.aspectRatio s) (resolvedContentW' <|> mw) (resolvedContentH' <|> mh)
      (minContentW0, maxContentW0) = resolveContentMinMax (S.boxSizing s) (S.minWidth s) (S.maxWidth s) ownerW padBorderW
      (minContentH0, maxContentH0) = resolveContentMinMax (S.boxSizing s) (S.minHeight s) (S.maxHeight s) ownerH padBorderH
      minContentW = intrinsicContentForDim S.minWidth AxisRow ownerW ownerH resolvedContentW resolvedContentH s n <|> minContentW0
      maxContentW = intrinsicContentForDim S.maxWidth AxisRow ownerW ownerH resolvedContentW resolvedContentH s n <|> maxContentW0
      minContentH = intrinsicContentForDim S.minHeight AxisColumn ownerW ownerH resolvedContentW resolvedContentH s n <|> minContentH0
      maxContentH = intrinsicContentForDim S.maxHeight AxisColumn ownerW ownerH resolvedContentW resolvedContentH s n <|> maxContentH0
      maxBoundW = if borderFixedW then fmap (\bw -> max 0 (bw - padBorderW)) resolvedBorderW else Nothing
      maxBoundH = if borderFixedH then fmap (\bh -> max 0 (bh - padBorderH)) resolvedBorderH else Nothing
      maxW = minMaybe maxContentW maxBoundW
      maxH = minMaybe maxContentH maxBoundH
      w1 = clampMaybe minContentW maxW w0
      h1 = clampMaybe minContentH maxH h0
      contentW = max 0 (fromMaybe 0 w1)
      contentH = max 0 (fromMaybe 0 h1)
      borderW = contentW + padBorderW
      borderH = contentH + padBorderH
      finalW = if borderFixedW then fromMaybe borderW resolvedBorderW else borderW
      finalH = if borderFixedH then fromMaybe borderH resolvedBorderH else borderH
  in (finalW, finalH, [])

layoutContainer :: LayoutConfig -> Node -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> EdgeValues Float -> EdgeValues Float -> Direction -> Bool -> Bool -> (Float, Float, [LayoutNode])
layoutContainer cfg n resolvedBorderW resolvedBorderH ownerW ownerH innerW innerH paddingE borderE dir borderFixedW borderFixedH =
  let s = N.style n
      axisInfo0 = axisFromStyle s dir
      axisInfo = if S.flexWrap s == WrapReverse
        then axisInfo0 { crossIsReverse = not (crossIsReverse axisInfo0) }
        else axisInfo0
      displayedChildren = filter (isDisplayed . N.style) (N.children n)
      indexedChildren = zip [0..] displayedChildren
      (flexKids, absKids) = partitionChildren indexedChildren
      gapMain = mainGap s axisInfo
      gapCross = crossGap s axisInfo
      innerMain = if mainAxis axisInfo == AxisRow then innerW else innerH
      innerCross = if mainAxis axisInfo == AxisRow then innerH else innerW
      flexItems = [buildFlexItem cfg axisInfo s dir innerMain innerCross idx kid | (idx, kid) <- flexKids]
      orderedItems = sortOn (\it -> (itemOrder it, itemIndex it)) flexItems
      padBorderMain = if mainAxis axisInfo == AxisRow
        then edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
        else edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
      padBorderCross = if mainAxis axisInfo == AxisRow
        then edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
        else edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
      mainDim = if mainAxis axisInfo == AxisRow then S.width s else S.height s
      crossDim = if mainAxis axisInfo == AxisRow then S.height s else S.width s
      intrinsicMain = intrinsicMode mainDim
      fitMainLimit = fitContentSpec mainDim
      fitCrossLimit = fitContentSpec crossDim
      intrinsicMainSize mode =
        let lineRef = case mode of
              IntrinsicMin -> Just 0
              IntrinsicMax -> Nothing
            linesI = buildLines orderedItems lineRef gapMain (S.flexWrap s)
            forceCrossI = if length linesI == 1 then innerCross else Nothing
            sizedI = map (sizeLine cfg s axisInfo dir innerMain innerCross gapMain forceCrossI (Just mode)) linesI
        in maxLineMainUsed sizedI
      availableMain =
        let base = if mainAxis axisInfo == AxisRow then ownerW else ownerH
            baseContent = fmap (\v -> if S.boxSizing s == BorderBox then max 0 (v - padBorderMain) else v) base
        in case fitMainLimit of
            Nothing -> baseContent
            Just limitOpt ->
              let limitContent = fmap (\l -> if S.boxSizing s == BorderBox then max 0 (l - padBorderMain) else l) limitOpt
              in case (baseContent, limitContent) of
                  (Nothing, Nothing) -> Nothing
                  (Just v, Nothing) -> Just v
                  (Nothing, Just l) -> Just l
                  (Just v, Just l) -> Just (min v l)
      (innerMainForLines, intrinsicMainForLines) =
        case fitMainLimit of
          Just _ ->
            let minMain = intrinsicMainSize IntrinsicMin
                maxMain = intrinsicMainSize IntrinsicMax
                target = case availableMain of
                  Nothing -> maxMain
                  Just a -> min maxMain (max minMain a)
            in (Just target, Nothing)
          Nothing -> (innerMain, intrinsicMain)
      lineMainRef = case intrinsicMainForLines of
        Just IntrinsicMin -> Just 0
        Just IntrinsicMax -> Nothing
        Nothing -> innerMainForLines
      lines0 = buildLines orderedItems lineMainRef gapMain (S.flexWrap s)
      forceLineCross = if length lines0 == 1 then innerCross else Nothing
      sizedLines = map (sizeLine cfg s axisInfo dir innerMainForLines innerCross gapMain forceLineCross intrinsicMainForLines) lines0
      (finalInnerCross, sizedLines2) = resolveFinalCross innerCross gapCross sizedLines
      finalInnerMain = case intrinsicMainForLines of
        Just _ -> maxLineMainUsed sizedLines2
        Nothing -> fromMaybe (maxLineMainUsed sizedLines2) innerMainForLines
      crossAvailable =
        let base = if mainAxis axisInfo == AxisRow then ownerH else ownerW
            baseContent = fmap (\v -> if S.boxSizing s == BorderBox then max 0 (v - padBorderCross) else v) base
        in case fitCrossLimit of
            Nothing -> baseContent
            Just limitOpt ->
              let limitContent = fmap (\l -> if S.boxSizing s == BorderBox then max 0 (l - padBorderCross) else l) limitOpt
              in case (baseContent, limitContent) of
                  (Nothing, Nothing) -> Nothing
                  (Just v, Nothing) -> Just v
                  (Nothing, Just l) -> Just l
                  (Just v, Just l) -> Just (min v l)
      minCross = maximum (0 : map lineCrossSize sizedLines2)
      maxCross = sum (map lineCrossSize sizedLines2) + gapCross * fromIntegral (max 0 (length sizedLines2 - 1))
      finalInnerCross' = case fitCrossLimit of
        Nothing -> finalInnerCross
        Just _ ->
          case crossAvailable of
            Nothing -> maxCross
            Just a -> min maxCross (max minCross a)
      (sizedLines3, linePositions) = positionLines axisInfo s finalInnerCross' gapCross sizedLines2
      positionedFlex = concat (zipWith (positionLineItems s axisInfo finalInnerMain finalInnerCross' gapMain) linePositions sizedLines3)
      ownerSize = Size (maybe DimUndefined DimPoints innerW) (maybe DimUndefined DimPoints innerH)
      flexLayouts = reorderFlexLayouts (map (buildFlexLayout cfg ownerSize dir paddingE borderE) positionedFlex)
      staticPositions = if null absKids
        then []
        else computeStaticPositions cfg axisInfo s dir (Just finalInnerMain) (Just finalInnerCross') gapMain gapCross (S.flexWrap s) indexedChildren
      absLayouts = map (layoutAbsChild cfg ownerSize paddingE borderE innerW innerH dir (lookupStaticPos staticPositions)) absKids
      orderedChildren = mergeChildren displayedChildren flexLayouts absLayouts
      contentW0 = if mainAxis axisInfo == AxisRow then finalInnerMain else finalInnerCross'
      contentH0 = if mainAxis axisInfo == AxisRow then finalInnerCross' else finalInnerMain
      padBorderW = edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
      padBorderH = edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
      (minContentW0, maxContentW0) = resolveContentMinMax (S.boxSizing s) (S.minWidth s) (S.maxWidth s) ownerW padBorderW
      (minContentH0, maxContentH0) = resolveContentMinMax (S.boxSizing s) (S.minHeight s) (S.maxHeight s) ownerH padBorderH
      minContentW = case intrinsicMode (S.minWidth s) of
        Just _ -> Just contentW0
        Nothing -> minContentW0
      maxContentW = case intrinsicMode (S.maxWidth s) of
        Just _ -> Just contentW0
        Nothing -> maxContentW0
      minContentH = case intrinsicMode (S.minHeight s) of
        Just _ -> Just contentH0
        Nothing -> minContentH0
      maxContentH = case intrinsicMode (S.maxHeight s) of
        Just _ -> Just contentH0
        Nothing -> maxContentH0
      maxBoundW = if borderFixedW then fmap (\bw -> max 0 (bw - padBorderW)) resolvedBorderW else Nothing
      maxBoundH = if borderFixedH then fmap (\bh -> max 0 (bh - padBorderH)) resolvedBorderH else Nothing
      maxW = minMaybe maxContentW maxBoundW
      maxH = minMaybe maxContentH maxBoundH
      contentW = max 0 (fromMaybe contentW0 (clampMaybe minContentW maxW (Just contentW0)))
      contentH = max 0 (fromMaybe contentH0 (clampMaybe minContentH maxH (Just contentH0)))
      borderW = contentW + padBorderW
      borderH = contentH + padBorderH
      finalW = if borderFixedW then fromMaybe borderW resolvedBorderW else borderW
      finalH = if borderFixedH then fromMaybe borderH resolvedBorderH else borderH
  in (finalW, finalH, orderedChildren)

isDisplayed :: Style -> Bool
isDisplayed s = S.display s /= DisplayNone

-- Axis
axisFromStyle :: Style -> Direction -> AxisInfo
axisFromStyle s dir =
  let fd = S.flexDirection s
      wm = S.writingMode s
      inlineAxis = case wm of
        HorizontalTB -> AxisRow
        VerticalRL -> AxisColumn
        VerticalLR -> AxisColumn
      blockAxis = if inlineAxis == AxisRow then AxisColumn else AxisRow
      inlineReverse = dir == RTL
      blockReverse = wm == VerticalRL
      (mainAxis', mainRev) = case fd of
        Row -> (inlineAxis, inlineReverse)
        RowReverse -> (inlineAxis, not inlineReverse)
        Column -> (blockAxis, blockReverse)
        ColumnReverse -> (blockAxis, not blockReverse)
      crossAxis' = if mainAxis' == AxisRow then AxisColumn else AxisRow
      crossRev =
        if crossAxis' == inlineAxis
          then inlineReverse
          else blockReverse
  in AxisInfo mainAxis' crossAxis' mainRev crossRev

resolveDirection :: Direction -> Style -> Direction
resolveDirection parent s = case S.direction s of
  Inherit -> parent
  other -> other

mainGap :: Style -> AxisInfo -> Float
mainGap s axisInfo = if mainAxis axisInfo == AxisRow then S.gapColumn s else S.gapRow s

crossGap :: Style -> AxisInfo -> Float
crossGap s axisInfo = if mainAxis axisInfo == AxisRow then S.gapRow s else S.gapColumn s

-- Edge helpers
edgeLeft :: EdgeValues a -> a
edgeLeft = leftE
edgeRight :: EdgeValues a -> a
edgeRight = rightE
edgeTop :: EdgeValues a -> a
edgeTop = topE
edgeBottom :: EdgeValues a -> a
edgeBottom = bottomE

resolveEdgesValue :: EdgeValues Value -> Direction -> WritingMode -> Maybe Float -> Maybe Float -> EdgeValues Float
resolveEdgesValue vals dir wm refW _refH =
  let leftV = resolveValue (pickEdgeValue vals dir wm EdgeLeft) refW
      rightV = resolveValue (pickEdgeValue vals dir wm EdgeRight) refW
      topV = resolveValue (pickEdgeValue vals dir wm EdgeTop) refW
      bottomV = resolveValue (pickEdgeValue vals dir wm EdgeBottom) refW
  in EdgeValues leftV topV rightV bottomV 0 0 0 0 0

resolveEdgesFloat :: EdgeValues Float -> Direction -> WritingMode -> EdgeValues Float
resolveEdgesFloat vals dir wm =
  EdgeValues (pickEdgeFloat vals dir wm EdgeLeft)
             (pickEdgeFloat vals dir wm EdgeTop)
             (pickEdgeFloat vals dir wm EdgeRight)
             (pickEdgeFloat vals dir wm EdgeBottom)
             0 0 0 0 0

pickEdgeValue :: EdgeValues Value -> Direction -> WritingMode -> Edge -> Value
pickEdgeValue vals dir wm edge =
  let inlineAxis = case wm of
        HorizontalTB -> AxisRow
        VerticalRL -> AxisColumn
        VerticalLR -> AxisColumn
      inlineReverse = dir == RTL
      startV = if inlineReverse then endE vals else startE vals
      endV = if inlineReverse then startE vals else endE vals
  in case edge of
      EdgeLeft ->
        if inlineAxis == AxisRow
          then firstNonAuto [leftE vals, startV, horizontalE vals, allE vals]
          else firstNonAuto [leftE vals, horizontalE vals, allE vals]
      EdgeRight ->
        if inlineAxis == AxisRow
          then firstNonAuto [rightE vals, endV, horizontalE vals, allE vals]
          else firstNonAuto [rightE vals, horizontalE vals, allE vals]
      EdgeTop ->
        if inlineAxis == AxisColumn
          then firstNonAuto [topE vals, startV, verticalE vals, allE vals]
          else firstNonAuto [topE vals, verticalE vals, allE vals]
      EdgeBottom ->
        if inlineAxis == AxisColumn
          then firstNonAuto [bottomE vals, endV, verticalE vals, allE vals]
          else firstNonAuto [bottomE vals, verticalE vals, allE vals]
      _ -> ValAuto

pickEdgeFloat :: EdgeValues Float -> Direction -> WritingMode -> Edge -> Float
pickEdgeFloat vals dir wm edge =
  let inlineAxis = case wm of
        HorizontalTB -> AxisRow
        VerticalRL -> AxisColumn
        VerticalLR -> AxisColumn
      inlineReverse = dir == RTL
      startV = if inlineReverse then endE vals else startE vals
      endV = if inlineReverse then startE vals else endE vals
      horiz = horizontalE vals
      vert = verticalE vals
      allV = allE vals
      leftV = leftE vals
      rightV = rightE vals
      topV = topE vals
      bottomV = bottomE vals
      leftShorthand = leftV == horiz && leftV == allV
      rightShorthand = rightV == horiz && rightV == allV
      topShorthand = topV == vert && topV == allV
      bottomShorthand = bottomV == vert && bottomV == allV
  in case edge of
      EdgeLeft ->
        if inlineAxis == AxisRow
          then if leftShorthand && startV /= leftV
            then startV
            else firstNonZero [leftV, startV, horiz, allV]
          else firstNonZero [leftV, horiz, allV]
      EdgeRight ->
        if inlineAxis == AxisRow
          then if rightShorthand && endV /= rightV
            then endV
            else firstNonZero [rightV, endV, horiz, allV]
          else firstNonZero [rightV, horiz, allV]
      EdgeTop ->
        if inlineAxis == AxisColumn
          then if topShorthand && startV /= topV
            then startV
            else firstNonZero [topV, startV, vert, allV]
          else firstNonZero [topV, vert, allV]
      EdgeBottom ->
        if inlineAxis == AxisColumn
          then if bottomShorthand && endV /= bottomV
            then endV
            else firstNonZero [bottomV, endV, vert, allV]
          else firstNonZero [bottomV, vert, allV]
      _ -> 0

firstNonAuto :: [Value] -> Value
firstNonAuto [] = ValAuto
firstNonAuto (v:vs) = case v of
  ValAuto -> firstNonAuto vs
  _ -> v

firstNonZero :: [Float] -> Float
firstNonZero [] = 0
firstNonZero (v:vs) = if v /= 0 then v else firstNonZero vs

resolveValue :: Value -> Maybe Float -> Float
resolveValue v ref = case v of
  ValPoints x -> x
  ValPercent p -> maybe 0 (\r -> r * p / 100) ref
  ValAuto -> 0

resolveValueMaybe :: Value -> Maybe Float -> Maybe Float
resolveValueMaybe v ref = case v of
  ValPoints x -> Just x
  ValPercent p -> fmap (\r -> r * p / 100) ref
  ValAuto -> Nothing

-- Dimension helpers
resolveDimensionWith :: Dimension -> Maybe Float -> Maybe Float
resolveDimensionWith dim ref = case dim of
  DimPoints x -> Just x
  DimPercent p -> fmap (\r -> r * p / 100) ref
  DimMinContent -> Nothing
  DimMaxContent -> Nothing
  DimFitContent _ -> Nothing
  DimContent -> Nothing
  DimAuto -> Nothing
  DimUndefined -> Nothing

resolveBorderSize :: BoxSizing -> Float -> Maybe Float -> Maybe Float
resolveBorderSize boxSizing padBorder =
  fmap (\v -> if boxSizing == BorderBox then v else v + padBorder)

isDefiniteDimension :: Dimension -> Maybe Float -> Bool
isDefiniteDimension dim ref = case dim of
  DimPoints _ -> True
  DimPercent _ -> isJust ref
  DimMinContent -> True
  DimMaxContent -> True
  DimFitContent _ -> True
  DimContent -> True
  _ -> False

isAutoMin :: Dimension -> Bool
isAutoMin dim = case dim of
  DimAuto -> True
  DimUndefined -> True
  _ -> False

dimensionToMaybe :: Dimension -> Maybe Float
dimensionToMaybe dim = case dim of
  DimPoints x -> Just x
  _ -> Nothing

applyAspectRatio :: Maybe Float -> Maybe Float -> Maybe Float -> (Maybe Float, Maybe Float)
applyAspectRatio ratio mw mh =
  case ratio of
    Nothing -> (mw, mh)
    Just r -> case (mw, mh) of
      (Just w, Nothing) -> (Just w, Just (w / r))
      (Nothing, Just h) -> (Just (h * r), Just h)
      _ -> (mw, mh)

resolveContentMinMax :: BoxSizing -> Dimension -> Dimension -> Maybe Float -> Float -> (Maybe Float, Maybe Float)
resolveContentMinMax boxSizing minD maxD ref padBorder =
  let minV = resolveDimensionWith minD ref
      maxV = resolveDimensionWith maxD ref
      toContent v = if boxSizing == BorderBox then max 0 (v - padBorder) else v
      minContent = fmap toContent minV
      maxContent = fmap toContent maxV
  in (minContent, maxContent)

clampMaybe :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float
clampMaybe minV maxV val =
  let v1 = case minV of
        Just m -> fmap (max m) val
        Nothing -> val
      v2 = case maxV of
        Just m -> fmap (min m) v1
        Nothing -> v1
  in v2

minMaybe :: Maybe Float -> Maybe Float -> Maybe Float
minMaybe a b = case (a, b) of
  (Nothing, other) -> other
  (other, Nothing) -> other
  (Just x, Just y) -> Just (min x y)

data MeasureOverride = MeasureOverride MeasureMode Float

intrinsicOverride :: Intrinsic -> MeasureOverride
intrinsicOverride intrinsic = case intrinsic of
  IntrinsicMin -> MeasureOverride MeasureAtMost 0
  IntrinsicMax -> MeasureOverride MeasureUndefined 0

measureIfNeeded :: Node -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe MeasureOverride -> Maybe MeasureOverride -> (Maybe Float, Maybe Float, Maybe Float)
measureIfNeeded n resolvedContentW resolvedContentH ownerW ownerH overrideW overrideH =
  case N.measure n of
    Nothing -> (Nothing, Nothing, Nothing)
    Just f ->
      let (availW, widthMode) = pickMeasure resolvedContentW ownerW overrideW
          (availH, heightMode) = pickMeasure resolvedContentH ownerH overrideH
          out = f (MeasureInput availW widthMode availH heightMode)
      in (Just (measuredWidth out), Just (measuredHeight out), measuredBaseline out)
  where
    pickMeasure resolved owner override =
      case override of
        Just (MeasureOverride mode avail) -> (avail, mode)
        Nothing ->
          case resolved of
            Just v -> (v, MeasureExactly)
            Nothing -> case owner of
              Just o -> (o, MeasureAtMost)
              Nothing -> (0, MeasureUndefined)

fitContentLeaf :: Axis -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Float -> Style -> Node -> Maybe Float
fitContentLeaf axis resolvedContentW resolvedContentH ownerW ownerH padBorder s n =
  case fitContentSpec (if axis == AxisRow then S.width s else S.height s) of
    Nothing -> Nothing
    Just limitOpt ->
      let overrideMin = if axis == AxisRow then Just (intrinsicOverride IntrinsicMin) else Nothing
          overrideMax = if axis == AxisRow then Just (intrinsicOverride IntrinsicMax) else Nothing
          overrideMinH = if axis == AxisColumn then Just (intrinsicOverride IntrinsicMin) else Nothing
          overrideMaxH = if axis == AxisColumn then Just (intrinsicOverride IntrinsicMax) else Nothing
          (minW, minH, _) = measureIfNeeded n resolvedContentW resolvedContentH ownerW ownerH overrideMin overrideMinH
          (maxW, maxH, _) = measureIfNeeded n resolvedContentW resolvedContentH ownerW ownerH overrideMax overrideMaxH
          minContent = fromMaybe 0 (if axis == AxisRow then minW else minH)
          maxContent = fromMaybe minContent (if axis == AxisRow then maxW else maxH)
          availBase = if axis == AxisRow then ownerW else ownerH
          avail0 = case (availBase, limitOpt) of
            (Nothing, Nothing) -> Nothing
            (Just v, Nothing) -> Just v
            (Nothing, Just l) -> Just l
            (Just v, Just l) -> Just (min v l)
          availContent = fmap (\v -> if S.boxSizing s == BorderBox then max 0 (v - padBorder) else v) avail0
          target = case availContent of
            Nothing -> maxContent
            Just a -> min maxContent (max minContent a)
      in Just target

intrinsicContentForDim :: (Style -> Dimension) -> Axis -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Style -> Node -> Maybe Float
intrinsicContentForDim dimGetter axis ownerW ownerH resolvedContentW resolvedContentH s n =
  case intrinsicMode (dimGetter s) of
    Nothing -> Nothing
    Just mode ->
      let overrideW = if axis == AxisRow then Just (intrinsicOverride mode) else Nothing
          overrideH = if axis == AxisColumn then Just (intrinsicOverride mode) else Nothing
          (mw, mh, _) = measureIfNeeded n resolvedContentW resolvedContentH ownerW ownerH overrideW overrideH
      in case axis of
        AxisRow -> mw
        AxisColumn -> mh

intrinsicMainContent :: LayoutConfig -> AxisInfo -> Direction -> Style -> Maybe Float -> Maybe Float -> Node -> Intrinsic -> Float
intrinsicMainContent cfg axisInfo parentDir _cs _mainRef crossRef kid mode =
  case N.measure kid of
    Just f ->
      let MeasureOverride modeMain availMain = intrinsicOverride mode
          (availW, modeW, availH, modeH) =
            if mainAxis axisInfo == AxisRow
              then (availMain, modeMain, fromMaybe 0 crossRef, if isJust crossRef then MeasureAtMost else MeasureUndefined)
              else (fromMaybe 0 crossRef, if isJust crossRef then MeasureAtMost else MeasureUndefined, availMain, modeMain)
          out = f (MeasureInput availW modeW availH modeH)
      in if mainAxis axisInfo == AxisRow then measuredWidth out else measuredHeight out
    Nothing ->
      if null (N.children kid)
        then 0
        else measureContentMainIntrinsic cfg axisInfo parentDir Nothing crossRef kid mode

measureContentMainIntrinsic :: LayoutConfig -> AxisInfo -> Direction -> Maybe Float -> Maybe Float -> Node -> Intrinsic -> Float
measureContentMainIntrinsic cfg axisInfo parentDir _mainRef crossRef kid mode =
  let mainRef' = case mode of
        IntrinsicMin -> Just 0
        IntrinsicMax -> Nothing
      ownerSize =
        if mainAxis axisInfo == AxisRow
          then Size (maybe DimUndefined DimPoints mainRef') (maybe DimUndefined DimPoints crossRef)
          else Size (maybe DimUndefined DimPoints crossRef) (maybe DimUndefined DimPoints mainRef')
      measured = computeNodeLayout cfg ownerSize Nothing parentDir kid
      l = LT.layout measured
  in if mainAxis axisInfo == AxisRow then LT.width l else LT.height l

-- Flex items

data FlexItem = FlexItem
  { itemNode :: Node
  , itemIndex :: Int
  , itemOrder :: Int
  , itemMainBase :: Float
  , itemMainMinContent :: Float
  , itemFlexGrow :: Float
  , itemFlexShrink :: Float
  , itemMarginMainStart :: Float
  , itemMarginMainEnd :: Float
  , itemMarginCrossStart :: Float
  , itemMarginCrossEnd :: Float
  , itemMarginMainStartAuto :: Bool
  , itemMarginMainEndAuto :: Bool
  , itemMarginCrossStartAuto :: Bool
  , itemMarginCrossEndAuto :: Bool
  , itemAlignSelf :: Align
  , itemMinMain :: Maybe Float
  , itemMaxMain :: Maybe Float
  }

buildFlexItem :: LayoutConfig -> AxisInfo -> Style -> Direction -> Maybe Float -> Maybe Float -> Int -> Node -> FlexItem
buildFlexItem cfg axisInfo parentStyle dir innerMain innerCross idx kid =
  let cs = N.style kid
      mainRef = innerMain
      crossRef = innerCross
      ownerW = if mainAxis axisInfo == AxisRow then innerMain else innerCross
      ownerH = if mainAxis axisInfo == AxisRow then innerCross else innerMain
      paddingE = resolveEdgesValue (S.padding cs) dir (S.writingMode cs) ownerW ownerH
      borderE = resolveEdgesFloat (S.border cs) dir (S.writingMode cs)
      padBorderMain =
        if mainAxis axisInfo == AxisRow
          then edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
          else edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
      baseMain = resolveBaseMain cfg axisInfo dir cs mainRef crossRef padBorderMain kid
      mainDim = if mainAxis axisInfo == AxisRow then S.width cs else S.height cs
      basisDim = S.flexBasis cs
      explicitContent =
        case resolveDimensionWith mainDim mainRef <|> resolveDimensionWith basisDim mainRef of
          Just v -> if S.boxSizing cs == BorderBox then max 0 (v - padBorderMain) else v
          Nothing -> intrinsicMainContent cfg axisInfo dir cs mainRef crossRef kid IntrinsicMin
      minContentMain = explicitContent + padBorderMain
      minDim = if mainAxis axisInfo == AxisRow then S.minWidth cs else S.minHeight cs
      mStart = resolveMarginFor axisInfo dir (S.writingMode cs) ownerW kid True
      mEnd = resolveMarginFor axisInfo dir (S.writingMode cs) ownerW kid False
      cStart = resolveCrossMarginFor axisInfo dir (S.writingMode cs) ownerW kid True
      cEnd = resolveCrossMarginFor axisInfo dir (S.writingMode cs) ownerW kid False
      mStartAuto = isAutoMargin (S.margin cs) dir (mainMarginEdge axisInfo True)
      mEndAuto = isAutoMargin (S.margin cs) dir (mainMarginEdge axisInfo False)
      cStartAuto = isAutoMargin (S.margin cs) dir (crossMarginEdge axisInfo True)
      cEndAuto = isAutoMargin (S.margin cs) dir (crossMarginEdge axisInfo False)
      minMainResolved = resolveBorderSize (S.boxSizing cs) padBorderMain (resolveDimensionWith minDim mainRef)
      basisAuto = not (isDefiniteDimension (S.flexBasis cs) mainRef)
      mainAuto = not (isDefiniteDimension mainDim mainRef)
      overflowVisible = S.overflow cs == OverflowVisible
      minMainDefault =
        if isAutoMin minDim && basisAuto && mainAuto
          then Just (if overflowVisible then minContentMain else padBorderMain)
          else Nothing
      minMain = minMainResolved <|> minMainDefault
      maxMain = resolveBorderSize (S.boxSizing cs) padBorderMain (resolveDimensionWith (if mainAxis axisInfo == AxisRow then S.maxWidth cs else S.maxHeight cs) mainRef)
      align = if S.alignSelf cs == AlignAuto then S.alignItems parentStyle else S.alignSelf cs
  in FlexItem
      { itemNode = kid
      , itemIndex = idx
      , itemOrder = S.order cs
      , itemMainBase = baseMain
      , itemMainMinContent = minContentMain
      , itemFlexGrow = S.flexGrow cs
      , itemFlexShrink = S.flexShrink cs
      , itemMarginMainStart = mStart
      , itemMarginMainEnd = mEnd
      , itemMarginCrossStart = cStart
      , itemMarginCrossEnd = cEnd
      , itemMarginMainStartAuto = mStartAuto
      , itemMarginMainEndAuto = mEndAuto
      , itemMarginCrossStartAuto = cStartAuto
      , itemMarginCrossEndAuto = cEndAuto
      , itemAlignSelf = align
      , itemMinMain = minMain
      , itemMaxMain = maxMain
      }

resolveBaseMain :: LayoutConfig -> AxisInfo -> Direction -> Style -> Maybe Float -> Maybe Float -> Float -> Node -> Float
resolveBaseMain cfg axisInfo dir cs mainRef crossRef padBorderMain kid =
  let basisDim = S.flexBasis cs
      basis = resolveDimensionWith basisDim mainRef
      mainSize = if mainAxis axisInfo == AxisRow
        then resolveDimensionWith (S.width cs) mainRef
        else resolveDimensionWith (S.height cs) mainRef
      basisIntrinsic = intrinsicMainSize basisDim
      mainIntrinsic = intrinsicMainSize (if mainAxis axisInfo == AxisRow then S.width cs else S.height cs)
      base0 = basis <|> basisIntrinsic <|> mainSize <|> mainIntrinsic
      baseFromSpecified = case base0 of
        Just v -> Just (if S.boxSizing cs == BorderBox then v else v + padBorderMain)
        Nothing -> Nothing
      measuredContent = if isJust base0 then Nothing else
        case N.measure kid of
          Nothing -> Nothing
          Just f ->
            let widthMode = if mainAxis axisInfo == AxisRow then MeasureAtMost else MeasureUndefined
                heightMode = if mainAxis axisInfo == AxisRow then MeasureUndefined else MeasureAtMost
                availW = fromMaybe 0 mainRef
                availH = fromMaybe 0 crossRef
                out = f (MeasureInput availW widthMode availH heightMode)
            in if mainAxis axisInfo == AxisRow
                 then Just (measuredWidth out)
                 else Just (measuredHeight out)
      aspectContent = case (S.aspectRatio cs, measuredContent, mainSize, crossRef) of
        (Just r, Nothing, Nothing, Just crossV) ->
          if mainAxis axisInfo == AxisRow then Just (crossV * r) else Just (crossV / r)
        _ -> Nothing
      contentMain
        | isJust base0 || isJust measuredContent = Nothing
        | null (N.children kid) = Nothing
        | otherwise = Just (measureContentMain cfg axisInfo dir mainRef crossRef kid)
      base =
        fromMaybe 0
          ( baseFromSpecified
            <|> fmap (+ padBorderMain) (measuredContent <|> aspectContent)
            <|> contentMain
          )
      intrinsicMainSize dim =
        case intrinsicMode dim of
          Nothing -> Nothing
          Just mode ->
            let content = intrinsicMainContent cfg axisInfo dir cs mainRef crossRef kid mode
            in Just (content + padBorderMain)
  in base

resolveMarginFor :: AxisInfo -> Direction -> WritingMode -> Maybe Float -> Node -> Bool -> Float
resolveMarginFor axisInfo dir wm ref kid isStart =
  let edge = mainMarginEdge axisInfo isStart
  in resolveValue (pickEdgeValue (S.margin (N.style kid)) dir wm edge) ref

resolveCrossMarginFor :: AxisInfo -> Direction -> WritingMode -> Maybe Float -> Node -> Bool -> Float
resolveCrossMarginFor axisInfo dir wm ref kid isStart =
  let edge = crossMarginEdge axisInfo isStart
  in resolveValue (pickEdgeValue (S.margin (N.style kid)) dir wm edge) ref

mainMarginEdge :: AxisInfo -> Bool -> Edge
mainMarginEdge axisInfo isStart
  | mainAxis axisInfo == AxisRow =
      if isStart /= mainIsReverse axisInfo then EdgeLeft else EdgeRight
  | isStart /= mainIsReverse axisInfo = EdgeTop
  | otherwise = EdgeBottom

crossMarginEdge :: AxisInfo -> Bool -> Edge
crossMarginEdge axisInfo isStart =
  case crossAxis axisInfo of
    AxisRow -> if isStart /= crossIsReverse axisInfo then EdgeLeft else EdgeRight
    AxisColumn -> if isStart then EdgeTop else EdgeBottom

isAutoMargin :: EdgeValues Value -> Direction -> Edge -> Bool
isAutoMargin vals _dir edge =
  case edge of
    EdgeLeft -> leftE vals == ValAuto
    EdgeRight -> rightE vals == ValAuto
    EdgeTop -> topE vals == ValAuto
    EdgeBottom -> bottomE vals == ValAuto
    _ -> False

-- Lines

newtype FlexLine = FlexLine [FlexItem]

buildLines :: [FlexItem] -> Maybe Float -> Float -> FlexWrap -> [FlexLine]
buildLines items innerMain gapMain wrapMode =
  case wrapMode of
    NoWrap -> [FlexLine items]
    _ ->
      case innerMain of
        Nothing -> [FlexLine items]
        Just maxMain ->
          foldl' (addItem maxMain) [] items
  where
    addItem _ [] item = [FlexLine [item]]
    addItem maxMain acc item =
      let FlexLine current = last acc
          currentSize = lineBaseSize current gapMain
          itemSize = itemOuterMain item
          newSize = if null current then itemSize else currentSize + gapMain + itemSize
      in if newSize > maxMain && not (null current)
          then acc ++ [FlexLine [item]]
          else init acc ++ [FlexLine (current ++ [item])]

lineBaseSize :: [FlexItem] -> Float -> Float
lineBaseSize items gapMain =
  let base = sum (map itemOuterMain items)
      gaps = gapMain * fromIntegral (max 0 (length items - 1))
  in base + gaps

itemOuterMain :: FlexItem -> Float
itemOuterMain it = itemMainBase it + itemMarginMainStart it + itemMarginMainEnd it

-- Sized line and items

data FlexItemSized = FlexItemSized
  { sizedItem :: FlexItem
  , itemMainSize :: Float
  , itemCrossSize :: Float
  , itemMinCross :: Maybe Float
  , itemMaxCross :: Maybe Float
  , itemBaseline :: Maybe Float
  , itemCrossIsExplicit :: Bool
  }


data FlexLineSized = FlexLineSized
  { lineItemsSized :: [FlexItemSized]
  , lineMainUsed :: Float
  , lineCrossSize :: Float
  , lineBaselineAscent :: Float
  }

resolveFlexibleMainSizes :: Maybe Float -> Float -> Maybe Intrinsic -> [FlexItem] -> [Float]
resolveFlexibleMainSizes innerMain gapMain intrinsic items =
  let count = length items
      baseSizes = case intrinsic of
        Just IntrinsicMin -> map (\it -> fromMaybe (itemMainMinContent it) (itemMinMain it)) items
        _ -> map itemMainBase items
      marginSum = sum [ itemMarginMainStart it + itemMarginMainEnd it | it <- items ]
      gapTotal = gapMain * fromIntegral (max 0 (count - 1))
      baseTotal = sum baseSizes
      available = fromMaybe (baseTotal + marginSum + gapTotal) innerMain
      freeSpace0 = available - baseTotal - marginSum - gapTotal
      isGrowing = freeSpace0 > 0
      eps = 1e-6
      initialFrozen = zipWith (\it base -> flexFactor it base <= 0) items baseSizes
      initialSizes = zipWith clampMain items baseSizes
      flexFactor it base =
        if isGrowing then itemFlexGrow it else itemFlexShrink it * base
      clampMain it v =
        let v1 = case itemMinMain it of
              Just m -> max m v
              Nothing -> v
            v2 = case itemMaxMain it of
              Just m -> min m v1
              Nothing -> v1
        in max 0 v2
      loop frozen sizes =
        let used = sum [ if fr then sz else base | (fr, sz, base) <- zip3 frozen sizes baseSizes ]
            freeSpace = available - used - marginSum - gapTotal
            flexSum = sum [ flexFactor it base | (it, base, fr) <- zip3 items baseSizes frozen, not fr ]
        in if flexSum <= eps || abs freeSpace <= eps
            then [ if fr then sz else clampMain it base | (it, base, fr, sz) <- zip4 items baseSizes frozen sizes ]
            else
              let propose it base = base + freeSpace * flexFactor it base / flexSum
                  stepped = [ if fr then (sz, False) else
                                let proposed = propose it base
                                    clamped = clampMain it proposed
                                    violated = abs (clamped - proposed) > eps
                                in (clamped, violated)
                            | (it, base, fr, sz) <- zip4 items baseSizes frozen sizes
                            ]
                  newSizes = map fst stepped
                  newFrozen = zipWith (\fr (_, violated) -> fr || violated) frozen stepped
              in if newFrozen == frozen
                    then newSizes
                    else loop newFrozen newSizes
  in case intrinsic of
      Just _ -> initialSizes
      Nothing ->
        case innerMain of
          Nothing -> initialSizes
          Just _ ->
            if abs freeSpace0 <= 1e-6
              then initialSizes
              else loop initialFrozen initialSizes

sizeLine :: LayoutConfig -> Style -> AxisInfo -> Direction -> Maybe Float -> Maybe Float -> Float -> Maybe Float -> Maybe Intrinsic -> FlexLine -> FlexLineSized
sizeLine cfg parentStyle axisInfo dir innerMain innerCross gapMain forceLineCross intrinsicMain (FlexLine items) =
  let mainSizes = resolveFlexibleMainSizes innerMain gapMain intrinsicMain items
      sizedMain = zip items mainSizes
      -- initial cross sizes (non-stretch)
      sizedCross0 = map (sizeCross cfg parentStyle axisInfo dir innerMain innerCross) sizedMain
      maxCross = maximum (0 : map (outerCross axisInfo) sizedCross0)
      -- baseline
      baselines0 = mapMaybe baselineInfo sizedCross0
      maxAscent = maximum (0 : map fst baselines0)
      maxDescent = maximum (0 : map snd baselines0)
      lineCrossBase = if null baselines0 then maxCross else max maxCross (maxAscent + maxDescent)
      lineCrossTarget = fromMaybe lineCrossBase forceLineCross
      -- apply stretch if needed
      sizedCross = map (applyStretch axisInfo innerCross lineCrossTarget) sizedCross0
      -- baseline
      baselines = mapMaybe baselineInfo sizedCross
      maxAscent2 = maximum (0 : map fst baselines)
      maxDescent2 = maximum (0 : map snd baselines)
      lineCross =
        if null baselines
          then lineCrossTarget
          else
            let maxCross2 = maximum (0 : map (outerCross axisInfo) sizedCross)
            in max maxCross2 (maxAscent2 + maxDescent2)
      lineMainUsed = sum (map (outerMain axisInfo) sizedCross) + gapMain * fromIntegral (max 0 (length sizedCross - 1))
  in FlexLineSized sizedCross lineMainUsed lineCross maxAscent

sizeCross :: LayoutConfig -> Style -> AxisInfo -> Direction -> Maybe Float -> Maybe Float -> (FlexItem, Float) -> FlexItemSized
sizeCross cfg _parentStyle axisInfo dir innerMain innerCross (it, mainSize) =
  let kid = itemNode it
      cs = N.style kid
      ownerW = if mainAxis axisInfo == AxisRow then innerMain else innerCross
      ownerH = if mainAxis axisInfo == AxisRow then innerCross else innerMain
      paddingE = resolveEdgesValue (S.padding cs) dir (S.writingMode cs) ownerW ownerH
      borderE = resolveEdgesFloat (S.border cs) dir (S.writingMode cs)
      padBorderMain =
        if mainAxis axisInfo == AxisRow
          then edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
          else edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
      padBorderCross =
        if mainAxis axisInfo == AxisRow
          then edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
          else edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
      padBorderCrossStart =
        if mainAxis axisInfo == AxisRow
          then edgeTop paddingE + edgeTop borderE
          else edgeLeft paddingE + edgeLeft borderE
      contentMainSize = max 0 (mainSize - padBorderMain)
      crossRef = innerCross
      crossDim = if mainAxis axisInfo == AxisRow then S.height cs else S.width cs
      resolved = resolveDimensionWith crossDim crossRef
      crossIntrinsic = intrinsicMode crossDim
      crossExplicit = isJust resolved || isJust crossIntrinsic
      measureCross override =
        case N.measure kid of
          Nothing -> (Nothing, Nothing)
          Just f ->
            let (crossMode, crossAvail) = case override of
                  Just IntrinsicMin -> (MeasureAtMost, 0)
                  Just IntrinsicMax -> (MeasureUndefined, 0)
                  Nothing -> (maybe MeasureUndefined (const MeasureAtMost) innerCross, fromMaybe 0 innerCross)
                (availW, modeW, availH, modeH) =
                  if mainAxis axisInfo == AxisRow
                    then (contentMainSize, MeasureExactly, crossAvail, crossMode)
                    else (crossAvail, crossMode, contentMainSize, MeasureExactly)
                out = f (MeasureInput availW modeW availH modeH)
                crossMeasured = if mainAxis axisInfo == AxisRow then measuredHeight out else measuredWidth out
            in (Just crossMeasured, measuredBaseline out)
      (measuredCross, measuredBase) =
        case crossIntrinsic of
          Just mode -> measureCross (Just mode)
          Nothing -> measureCross Nothing
      intrinsicCrossContent = case crossIntrinsic of
        Nothing -> Nothing
        Just _ ->
          case measuredCross of
            Just v -> Just v
            Nothing ->
              if null (N.children kid)
                then Just 0
                else Just (measureContentCross cfg axisInfo dir contentMainSize crossRef kid)
      crossSpecified = case resolved of
        Just v -> Just (if S.boxSizing cs == BorderBox then v else v + padBorderCross)
        Nothing ->
          fmap (\v -> if S.boxSizing cs == BorderBox then v + padBorderCross else v) intrinsicCrossContent
      aspectContent = case (S.aspectRatio cs, resolved) of
        (Just r, Nothing) ->
          if mainAxis axisInfo == AxisRow then Just (contentMainSize / r) else Just (contentMainSize * r)
        _ -> Nothing
      measuredContent = if isJust crossIntrinsic then Nothing else measuredCross
      cross0 = crossSpecified <|> fmap (+ padBorderCross) (aspectContent <|> measuredContent)
      minDim = if mainAxis axisInfo == AxisRow then S.minHeight cs else S.minWidth cs
      crossAuto = not (isDefiniteDimension crossDim crossRef)
      minCrossResolved = resolveBorderSize (S.boxSizing cs) padBorderCross (resolveDimensionWith minDim crossRef)
      minCrossIntrinsic = intrinsicCrossForDim minDim
      overflowVisible = S.overflow cs == OverflowVisible
      minCrossDefault =
        if isAutoMin minDim && crossAuto
          then
            let contentCross =
                  if overflowVisible
                    then measuredContent <|> aspectContent <|> if null (N.children kid)
                      then Nothing
                      else Just (measureContentCross cfg axisInfo dir contentMainSize crossRef kid)
                    else Just 0
            in fmap (+ padBorderCross) contentCross
          else Nothing
      minCross = minCrossResolved <|> minCrossIntrinsic <|> minCrossDefault
      maxCrossResolved = resolveBorderSize (S.boxSizing cs) padBorderCross (resolveDimensionWith (if mainAxis axisInfo == AxisRow then S.maxHeight cs else S.maxWidth cs) crossRef)
      maxCrossIntrinsic = intrinsicCrossForDim (if mainAxis axisInfo == AxisRow then S.maxHeight cs else S.maxWidth cs)
      maxCross = maxCrossResolved <|> maxCrossIntrinsic
      cross1 = clampMaybe minCross maxCross cross0
      crossSize = max 0 (fromMaybe 0 cross1)
      usedMeasuredBaseline =
        case crossIntrinsic of
          Just _ -> measuredBase
          Nothing -> if isNothing crossSpecified && isNothing aspectContent then measuredBase else Nothing
      intrinsicCrossForDim dim =
        case intrinsicMode dim of
          Nothing -> Nothing
          Just mode ->
            case N.measure kid of
              Just _ ->
                let (measuredCrossDim, _) = measureCross (Just mode)
                in fmap (\v -> if S.boxSizing cs == BorderBox then v + padBorderCross else v) measuredCrossDim
              Nothing ->
                if null (N.children kid)
                  then Just (if S.boxSizing cs == BorderBox then padBorderCross else 0)
                  else
                    let contentCross = measureContentCross cfg axisInfo dir contentMainSize crossRef kid
                    in Just (if S.boxSizing cs == BorderBox then contentCross + padBorderCross else contentCross)
  in FlexItemSized it mainSize crossSize minCross maxCross (baselineFor axisInfo (itemAlignSelf it) kid mainSize crossSize usedMeasuredBaseline padBorderCrossStart) crossExplicit

applyStretch :: AxisInfo -> Maybe Float -> Float -> FlexItemSized -> FlexItemSized
applyStretch _axisInfo innerCross lineCross itemSized =
  let it = sizedItem itemSized
      align = itemAlignSelf it
      shouldStretch = align == AlignStretch
      hasExplicit = itemCrossIsExplicit itemSized
      target = if shouldStretch && not hasExplicit
        then
          case innerCross of
            Just _ -> lineCross - itemMarginCrossStart it - itemMarginCrossEnd it
            Nothing -> lineCross - itemMarginCrossStart it - itemMarginCrossEnd it
        else itemCrossSize itemSized
      clamped = clampMaybe (itemMinCross itemSized) (itemMaxCross itemSized) (Just target)
      finalSize = max 0 (fromMaybe target clamped)
  in itemSized { itemCrossSize = finalSize }

stretchLine :: AxisInfo -> Maybe Float -> Float -> FlexLineSized -> FlexLineSized
stretchLine axisInfo innerCross target lineSized =
  let items = map (applyStretch axisInfo innerCross target) (lineItemsSized lineSized)
  in lineSized { lineItemsSized = items, lineCrossSize = target }

outerCross :: AxisInfo -> FlexItemSized -> Float
outerCross _ itemSized =
  let it = sizedItem itemSized
  in itemCrossSize itemSized + itemMarginCrossStart it + itemMarginCrossEnd it

outerMain :: AxisInfo -> FlexItemSized -> Float
outerMain _ itemSized =
  let it = sizedItem itemSized
  in itemMainSize itemSized + itemMarginMainStart it + itemMarginMainEnd it

baselineInfo :: FlexItemSized -> Maybe (Float, Float)
baselineInfo itemSized =
  case itemBaseline itemSized of
    Nothing -> Nothing
    Just base ->
      let it = sizedItem itemSized
          ascent = base + itemMarginCrossStart it
          descent = (itemCrossSize itemSized - base) + itemMarginCrossEnd it
      in Just (ascent, descent)

baselineFor :: AxisInfo -> Align -> Node -> Float -> Float -> Maybe Float -> Float -> Maybe Float
baselineFor axisInfo align kid w h measuredBaseline padBorderCrossStart
  | mainAxis axisInfo /= AxisRow = Nothing
  | not (align == AlignBaseline || align == AlignFirstBaseline || align == AlignLastBaseline) = Nothing
  | otherwise =
      case N.baseline kid of
        Just f -> Just (f w h)
        Nothing -> case measuredBaseline of
          Just b -> Just (b + padBorderCrossStart)
          Nothing -> Just h

resolveFinalCross :: Maybe Float -> Float -> [FlexLineSized] -> (Float, [FlexLineSized])
resolveFinalCross innerCross gapCross lines0 =
  case innerCross of
    Just v -> (v, lines0)
    Nothing ->
      let total = sum (map lineCrossSize lines0) + gapCross * fromIntegral (max 0 (length lines0 - 1))
      in (total, lines0)

maxLineMainUsed :: [FlexLineSized] -> Float
maxLineMainUsed lines0 =
  maximum (0 : map lineMainUsed lines0)

positionLines :: AxisInfo -> Style -> Float -> Float -> [FlexLineSized] -> ([FlexLineSized], [Float])
positionLines axisInfo s innerCross gapCross lines0 =
  let count = length lines0
      total = sum (map lineCrossSize lines0) + gapCross * fromIntegral (max 0 (count - 1))
      remaining = max 0 (innerCross - total)
      (start, between, stretchAdd) = if count <= 1
        then (0, gapCross, 0)
        else alignContentOffsets (S.alignContent s) remaining gapCross count
      baseSizes = map lineCrossSize lines0
      sizes = if stretchAdd > 0
        then map (+ stretchAdd) baseSizes
        else baseSizes
      sizedLines = if stretchAdd > 0
        then zipWith (stretchLine axisInfo (Just innerCross)) sizes lines0
        else lines0
      offsets = scanl' (+) start (zipWith (+) sizes (replicate (max 0 (length sizes - 1)) between))
  in (sizedLines, take (length lines0) offsets)

alignContentOffsets :: Align -> Float -> Float -> Int -> (Float, Float, Float)
alignContentOffsets align remSpace gapCross count =
  case align of
    AlignFlexStart -> (0, gapCross, 0)
    AlignCenter -> (remSpace / 2, gapCross, 0)
    AlignFlexEnd -> (remSpace, gapCross, 0)
    AlignBaseline -> (0, gapCross, 0)
    AlignFirstBaseline -> (0, gapCross, 0)
    AlignLastBaseline -> (0, gapCross, 0)
    AlignSpaceBetween -> if count > 1
      then (0, gapCross + remSpace / fromIntegral (count - 1), 0)
      else (0, gapCross, 0)
    AlignSpaceAround -> if count > 0
      then let between = gapCross + remSpace / fromIntegral count
           in (between / 2 - gapCross / 2, between, 0)
      else (0, gapCross, 0)
    AlignSpaceEvenly -> if count > 0
      then let between = gapCross + remSpace / fromIntegral (count + 1)
           in (between - gapCross, between, 0)
      else (0, gapCross, 0)
    AlignStretch -> if count > 0
      then (0, gapCross, remSpace / fromIntegral count)
      else (0, gapCross, 0)
    AlignAuto -> (0, gapCross, 0)

positionLineItems :: Style -> AxisInfo -> Float -> Float -> Float -> Float -> FlexLineSized -> [(FlexItemSized, Float, Float, Float, Float)]
positionLineItems s axisInfo innerMain innerCross gapMain lineCrossStart lineSized =
  let items0 = lineItemsSized lineSized
      remaining0 = max 0 (innerMain - lineMainUsed lineSized)
      autoCount = sum (map autoMainCount items0)
      autoSize = if autoCount > 0 then remaining0 / fromIntegral autoCount else 0
      items = if autoCount > 0 then map (applyAutoMargins autoSize) items0 else items0
      remaining = if autoCount > 0 then 0 else remaining0
      (start, between) = if autoCount > 0 then (0, gapMain) else justifyOffsets (S.justifyContent s) remaining gapMain (length items)
      positions = scanl' (+) start (zipWith (+) (map (outerMain axisInfo) items) (replicate (max 0 (length items - 1)) between))
  in zipWith (placeItem axisInfo lineCrossStart lineSized innerMain innerCross) items positions

placeItem :: AxisInfo -> Float -> FlexLineSized -> Float -> Float -> FlexItemSized -> Float -> (FlexItemSized, Float, Float, Float, Float)
placeItem axisInfo lineCrossStart lineSized innerMain innerCross itemSized mainPos =
  let it = sizedItem itemSized
      mainSize = itemMainSize itemSized
      crossSize = itemCrossSize itemSized
      mainPos' = mainPos + itemMarginMainStart it
      crossAutoCount = fromEnum (itemMarginCrossStartAuto it) + fromEnum (itemMarginCrossEndAuto it)
      crossRemaining = max 0 (lineCrossSize lineSized - crossSize - itemMarginCrossStart it - itemMarginCrossEnd it)
      crossAuto = if crossAutoCount > 0 then crossRemaining / fromIntegral crossAutoCount else 0
      marginCrossStart' = itemMarginCrossStart it + if itemMarginCrossStartAuto it then crossAuto else 0
      marginCrossEnd' = itemMarginCrossEnd it + if itemMarginCrossEndAuto it then crossAuto else 0
      crossPosBase =
        if crossAutoCount > 0
          then lineCrossStart + marginCrossStart'
          else case itemAlignSelf it of
            AlignFlexStart -> lineCrossStart + marginCrossStart'
            AlignFlexEnd -> lineCrossStart + lineCrossSize lineSized - crossSize - marginCrossEnd'
            AlignCenter ->
              lineCrossStart + (lineCrossSize lineSized - crossSize - marginCrossStart' - marginCrossEnd') / 2 + marginCrossStart'
            AlignBaseline ->
              case itemBaseline itemSized of
                Just base -> lineCrossStart + lineBaselineAscent lineSized - base + marginCrossStart'
                Nothing -> lineCrossStart + marginCrossStart'
            AlignFirstBaseline ->
              case itemBaseline itemSized of
                Just base -> lineCrossStart + lineBaselineAscent lineSized - base + marginCrossStart'
                Nothing -> lineCrossStart + marginCrossStart'
            AlignLastBaseline ->
              case itemBaseline itemSized of
                Just base -> lineCrossStart + lineBaselineAscent lineSized - base + marginCrossStart'
                Nothing -> lineCrossStart + marginCrossStart'
            _ -> lineCrossStart + marginCrossStart'
      mainCoord = if mainIsReverse axisInfo then innerMain - mainPos' - mainSize else mainPos'
      crossCoord =
        if crossIsReverse axisInfo
          then innerCross - crossPosBase - crossSize
          else crossPosBase
      (x, y) =
        if mainAxis axisInfo == AxisRow
          then (mainCoord, crossCoord)
          else (crossCoord, mainCoord)
      (w, h) =
        if mainAxis axisInfo == AxisRow
          then (mainSize, crossSize)
          else (crossSize, mainSize)
  in (itemSized, x, y, w, h)

justifyOffsets :: Justify -> Float -> Float -> Int -> (Float, Float)
justifyOffsets justify remSpace gapMain count =
  case justify of
    JustifyFlexStart -> (0, gapMain)
    JustifyCenter -> (remSpace / 2, gapMain)
    JustifyFlexEnd -> (remSpace, gapMain)
    JustifySpaceBetween -> if count > 1
      then (0, gapMain + remSpace / fromIntegral (count - 1))
      else (0, gapMain)
    JustifySpaceAround -> if count > 0
      then let between = gapMain + remSpace / fromIntegral count
           in (between / 2 - gapMain / 2, between)
      else (0, gapMain)
    JustifySpaceEvenly -> if count > 0
      then let between = gapMain + remSpace / fromIntegral (count + 1)
           in (between - gapMain, between)
      else (0, gapMain)

autoMainCount :: FlexItemSized -> Int
autoMainCount itemSized =
  let it = sizedItem itemSized
  in fromEnum (itemMarginMainStartAuto it) + fromEnum (itemMarginMainEndAuto it)

applyAutoMargins :: Float -> FlexItemSized -> FlexItemSized
applyAutoMargins autoSize itemSized =
  let it = sizedItem itemSized
      mStart = itemMarginMainStart it + if itemMarginMainStartAuto it then autoSize else 0
      mEnd = itemMarginMainEnd it + if itemMarginMainEndAuto it then autoSize else 0
      it' = it { itemMarginMainStart = mStart, itemMarginMainEnd = mEnd }
  in itemSized { sizedItem = it' }

buildChildLayout :: LayoutConfig -> Size -> Direction -> EdgeValues Float -> EdgeValues Float -> (Node, Float, Float, Float, Float) -> LayoutNode
buildChildLayout cfg ownerSize parentDir paddingE borderE (kid, x, y, w, h) =
  let originX = edgeLeft paddingE + edgeLeft borderE
      originY = edgeTop paddingE + edgeTop borderE
      childLayout = computeNodeLayout cfg ownerSize (Just (w, h)) parentDir kid
      l = LT.layout childLayout
      l' = l { LT.left = originX + x + LT.left l, LT.top = originY + y + LT.top l }
  in childLayout { LT.layout = applyRounding cfg l' }

buildFlexLayout :: LayoutConfig -> Size -> Direction -> EdgeValues Float -> EdgeValues Float -> (FlexItemSized, Float, Float, Float, Float) -> (Int, LayoutNode)
buildFlexLayout cfg ownerSize parentDir paddingE borderE (itemSized, x, y, w, h) =
  let it = sizedItem itemSized
      idx = itemIndex it
      kid = itemNode it
  in (idx, buildChildLayout cfg ownerSize parentDir paddingE borderE (kid, x, y, w, h))

reorderFlexLayouts :: [(Int, LayoutNode)] -> [LayoutNode]
reorderFlexLayouts layouts =
  map snd (sortOn fst layouts)

computeStaticPositions :: LayoutConfig -> AxisInfo -> Style -> Direction -> Maybe Float -> Maybe Float -> Float -> Float -> FlexWrap -> [(Int, Node)] -> [(Int, (Float, Float))]
computeStaticPositions cfg axisInfo parentStyle dir innerMain innerCross gapMain gapCross wrapMode indexedKids =
  let flexItems = [buildFlexItem cfg axisInfo parentStyle dir innerMain innerCross idx kid | (idx, kid) <- indexedKids]
      orderedItems = sortOn (\it -> (itemOrder it, itemIndex it)) flexItems
      lineMainRef = innerMain
      lines0 = buildLines orderedItems lineMainRef gapMain wrapMode
      forceLineCross = if length lines0 == 1 then innerCross else Nothing
      sizedLines = map (sizeLine cfg parentStyle axisInfo dir innerMain innerCross gapMain forceLineCross Nothing) lines0
      (finalInnerCross, sizedLines2) = resolveFinalCross innerCross gapCross sizedLines
      finalInnerMain = fromMaybe (maxLineMainUsed sizedLines2) innerMain
      (sizedLines3, linePositions) = positionLines axisInfo parentStyle finalInnerCross gapCross sizedLines2
      positioned = concat (zipWith (positionLineItems parentStyle axisInfo finalInnerMain finalInnerCross gapMain) linePositions sizedLines3)
  in [ (itemIndex (sizedItem itemSized), (x, y)) | (itemSized, x, y, _, _) <- positioned ]

lookupStaticPos :: [(Int, (Float, Float))] -> Int -> Maybe (Float, Float)
lookupStaticPos positions idx = lookup idx positions

-- Absolute children
layoutAbsChild :: LayoutConfig -> Size -> EdgeValues Float -> EdgeValues Float -> Maybe Float -> Maybe Float -> Direction -> (Int -> Maybe (Float, Float)) -> (Int, Node) -> LayoutNode
layoutAbsChild cfg ownerSize paddingE borderE innerW innerH dir staticLookup (idx, kid) =
  let cs = N.style kid
      wm = S.writingMode cs
      leftV = resolveValueMaybe (pickEdgeValue (S.position cs) dir wm EdgeLeft) innerW
      rightV = resolveValueMaybe (pickEdgeValue (S.position cs) dir wm EdgeRight) innerW
      topV = resolveValueMaybe (pickEdgeValue (S.position cs) dir wm EdgeTop) innerH
      bottomV = resolveValueMaybe (pickEdgeValue (S.position cs) dir wm EdgeBottom) innerH
      marginE = resolveEdgesValue (S.margin cs) dir wm innerW innerH
      marginLeft = edgeLeft marginE
      marginRight = edgeRight marginE
      marginTop = edgeTop marginE
      marginBottom = edgeBottom marginE
      marginLeftAuto = isAutoMargin (S.margin cs) dir EdgeLeft
      marginRightAuto = isAutoMargin (S.margin cs) dir EdgeRight
      marginTopAuto = isAutoMargin (S.margin cs) dir EdgeTop
      marginBottomAuto = isAutoMargin (S.margin cs) dir EdgeBottom
      paddingChild = resolveEdgesValue (S.padding cs) dir wm innerW innerH
      borderChild = resolveEdgesFloat (S.border cs) dir wm
      padBorderW = edgeLeft paddingChild + edgeRight paddingChild + edgeLeft borderChild + edgeRight borderChild
      padBorderH = edgeTop paddingChild + edgeBottom paddingChild + edgeTop borderChild + edgeBottom borderChild
      widthIntrinsic = intrinsicMode (S.width cs)
      heightIntrinsic = intrinsicMode (S.height cs)
      intrinsicLayout =
        if isJust widthIntrinsic || isJust heightIntrinsic
          then Just (computeNodeLayout cfg ownerSize Nothing dir kid)
          else Nothing
      intrinsicW = fmap (LT.width . LT.layout) intrinsicLayout
      intrinsicH = fmap (LT.height . LT.layout) intrinsicLayout
      widthV = case widthIntrinsic of
        Just _ -> intrinsicW
        Nothing -> resolveBorderSize (S.boxSizing cs) padBorderW (resolveDimensionWith (S.width cs) innerW)
      heightV = case heightIntrinsic of
        Just _ -> intrinsicH
        Nothing -> resolveBorderSize (S.boxSizing cs) padBorderH (resolveDimensionWith (S.height cs) innerH)
      (w0, h0) =
        let w' = case (widthV, leftV, rightV, innerW) of
              (Just widthVal, _, _, _) -> Just widthVal
              (Nothing, Just leftVal, Just rightVal, Just iw) -> Just (max 0 (iw - leftVal - rightVal))
              _ -> Nothing
            h' = case (heightV, topV, bottomV, innerH) of
              (Just heightVal, _, _, _) -> Just heightVal
              (Nothing, Just topVal, Just bottomVal, Just ih) -> Just (max 0 (ih - topVal - bottomVal))
              _ -> Nothing
        in (w', h')
      autoOwnerSize =
        let ownerW' = case w0 of
              Just bw -> DimPoints (max 0 (bw - padBorderW))
              Nothing -> DimUndefined
            ownerH' = case h0 of
              Just bh -> DimPoints (max 0 (bh - padBorderH))
              Nothing -> DimUndefined
        in Size ownerW' ownerH'
      autoLayout =
        if isNothing w0 || isNothing h0
          then intrinsicLayout <|> Just (computeNodeLayout cfg autoOwnerSize Nothing dir kid)
          else Nothing
      autoW = fmap (LT.width . LT.layout) autoLayout
      autoH = fmap (LT.height . LT.layout) autoLayout
      w1 = w0 <|> autoW
      h1 = h0 <|> autoH
      contentW0 = fmap (\val -> max 0 (val - padBorderW)) w1
      contentH0 = fmap (\val -> max 0 (val - padBorderH)) h1
      (contentW1, contentH1) = applyAspectRatio (S.aspectRatio cs) contentW0 contentH0
      (minContentW, maxContentW) = resolveContentMinMax (S.boxSizing cs) (S.minWidth cs) (S.maxWidth cs) innerW padBorderW
      (minContentH, maxContentH) = resolveContentMinMax (S.boxSizing cs) (S.minHeight cs) (S.maxHeight cs) innerH padBorderH
      contentW2 = clampMaybe minContentW maxContentW contentW1
      contentH2 = clampMaybe minContentH maxContentH contentH1
      w = max 0 (maybe 0 (+ padBorderW) contentW2)
      h = max 0 (maybe 0 (+ padBorderH) contentH2)
      (staticX, staticY) = fromMaybe (0, 0) (staticLookup idx)
      (x0, _) =
        case (leftV, rightV, innerW) of
          (Just leftPos, Just rightPos, Just iw) ->
            let free = max 0 (iw - leftPos - rightPos - w - marginLeft - marginRight)
                autoCount = fromEnum marginLeftAuto + fromEnum marginRightAuto
                autoSize = if autoCount > 0 then free / fromIntegral autoCount else 0
                mLeft = marginLeft + if marginLeftAuto then autoSize else 0
            in (leftPos + mLeft, free)
          (Just leftPos, _, _) ->
            let mLeft = marginLeft
            in (leftPos + mLeft, 0)
          (Nothing, Just r, Just iw) ->
            let mRight = marginRight
            in (max 0 (iw - r - w - mRight), 0)
          (Nothing, Nothing, _) -> (staticX, 0)
          _ -> (staticX, 0)
      y0 =
        case (topV, bottomV, innerH) of
          (Just topPos, Just bottomPos, Just ih) ->
            let free = max 0 (ih - topPos - bottomPos - h - marginTop - marginBottom)
                autoCount = fromEnum marginTopAuto + fromEnum marginBottomAuto
                autoSize = if autoCount > 0 then free / fromIntegral autoCount else 0
                mTop = marginTop + if marginTopAuto then autoSize else 0
            in topPos + mTop
          (Just t, _, _) ->
            let mTop = marginTop
            in t + mTop
          (Nothing, Just b, Just ih) ->
            let mBottom = marginBottom
            in max 0 (ih - b - h - mBottom)
          (Nothing, Nothing, _) -> staticY
          _ -> staticY
      originX = edgeLeft paddingE + edgeLeft borderE
      originY = edgeTop paddingE + edgeTop borderE
      childLayout = computeNodeLayout cfg ownerSize (Just (w, h)) dir kid
      l = LT.layout childLayout
      l' = l { LT.left = originX + x0 + LT.left l, LT.top = originY + y0 + LT.top l }
  in childLayout { LT.layout = applyRounding cfg l' }

partitionChildren :: [(Int, Node)] -> ([(Int, Node)], [(Int, Node)])
partitionChildren = foldr go ([], [])
  where
    go n (flexAcc, absAcc) =
      if S.positionType (N.style (snd n)) == PositionAbsolute
        then (flexAcc, n : absAcc)
        else (n : flexAcc, absAcc)

mergeChildren :: [Node] -> [LayoutNode] -> [LayoutNode] -> [LayoutNode]
mergeChildren original flexLayouts absLayouts =
  let flexQueue = flexLayouts
      absQueue = absLayouts
  in merge original flexQueue absQueue
  where
    merge [] _ _ = []
    merge (c:cs) flexQ absQ =
      if S.positionType (N.style c) == PositionAbsolute
        then case absQ of
          [] -> merge cs flexQ absQ
          (a:as) -> a : merge cs flexQ as
        else case flexQ of
          [] -> merge cs flexQ absQ
          (f:fs) -> f : merge cs fs absQ

-- Rounding
applyRounding :: LayoutConfig -> Layout -> Layout
applyRounding cfg l =
  case pointScaleFactor cfg of
    Nothing -> l
    Just s | s <= 0 -> l
    Just s ->
      let roundTo :: Float -> Float
          roundTo v = fromIntegral (round (v * s) :: Int) / s
          left0 = roundTo (LT.left l)
          top0 = roundTo (LT.top l)
          right0 = roundTo (LT.left l + LT.width l)
          bottom0 = roundTo (LT.top l + LT.height l)
      in l
        { LT.left = left0
        , LT.top = top0
        , LT.width = right0 - left0
        , LT.height = bottom0 - top0
        , LT.margin = mapEdgeValues roundTo (LT.margin l)
        , LT.padding = mapEdgeValues roundTo (LT.padding l)
        , LT.border = mapEdgeValues roundTo (LT.border l)
        }

applyRelativeOffset :: Style -> Direction -> Maybe Float -> Maybe Float -> Layout -> Layout
applyRelativeOffset s dir ownerW ownerH l =
  if S.positionType s /= PositionRelative
    then l
    else
      let wm = S.writingMode s
          leftV = resolveValueMaybe (pickEdgeValue (S.position s) dir wm EdgeLeft) ownerW
          rightV = resolveValueMaybe (pickEdgeValue (S.position s) dir wm EdgeRight) ownerW
          topV = resolveValueMaybe (pickEdgeValue (S.position s) dir wm EdgeTop) ownerH
          bottomV = resolveValueMaybe (pickEdgeValue (S.position s) dir wm EdgeBottom) ownerH
          dx = case (leftV, rightV) of
            (Just x, _) -> x
            (Nothing, Just r) -> -r
            _ -> 0
          dy = case (topV, bottomV) of
            (Just y, _) -> y
            (Nothing, Just b) -> -b
            _ -> 0
      in l { LT.left = LT.left l + dx, LT.top = LT.top l + dy }

measureContentMain :: LayoutConfig -> AxisInfo -> Direction -> Maybe Float -> Maybe Float -> Node -> Float
measureContentMain cfg axisInfo parentDir mainRef crossRef kid =
  measureContentMainIntrinsic cfg axisInfo parentDir mainRef crossRef kid IntrinsicMax

measureContentCross :: LayoutConfig -> AxisInfo -> Direction -> Float -> Maybe Float -> Node -> Float
measureContentCross cfg axisInfo parentDir mainSize crossRef kid =
  let ownerSize =
        if mainAxis axisInfo == AxisRow
          then Size (DimPoints mainSize) DimUndefined
          else Size DimUndefined (DimPoints mainSize)
      measured = computeNodeLayout cfg ownerSize Nothing parentDir kid
      l = LT.layout measured
      cs = N.style kid
      dir = resolveDirection parentDir cs
      ownerW = if mainAxis axisInfo == AxisRow then Just mainSize else crossRef
      ownerH = if mainAxis axisInfo == AxisRow then crossRef else Just mainSize
      paddingE = resolveEdgesValue (S.padding cs) dir (S.writingMode cs) ownerW ownerH
      borderE = resolveEdgesFloat (S.border cs) dir (S.writingMode cs)
      padBorderCross =
        if mainAxis axisInfo == AxisRow
          then edgeTop paddingE + edgeBottom paddingE + edgeTop borderE + edgeBottom borderE
          else edgeLeft paddingE + edgeRight paddingE + edgeLeft borderE + edgeRight borderE
      measuredCross = if mainAxis axisInfo == AxisRow then LT.height l else LT.width l
  in max 0 (measuredCross - padBorderCross)

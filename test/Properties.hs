module Properties
  ( propertyTests
  ) where

import Data.Function ((&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import Flexy
import qualified Flexy.Layout as L
import Flexy.Measure (MeasureInput(..), MeasureMode(..), MeasureOutput(..))
import Flexy.Types (EdgeValues(..))

propertyTests :: TestTree
propertyTests = testGroup "flexy-properties"
  [ QC.testProperty "flex_grow_distributes" propFlexGrow
  , QC.testProperty "flex_shrink_proportional" propFlexShrink
  , QC.testProperty "percent_width_resolves" propPercentWidth
  , QC.testProperty "padding_border_origin" propPaddingOrigin
  , QC.testProperty "percent_padding_uses_width" propPercentPaddingWidth
  , QC.testProperty "auto_margins_center" propAutoMarginsCenter
  , QC.testProperty "non_negative_sizes" propNonNegativeSizes
  , QC.testProperty "box_sizing_border_box_width" propBoxSizingBorderBox
  , QC.testProperty "box_sizing_content_box_width" propBoxSizingContentBox
  , QC.testProperty "stretch_respects_max_height" propStretchMaxHeight
  , QC.testProperty "min_content_container_width" propMinContentContainerWidth
  , QC.testProperty "max_content_container_width" propMaxContentContainerWidth
  , QC.testProperty "baseline_aligns_equal_baseline" propBaselineAlignsEqual
  , QC.testProperty "auto_margins_cross_axis_center" propAutoMarginsCrossAxisCenter
  , QC.testProperty "flex_basis_percent_resolves" propFlexBasisPercent
  , QC.testProperty "justify_space_evenly_positions" propJustifySpaceEvenlyPositions
  , QC.testProperty "direction_inheritance_border_rtl" propDirectionInheritanceBorderRtl
  , QC.testProperty "writing_mode_start_end_vertical" propWritingModeStartEndVertical
  , QC.testProperty "absolute_auto_size_measure" propAbsoluteAutoSizeMeasure
  , QC.testProperty "percent_margin_uses_width" propPercentMarginUsesWidth
  , QC.testProperty "baseline_line_contains_tall" propBaselineLineContainsTall
  , QC.testProperty "explicit_zero_border_override" propExplicitZeroBorderOverride
  , QC.testProperty "random_tree_finite" propRandomTreeFinite
  , QC.testProperty "random_tree_non_negative" propRandomTreeNonNegative
  ]

propFlexGrow :: Property
propFlexGrow = forAll gen $ \(cw, g1, g2) ->
  let childStyle = defaultStyle
        & setFlexGrow g1
        & setFlexBasis (DimPoints 0)
        & setHeight (DimPoints 10)
      childStyle2 = defaultStyle
        & setFlexGrow g2
        & setFlexBasis (DimPoints 0)
        & setHeight (DimPoints 10)
      c1 = node childStyle
      c2 = node childStyle2
      rootStyle = defaultStyle & setFlexDirection Row
      root = withChildren [c1, c2] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints 20)) root
  in expectChildren2 layoutRoot $ \l1 l2 ->
      let (_, _, w1, _) = layoutBounds l1
          (_, _, w2, _) = layoutBounds l2
          totalW = w1 + w2
          expected1 = cw * g1 / (g1 + g2)
          expected2 = cw * g2 / (g1 + g2)
      in approxEq totalW cw .&&. approxEq w1 expected1 .&&. approxEq w2 expected2
  where
    gen = do
      cw <- chooseFloat (50, 500)
      g1 <- chooseFloat (0.5, 5)
      g2 <- chooseFloat (0.5, 5)
      pure (cw, g1, g2)

propFlexShrink :: Property
propFlexShrink = forAll gen $ \(cw, w1, w2) ->
  let c1 = node (defaultStyle & setSize (DimPoints w1) (DimPoints 10))
      c2 = node (defaultStyle & setSize (DimPoints w2) (DimPoints 10))
      root = withChildren [c1, c2] (node defaultStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints 20)) root
  in expectChildren2 layoutRoot $ \l1 l2 ->
      let (_, _, lw1, _) = layoutBounds l1
          (_, _, lw2, _) = layoutBounds l2
          totalW = w1 + w2
          expected1 = w1 * cw / totalW
          expected2 = w2 * cw / totalW
      in approxEq lw1 expected1 .&&. approxEq lw2 expected2
  where
    gen = do
      w1 <- chooseFloat (50, 200)
      w2 <- chooseFloat (50, 200)
      let totalW = w1 + w2
      cw <- chooseFloat (10, totalW - 1)
      pure (cw, w1, w2)

propPercentWidth :: Property
propPercentWidth = forAll gen $ \(cw, p) ->
  let childStyle = defaultStyle & setSize (DimPercent p) (DimPoints 10)
      c1 = node childStyle
      root = withChildren [c1] (node defaultStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints 20)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, w, _) = layoutBounds l1
          expected = cw * p / 100
      in approxEq w expected
  where
    gen = do
      cw <- chooseFloat (10, 500)
      p <- chooseFloat (1, 99)
      pure (cw, p)

propPaddingOrigin :: Property
propPaddingOrigin = forAll gen $ \(cw, ch, pad, borderW) ->
  let rootStyle = defaultStyle
        & setPaddingAll (ValPoints pad)
        & setBorderAll borderW
      childStyle = defaultStyle & setSize (DimPoints 10) (DimPoints 10)
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints ch)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (x, y, _, _) = layoutBounds l1
          expected = pad + borderW
      in approxEq x expected .&&. approxEq y expected
  where
    gen = do
      pad <- chooseFloat (0, 20)
      borderW <- chooseFloat (0, 10)
      cw <- chooseFloat (50, 200)
      ch <- chooseFloat (50, 200)
      pure (cw, ch, pad, borderW)

propPercentPaddingWidth :: Property
propPercentPaddingWidth = forAll gen $ \(cw, ch, p) ->
  let rootStyle = defaultStyle & setPaddingAll (ValPercent p)
      childStyle = defaultStyle & setSize (DimPoints 10) (DimPoints 10)
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints ch)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (x, y, _, _) = layoutBounds l1
          expected = cw * p / 100
      in approxEq x expected .&&. approxEq y expected
  where
    gen = do
      cw <- chooseFloat (50, 300)
      ch <- chooseFloat (50, 300)
      p <- chooseFloat (1, 40)
      pure (cw, ch, p)

propAutoMarginsCenter :: Property
propAutoMarginsCenter = forAll gen $ \(cw, w) ->
  let childStyle = defaultStyle
        & setSize (DimPoints w) (DimPoints 10)
        & setMarginLR ValAuto ValAuto
      root = withChildren [node childStyle] (node defaultStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints 20)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (x, _, _, _) = layoutBounds l1
          expected = (cw - w) / 2
      in approxEq x expected
  where
    gen = do
      cw <- chooseFloat (50, 300)
      w <- chooseFloat (10, 40)
      pure (cw, w)

propNonNegativeSizes :: Property
propNonNegativeSizes = forAll gen $ \(cw, ch, pad, borderW, w, h) ->
  let rootStyle = defaultStyle
        & setPaddingAll (ValPoints pad)
        & setBorderAll borderW
      childStyle = defaultStyle & setSize (DimPoints w) (DimPoints h)
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints ch)) root
      (rw, rh) = layoutSize layoutRoot
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, cw1, ch1) = layoutBounds l1
      in rw >= 0 .&&. rh >= 0 .&&. cw1 >= 0 .&&. ch1 >= 0
  where
    layoutSize n =
      let (_, _, w0, h0) = layoutBounds n
      in (w0, h0)

    gen = do
      cw <- chooseFloat (10, 200)
      ch <- chooseFloat (10, 200)
      pad <- chooseFloat (0, 30)
      borderW <- chooseFloat (0, 10)
      w <- chooseFloat (0, 100)
      h <- chooseFloat (0, 100)
      pure (cw, ch, pad, borderW, w, h)

propBoxSizingBorderBox :: Property
propBoxSizingBorderBox = forAll gen $ \(w, pad, borderW) ->
  let childStyle = defaultStyle
        & setBoxSizing BorderBox
        & setWidth (DimPoints w)
        & setPaddingAll (ValPoints pad)
        & setBorderAll borderW
      root = withChildren [node childStyle] (node defaultStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 500) (DimPoints 100)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, w1, _) = layoutBounds l1
      in approxEq w1 w
  where
    gen = do
      w <- chooseFloat (10, 200)
      pad <- chooseFloat (0, 20)
      borderW <- chooseFloat (0, 10)
      pure (w, pad, borderW)

propBoxSizingContentBox :: Property
propBoxSizingContentBox = forAll gen $ \(w, pad, borderW) ->
  let childStyle = defaultStyle
        & setWidth (DimPoints w)
        & setPaddingAll (ValPoints pad)
        & setBorderAll borderW
      root = withChildren [node childStyle] (node defaultStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 500) (DimPoints 100)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, w1, _) = layoutBounds l1
          expected = w + pad * 2 + borderW * 2
      in approxEq w1 expected
  where
    gen = do
      w <- chooseFloat (10, 200)
      pad <- chooseFloat (0, 20)
      borderW <- chooseFloat (0, 10)
      pure (w, pad, borderW)

propStretchMaxHeight :: Property
propStretchMaxHeight = forAll gen $ \(rootH, maxH) ->
  let childStyle = defaultStyle
        & setWidth (DimPoints 20)
        & setMaxHeight (DimPoints maxH)
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignStretch
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 100) (DimPoints rootH)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, _, h1) = layoutBounds l1
      in counterexample ("expected height <= " ++ show maxH ++ ", got " ++ show h1) (h1 <= maxH + 0.01)
  where
    gen = do
      rootH <- chooseFloat (50, 200)
      maxH <- chooseFloat (10, 80)
      pure (rootH, maxH)

propMinContentContainerWidth :: Property
propMinContentContainerWidth = forAll gen $ \ws ->
  let childStyle w = defaultStyle & setSize (DimPoints w) (DimPoints 10)
      kids = map (node . childStyle) ws
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setFlexWrap Wrap
        & setWidth DimMinContent
      root = withChildren kids (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size DimUndefined DimUndefined) root
      (_, _, w0, _) = layoutBounds layoutRoot
      expected = maximum ws
  in approxEq w0 expected
  where
    gen = do
      count <- choose (1, 5)
      vectorOf count (chooseFloat (10, 120))

propMaxContentContainerWidth :: Property
propMaxContentContainerWidth = forAll gen $ \ws ->
  let childStyle w = defaultStyle & setSize (DimPoints w) (DimPoints 10)
      kids = map (node . childStyle) ws
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setFlexWrap Wrap
        & setWidth DimMaxContent
      root = withChildren kids (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size DimUndefined DimUndefined) root
      (_, _, w0, _) = layoutBounds layoutRoot
      expected = sum ws
  in approxEq w0 expected
  where
    gen = do
      count <- choose (1, 5)
      vectorOf count (chooseFloat (10, 120))

propBaselineAlignsEqual :: Property
propBaselineAlignsEqual = forAll gen $ \(h1, h2, base) ->
  let measureFn h _ = MeasureOutput 20 h (Just base)
      c1 = withMeasure (measureFn h1) (node defaultStyle)
      c2 = withMeasure (measureFn h2) (node defaultStyle)
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignBaseline
      root = withChildren [c1, c2] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 200) DimUndefined) root
  in expectChildren2 layoutRoot $ \l1 l2 ->
      let (_, y1, _, _) = layoutBounds l1
          (_, y2, _, _) = layoutBounds l2
      in approxEq y1 y2
  where
    gen = do
      h1 <- chooseFloat (10, 80)
      h2 <- chooseFloat (10, 80)
      base <- chooseFloat (0, min h1 h2)
      pure (h1, h2, base)

propAutoMarginsCrossAxisCenter :: Property
propAutoMarginsCrossAxisCenter = forAll gen $ \(ch, h) ->
  let childStyle = defaultStyle
        & setSize (DimPoints 20) (DimPoints h)
        & setMarginLTRB (ValPoints 0) ValAuto (ValPoints 0) ValAuto
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignFlexStart
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 100) (DimPoints ch)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, y1, _, _) = layoutBounds l1
          expected = (ch - h) / 2
      in approxEq y1 expected
  where
    gen = do
      ch <- chooseFloat (40, 200)
      h <- chooseFloat (10, ch - 10)
      pure (ch, h)

propFlexBasisPercent :: Property
propFlexBasisPercent = forAll gen $ \(cw, p) ->
  let childStyle = defaultStyle
        & setFlexBasis (DimPercent p)
        & setFlexGrow 0
        & setFlexShrink 0
        & setHeight (DimPoints 10)
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignFlexStart
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints 20)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, w1, _) = layoutBounds l1
          expected = cw * p / 100
      in approxEq w1 expected
  where
    gen = do
      cw <- chooseFloat (50, 400)
      p <- chooseFloat (5, 95)
      pure (cw, p)

propJustifySpaceEvenlyPositions :: Property
propJustifySpaceEvenlyPositions = forAll gen $ \(cw, w) ->
  let childStyle = defaultStyle & setSize (DimPoints w) (DimPoints 10)
      c1 = node childStyle
      c2 = node childStyle
      c3 = node childStyle
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setJustifyContent JustifySpaceEvenly
        & setAlignItems AlignFlexStart
      root = withChildren [c1, c2, c3] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints 20)) root
  in expectChildren3 layoutRoot $ \l1 l2 l3 ->
      let (x1, _, _, _) = layoutBounds l1
          (x2, _, _, _) = layoutBounds l2
          (x3, _, _, _) = layoutBounds l3
          remSpace = cw - 3 * w
          gap = remSpace / 4
          expected1 = gap
          expected2 = gap + w + gap
          expected3 = expected2 + w + gap
      in approxEq x1 expected1 .&&. approxEq x2 expected2 .&&. approxEq x3 expected3
  where
    gen = do
      w <- chooseFloat (5, 20)
      extra <- chooseFloat (10, 120)
      let cw = 3 * w + extra
      pure (cw, w)

propDirectionInheritanceBorderRtl :: Property
propDirectionInheritanceBorderRtl = forAll gen $ \(startV, endV) ->
  let childStyle = defaultStyle
        & setBorderStartEnd startV endV
        & setSize (DimPoints 20) (DimPoints 10)
      child = node childStyle
      rootStyle = defaultStyle
        & setDirection RTL
        & setFlexDirection Row
      root = withChildren [child] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 100) (DimPoints 20)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let b = layoutBorder l1
      in approxEq (leftE b) endV .&&. approxEq (rightE b) startV
  where
    gen = do
      startV <- chooseFloat (0, 15)
      endV <- chooseFloat (0, 15)
      pure (startV, endV)

propWritingModeStartEndVertical :: Property
propWritingModeStartEndVertical = forAll gen $ \(startV, endV) ->
  let childStyle = defaultStyle
        & setWritingMode VerticalRL
        & setBorderStartEnd startV endV
        & setSize (DimPoints 20) (DimPoints 20)
      child = node childStyle
      rootStyle = defaultStyle & setFlexDirection Row
      root = withChildren [child] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 100) (DimPoints 40)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let b = layoutBorder l1
      in approxEq (topE b) startV .&&. approxEq (bottomE b) endV
  where
    gen = do
      startV <- chooseFloat (0, 15)
      endV <- chooseFloat (0, 15)
      pure (startV, endV)

propAbsoluteAutoSizeMeasure :: Property
propAbsoluteAutoSizeMeasure = forAll gen $ \(w, h) ->
  let measureFn _ = MeasureOutput w h Nothing
      absStyle = defaultStyle
        & setPositionType PositionAbsolute
        & setPositionLTRB (ValPoints 0) (ValPoints 0) ValAuto ValAuto
      absNode = withMeasure measureFn (node absStyle)
      rootStyle = defaultStyle & setFlexDirection Row
      root = withChildren [absNode] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 200) (DimPoints 200)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, _, w1, h1) = layoutBounds l1
      in approxEq w1 w .&&. approxEq h1 h
  where
    gen = do
      w <- chooseFloat (5, 120)
      h <- chooseFloat (5, 120)
      pure (w, h)

propPercentMarginUsesWidth :: Property
propPercentMarginUsesWidth = forAll gen $ \(cw, ch, p) ->
  let childStyle = defaultStyle
        & setSize (DimPoints 10) (DimPoints 10)
        & setMarginLTRB (ValPoints 0) (ValPercent p) (ValPoints 0) (ValPoints 0)
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignFlexStart
      root = withChildren [node childStyle] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints cw) (DimPoints ch)) root
  in expectChildren1 layoutRoot $ \l1 ->
      let (_, y1, _, _) = layoutBounds l1
          expected = cw * p / 100
      in approxEq y1 expected
  where
    gen = do
      cw <- chooseFloat (60, 300)
      ch <- chooseFloat (30, 200)
      p <- chooseFloat (5, 40)
      pure (cw, ch, p)

propBaselineLineContainsTall :: Property
propBaselineLineContainsTall = forAll gen $ \(hBase, hTall, base) ->
  let measureFn _ = MeasureOutput 20 hBase (Just base)
      c1 = withMeasure measureFn (node defaultStyle)
      c2 = node (defaultStyle & setSize (DimPoints 20) (DimPoints hTall) & setAlignSelf AlignFlexStart)
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignBaseline
      root = withChildren [c1, c2] (node rootStyle)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 100) DimUndefined) root
      (_, _, _, rh) = layoutBounds layoutRoot
      expectedMin = max hBase hTall
  in counterexample ("expected root height >= " ++ show expectedMin ++ ", got " ++ show rh) (rh + 0.01 >= expectedMin)
  where
    gen = do
      hBase <- chooseFloat (10, 40)
      hTall <- chooseFloat (30, 120)
      base <- chooseFloat (0, hBase)
      pure (hBase, hTall, base)

propExplicitZeroBorderOverride :: Property
propExplicitZeroBorderOverride = forAll gen $ \(allV, endV) ->
  let rootStyle = defaultStyle
        & setBorderAll allV
        & setBorderStartEnd 0 endV
      root = node rootStyle
      layoutRoot = computeLayout defaultConfig (Size (DimPoints 50) (DimPoints 30)) root
      b = layoutBorder layoutRoot
  in approxEq (leftE b) 0 .&&. approxEq (rightE b) endV
  where
    gen = do
      allV <- chooseFloat (1, 10)
      endV <- chooseFloat (0, 10)
      pure (allV, endV)

layoutBorder :: LayoutNode -> EdgeValues Float
layoutBorder n = L.border (L.layout n)

propRandomTreeFinite :: Property
propRandomTreeFinite = QC.forAllShow genRootAndTree showRootAndTree $ \(rootSize, tree) ->
  let layoutRoot = computeLayout defaultConfig rootSize tree
      values = layoutValues layoutRoot
  in counterexample "non-finite layout value detected" (all isFinite values)

propRandomTreeNonNegative :: Property
propRandomTreeNonNegative = QC.forAllShow genRootAndTree showRootAndTree $ \(rootSize, tree) ->
  let layoutRoot = computeLayout defaultConfig rootSize tree
      sizes = layoutSizes layoutRoot
  in counterexample "negative size detected" (all (>= 0) sizes)

showRootAndTree :: (Size, Node) -> String
showRootAndTree (rootSize, _) = "rootSize=" ++ show rootSize

genRootAndTree :: Gen (Size, Node)
genRootAndTree = do
  rootSize <- genRootSize
  tree <- sized genTreeSized
  pure (rootSize, tree)

genRootSize :: Gen Size
genRootSize = do
  w <- genRootDim
  h <- genRootDim
  pure (Size w h)

genRootDim :: Gen Dimension
genRootDim =
  frequency
    [ (7, DimPoints <$> chooseFloat (10, 600))
    , (3, pure DimUndefined)
    ]

genTreeSized :: Int -> Gen Node
genTreeSized n = genNode (max 1 (min 6 (n `div` 2 + 1)))

genNode :: Int -> Gen Node
genNode depth = do
  style <- genStyle
  childCount <- if depth <= 0 then pure 0 else frequency [(2, pure 0), (8, choose (1, 4))]
  if childCount == 0
    then genLeaf style
    else do
      kids <- vectorOf childCount (genNode (depth - 1))
      pure (withChildren kids (node style))

genLeaf :: Style -> Gen Node
genLeaf style = do
  useMeasure <- frequency [(3, pure True), (7, pure False)]
  if useMeasure
    then do
      mw <- chooseFloat (0, 400)
      mh <- chooseFloat (0, 400)
      let measureFn input =
            let w = case widthMode input of
                  MeasureExactly -> availableWidth input
                  MeasureAtMost -> min mw (availableWidth input)
                  MeasureUndefined -> mw
                h = case heightMode input of
                  MeasureExactly -> availableHeight input
                  MeasureAtMost -> min mh (availableHeight input)
                  MeasureUndefined -> mh
                base = min h (mh / 2)
            in MeasureOutput w h (Just base)
      pure (withMeasure measureFn (node style))
    else pure (node style)

genStyle :: Gen Style
genStyle = do
  dir <- elements [LTR, RTL, Inherit]
  wm <- elements [HorizontalTB, VerticalRL, VerticalLR]
  flexDir <- elements [Row, RowReverse, Column, ColumnReverse]
  wrap <- elements [NoWrap, Wrap, WrapReverse]
  justify <- elements [JustifyFlexStart, JustifyCenter, JustifyFlexEnd, JustifySpaceBetween, JustifySpaceAround, JustifySpaceEvenly]
  alignItems <- elements [AlignFlexStart, AlignCenter, AlignFlexEnd, AlignStretch, AlignBaseline, AlignFirstBaseline, AlignLastBaseline]
  alignSelf <- elements [AlignAuto, AlignFlexStart, AlignCenter, AlignFlexEnd, AlignStretch, AlignBaseline, AlignFirstBaseline, AlignLastBaseline]
  alignContent <- elements [AlignFlexStart, AlignCenter, AlignFlexEnd, AlignStretch, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
  posType <- elements [PositionRelative, PositionAbsolute]
  disp <- frequency [(8, pure DisplayFlex), (1, pure DisplayNone)]
  order <- choose (-2, 2)
  grow <- chooseFloat (0, 5)
  shrinkFactor <- chooseFloat (0, 5)
  basis <- genDim
  box <- elements [ContentBox, BorderBox]
  overflow <- elements [OverflowVisible, OverflowHidden, OverflowScroll, OverflowClip]
  w <- genDim
  h <- genDim
  minW <- genDim
  minH <- genDim
  maxW <- genDim
  maxH <- genDim
  aspect <- frequency [(4, pure Nothing), (1, Just <$> chooseFloat (0.1, 10.0))]
  gapRow <- chooseFloat (0, 40)
  gapCol <- chooseFloat (0, 40)
  padSetter <- genPaddingSetter
  marginSetter <- genMarginSetter
  borderSetter <- genBorderSetter
  posSetter <- genPositionSetter
  let s0 = defaultStyle
      s1 = setDirection dir s0
      s2 = setWritingMode wm s1
      s3 = setFlexDirection flexDir s2
      s4 = setFlexWrap wrap s3
      s5 = setJustifyContent justify s4
      s6 = setAlignItems alignItems s5
      s7 = setAlignSelf alignSelf s6
      s8 = setAlignContent alignContent s7
      s9 = setPositionType posType s8
      s10 = setOrder order s9
      s11 = setFlexGrow grow s10
      s12 = setFlexShrink shrinkFactor s11
      s13 = setFlexBasis basis s12
      s14 = setBoxSizing box s13
      s15 = setOverflow overflow s14
      s16 = setDisplay disp s15
      s17 = setWidth w s16
      s18 = setHeight h s17
      s19 = setMinWidth minW s18
      s20 = setMinHeight minH s19
      s21 = setMaxWidth maxW s20
      s22 = setMaxHeight maxH s21
      s23 = setAspectRatio aspect s22
      s24 = setGaps gapRow gapCol s23
      s25 = padSetter s24
      s26 = marginSetter s25
      s27 = borderSetter s26
      s28 = posSetter s27
  pure s28

genDim :: Gen Dimension
genDim =
  frequency
    [ (4, pure DimAuto)
    , (2, pure DimUndefined)
    , (7, DimPoints <$> chooseFloat (0, 400))
    , (4, DimPercent <$> chooseFloat (0, 200))
    , (1, pure DimMinContent)
    , (1, pure DimMaxContent)
    , (1, pure DimContent)
    , (1, pure (DimFitContent Nothing))
    , (1, DimFitContent . Just <$> chooseFloat (20, 400))
    ]

genValueSigned :: Gen Value
genValueSigned =
  frequency
    [ (2, pure ValAuto)
    , (6, ValPoints <$> chooseFloat (-50, 80))
    , (3, ValPercent <$> chooseFloat (-50, 80))
    ]

genValueNonNegative :: Gen Value
genValueNonNegative =
  frequency
    [ (2, pure ValAuto)
    , (7, ValPoints <$> chooseFloat (0, 40))
    , (3, ValPercent <$> chooseFloat (0, 60))
    ]

genPaddingSetter :: Gen (Style -> Style)
genPaddingSetter = do
  useAll <- elements [True, False]
  if useAll
    then do
      v <- genValueNonNegative
      pure (setPaddingAll v)
    else do
      l <- genValueNonNegative
      t <- genValueNonNegative
      r <- genValueNonNegative
      b <- genValueNonNegative
      pure (setPaddingLTRB l t r b)

genMarginSetter :: Gen (Style -> Style)
genMarginSetter = do
  choice <- elements [0 :: Int, 1, 2]
  case choice of
    0 -> do
      v <- genValueSigned
      pure (setMarginAll v)
    1 -> do
      l <- genValueSigned
      t <- genValueSigned
      r <- genValueSigned
      b <- genValueSigned
      pure (setMarginLTRB l t r b)
    _ -> do
      l <- genValueSigned
      r <- genValueSigned
      pure (setMarginLR l r)

genBorderSetter :: Gen (Style -> Style)
genBorderSetter = do
  useAll <- elements [True, False]
  if useAll
    then do
      v <- chooseFloat (0, 8)
      pure (setBorderAll v)
    else do
      startV <- chooseFloat (0, 8)
      endV <- chooseFloat (0, 8)
      pure (setBorderStartEnd startV endV)

genPositionSetter :: Gen (Style -> Style)
genPositionSetter = do
  l <- genValueSigned
  t <- genValueSigned
  r <- genValueSigned
  b <- genValueSigned
  pure (setPositionLTRB l t r b)

layoutValues :: LayoutNode -> [Float]
layoutValues n =
  let l = L.layout n
      edgeVals ev = [leftE ev, topE ev, rightE ev, bottomE ev, startE ev, endE ev, horizontalE ev, verticalE ev, allE ev]
      here =
        [L.left l, L.top l, L.width l, L.height l]
        ++ edgeVals (L.margin l)
        ++ edgeVals (L.padding l)
        ++ edgeVals (L.border l)
  in here ++ concatMap layoutValues (layoutChildren n)

layoutSizes :: LayoutNode -> [Float]
layoutSizes n =
  let l = L.layout n
  in [L.width l, L.height l] ++ concatMap layoutSizes (layoutChildren n)

isFinite :: Float -> Bool
isFinite v = not (isNaN v || isInfinite v)

expectChildren1 :: LayoutNode -> (LayoutNode -> Property) -> Property
expectChildren1 layout f =
  case layoutChildren layout of
    [l1] -> f l1
    kids -> counterexample ("expected 1 child, got " ++ show (length kids)) False

expectChildren2 :: LayoutNode -> (LayoutNode -> LayoutNode -> Property) -> Property
expectChildren2 layout f =
  case layoutChildren layout of
    [l1, l2] -> f l1 l2
    kids -> counterexample ("expected 2 children, got " ++ show (length kids)) False

expectChildren3 :: LayoutNode -> (LayoutNode -> LayoutNode -> LayoutNode -> Property) -> Property
expectChildren3 layout f =
  case layoutChildren layout of
    [l1, l2, l3] -> f l1 l2 l3
    kids -> counterexample ("expected 3 children, got " ++ show (length kids)) False

approxEq :: Float -> Float -> Property
approxEq a b = counterexample ("expected " ++ show b ++ ", got " ++ show a) (abs (a - b) < 0.01)

chooseFloat :: (Float, Float) -> Gen Float
chooseFloat (lo, hi) = realToFrac <$> choose (realToFrac lo :: Double, realToFrac hi :: Double)

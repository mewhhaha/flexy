module Properties
  ( propertyTests
  ) where

import Data.Function ((&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import Flexy
import Flexy.Measure (MeasureOutput(..))

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

module Golden
  ( goldenTests
  ) where

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Numeric (showFFloat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Flexy
import Flexy.Measure (MeasureInput(..), MeasureMode(..), MeasureOutput(..))

-- Render layout tree as deterministic text.
renderLayout :: LayoutNode -> String
renderLayout node = unlines (go 0 node)
  where
    go indent n =
      let (l, t, w, h) = layoutBounds n
          key = fromMaybe "_" (layoutKey n)
          line = replicate (indent * 2) ' ' ++ key ++ " " ++ fmt l ++ " " ++ fmt t ++ " " ++ fmt w ++ " " ++ fmt h
      in line : concatMap (go (indent + 1)) (layoutChildren n)

    fmt v = showFFloat (Just 2) v ""

goldenTests :: TestTree
goldenTests = testGroup "flexy-golden"
  [ golden "basic_row_flex_grow" basicRow
  , golden "wrap_two_lines" wrapTwoLines
  , golden "align_items_center" alignItemsCenter
  , golden "baseline_alignment" baselineAlignment
  , golden "absolute_positioned_child" absoluteChild
  , golden "aspect_ratio_auto" aspectRatioAuto
  , golden "gap_row_column" gapRowColumn
  , golden "column_justify_center" columnJustifyCenter
  , golden "row_reverse_order" rowReverseOrder
  , golden "align_self_end" alignSelfEnd
  , golden "min_max_clamp" minMaxClamp
  , golden "percent_sizes" percentSizes
  , golden "padding_border_origin" paddingBorderOrigin
  , golden "absolute_right_bottom" absoluteRightBottom
  , golden "flex_shrink" flexShrinkCase
  , golden "align_content_space_between" alignContentSpaceBetween
  , golden "align_content_space_around" alignContentSpaceAround
  , golden "align_content_space_evenly" alignContentSpaceEvenly
  , golden "measure_callback" measureCallback
  , golden "gap_column_with_column" gapColumnWithColumn
  , golden "aspect_ratio_from_height" aspectRatioFromHeight
  , golden "wrap_reverse" wrapReverse
  , golden "align_content_single_line_ignored" alignContentSingleLine
  , golden "percent_padding_uses_parent_width" percentPaddingWidth
  , golden "border_start_end_rtl" borderStartEndRtl
  , golden "auto_margin_main_axis" autoMarginMainAxis
  , golden "auto_margin_cross_axis" autoMarginCrossAxis
  , golden "absolute_min_max_clamp" absoluteMinMaxClamp
  , golden "explicit_zero_cross_size_no_stretch" explicitZeroCrossNoStretch
  , golden "minimal_api_smoke" minimalApiSmoke
  , golden "relative_position_offset" relativePositionOffset
  , golden "order_property" orderProperty
  , golden "min_size_auto" minSizeAuto
  , golden "flex_basis_auto_content" flexBasisAutoContent
  , golden "flex_basis_percent" flexBasisPercent
  , golden "absolute_static_position" absoluteStaticPosition
  , golden "box_sizing_content_box" boxSizingContentBox
  , golden "box_sizing_border_box" boxSizingBorderBox
  , golden "flex_item_padding_includes_outer" flexItemPaddingOuter
  , golden "stretch_respects_max" stretchRespectsMax
  , golden "intrinsic_min_max_content" intrinsicMinMaxContent
  , golden "intrinsic_cross_no_stretch" intrinsicCrossNoStretch
  , golden "align_content_stretch_multiline" alignContentStretchMultiline
  , golden "container_min_content_width" containerMinContentWidth
  , golden "container_max_content_width" containerMaxContentWidth
  , golden "overflow_hidden_allows_shrink" overflowHiddenAllowsShrink
  , golden "writing_mode_vertical_rl" writingModeVerticalRL
  , golden "writing_mode_vertical_lr" writingModeVerticalLR
  , golden "fit_content_container_clamp" fitContentContainerClamp
  , golden "justify_space_evenly" justifySpaceEvenly
  , golden "direction_inheritance_rtl" directionInheritanceRtl
  , golden "absolute_auto_size_measure" absoluteAutoSizeMeasure
  , golden "baseline_non_baseline_tall" baselineNonBaselineTall
  , golden "percent_margin_uses_width" percentMarginUsesWidth
  , golden "writing_mode_start_end" writingModeStartEnd
  , golden "explicit_zero_border_override" explicitZeroBorderOverride
  , golden "display_none_ignored" displayNoneIgnored
  ]

  where
    golden name nodeInput =
      let out = renderLayout (computeLayout defaultConfig (rootSize name) nodeInput)
      in goldenVsString name ("test/fixtures/" ++ name ++ ".golden") (pure (LBS.pack out))

    rootSize name =
      case name of
        "basic_row_flex_grow" -> Size (DimPoints 500) (DimPoints 100)
        "wrap_two_lines" -> Size (DimPoints 120) DimUndefined
        "align_items_center" -> Size (DimPoints 200) (DimPoints 100)
        "baseline_alignment" -> Size (DimPoints 200) DimUndefined
        "absolute_positioned_child" -> Size (DimPoints 200) (DimPoints 200)
        "aspect_ratio_auto" -> Size (DimPoints 200) DimUndefined
        "gap_row_column" -> Size (DimPoints 200) DimUndefined
        "column_justify_center" -> Size (DimPoints 100) (DimPoints 200)
        "row_reverse_order" -> Size (DimPoints 100) (DimPoints 20)
        "align_self_end" -> Size (DimPoints 100) (DimPoints 100)
        "min_max_clamp" -> Size (DimPoints 200) (DimPoints 20)
        "percent_sizes" -> Size (DimPoints 200) (DimPoints 100)
        "padding_border_origin" -> Size (DimPoints 200) (DimPoints 100)
        "absolute_right_bottom" -> Size (DimPoints 200) (DimPoints 200)
        "flex_shrink" -> Size (DimPoints 100) (DimPoints 20)
        "align_content_space_between" -> Size (DimPoints 100) (DimPoints 100)
        "align_content_space_around" -> Size (DimPoints 120) (DimPoints 100)
        "align_content_space_evenly" -> Size (DimPoints 120) (DimPoints 100)
        "measure_callback" -> Size (DimPoints 200) (DimPoints 100)
        "gap_column_with_column" -> Size (DimPoints 100) (DimPoints 100)
        "aspect_ratio_from_height" -> Size (DimPoints 200) (DimPoints 200)
        "wrap_reverse" -> Size (DimPoints 120) (DimPoints 40)
        "align_content_single_line_ignored" -> Size (DimPoints 120) (DimPoints 100)
        "percent_padding_uses_parent_width" -> Size (DimPoints 200) (DimPoints 100)
        "border_start_end_rtl" -> Size (DimPoints 100) (DimPoints 40)
        "auto_margin_main_axis" -> Size (DimPoints 100) (DimPoints 40)
        "auto_margin_cross_axis" -> Size (DimPoints 100) (DimPoints 40)
        "absolute_min_max_clamp" -> Size (DimPoints 100) (DimPoints 100)
        "explicit_zero_cross_size_no_stretch" -> Size (DimPoints 100) (DimPoints 20)
        "minimal_api_smoke" -> Size (DimPoints 100) (DimPoints 50)
        "relative_position_offset" -> Size (DimPoints 100) (DimPoints 20)
        "order_property" -> Size (DimPoints 100) (DimPoints 20)
        "min_size_auto" -> Size (DimPoints 100) (DimPoints 20)
        "flex_basis_auto_content" -> Size (DimPoints 120) (DimPoints 20)
        "flex_basis_percent" -> Size (DimPoints 200) (DimPoints 20)
        "absolute_static_position" -> Size (DimPoints 100) (DimPoints 20)
        "box_sizing_content_box" -> Size (DimPoints 100) (DimPoints 40)
        "box_sizing_border_box" -> Size (DimPoints 100) (DimPoints 40)
        "flex_item_padding_includes_outer" -> Size (DimPoints 60) (DimPoints 20)
        "stretch_respects_max" -> Size (DimPoints 100) (DimPoints 100)
        "intrinsic_min_max_content" -> Size (DimPoints 200) (DimPoints 40)
        "intrinsic_cross_no_stretch" -> Size (DimPoints 100) (DimPoints 60)
        "align_content_stretch_multiline" -> Size (DimPoints 50) (DimPoints 90)
        "container_min_content_width" -> Size DimUndefined DimUndefined
        "container_max_content_width" -> Size DimUndefined DimUndefined
        "overflow_hidden_allows_shrink" -> Size (DimPoints 50) (DimPoints 20)
        "writing_mode_vertical_rl" -> Size (DimPoints 100) (DimPoints 60)
        "writing_mode_vertical_lr" -> Size (DimPoints 100) (DimPoints 60)
        "fit_content_container_clamp" -> Size (DimPoints 100) (DimPoints 40)
        "justify_space_evenly" -> Size (DimPoints 100) (DimPoints 20)
        "direction_inheritance_rtl" -> Size (DimPoints 100) (DimPoints 50)
        "absolute_auto_size_measure" -> Size (DimPoints 200) (DimPoints 200)
        "baseline_non_baseline_tall" -> Size (DimPoints 100) DimUndefined
        "percent_margin_uses_width" -> Size (DimPoints 200) (DimPoints 100)
        "writing_mode_start_end" -> Size (DimPoints 100) (DimPoints 100)
        "explicit_zero_border_override" -> Size (DimPoints 50) (DimPoints 30)
        "display_none_ignored" -> Size (DimPoints 100) (DimPoints 20)
        _ -> Size DimUndefined DimUndefined

    basicRow =
      let childStyle = defaultStyle
            & setFlexGrow 1
            & setFlexBasis (DimPoints 0)
            & setHeight (DimPoints 100)
          a = withKey "a" (node childStyle)
          b = withKey "b" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [a, b] root

    wrapTwoLines =
      let childStyle = defaultStyle & setSize (DimPoints 50) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setAlignContent AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    alignItemsCenter =
      let childStyle = defaultStyle & setSize (DimPoints 50) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignCenter
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    baselineAlignment =
      let c1Style = defaultStyle & setSize (DimPoints 50) (DimPoints 20)
          c2Style = defaultStyle & setSize (DimPoints 50) (DimPoints 40)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignBaseline
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    absoluteChild =
      let absStyle = defaultStyle
            & setPositionType PositionAbsolute
            & setSize (DimPoints 50) (DimPoints 60)
            & setPositionLTRB (ValPoints 10) (ValPoints 20) ValAuto ValAuto
          absNode = withKey "abs" (node absStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [absNode] root

    aspectRatioAuto =
      let childStyle = defaultStyle
            & setWidth (DimPoints 100)
            & setAspectRatio (Just 2.0)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    gapRowColumn =
      let childStyle = defaultStyle & setSize (DimPoints 50) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setGaps 10 5
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    columnJustifyCenter =
      let childStyle = defaultStyle & setSize (DimPoints 50) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Column
            & setJustifyContent JustifyCenter
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    rowReverseOrder =
      let childStyle = defaultStyle & setSize (DimPoints 30) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection RowReverse
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    alignSelfEnd =
      let c1Style = defaultStyle
            & setSize (DimPoints 30) (DimPoints 20)
            & setAlignSelf AlignFlexEnd
          c2Style = defaultStyle & setSize (DimPoints 30) (DimPoints 60)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    minMaxClamp =
      let c1Style = defaultStyle
            & setFlexGrow 1
            & setFlexBasis (DimPoints 0)
            & setMinWidth (DimPoints 120)
            & setHeight (DimPoints 20)
          c2Style = defaultStyle
            & setFlexGrow 1
            & setFlexBasis (DimPoints 0)
            & setMaxWidth (DimPoints 50)
            & setHeight (DimPoints 20)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    percentSizes =
      let childStyle = defaultStyle & setSize (DimPercent 50) (DimPercent 25)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    paddingBorderOrigin =
      let childStyle = defaultStyle & setSize (DimPoints 50) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setPaddingAll (ValPoints 10)
            & setBorderAll 2
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    absoluteRightBottom =
      let absStyle = defaultStyle
            & setPositionType PositionAbsolute
            & setSize (DimPoints 40) (DimPoints 30)
            & setPositionLTRB ValAuto ValAuto (ValPoints 10) (ValPoints 20)
          absNode = withKey "abs" (node absStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [absNode] root

    flexShrinkCase =
      let childStyle = defaultStyle & setSize (DimPoints 80) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    alignContentSpaceBetween =
      let childStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setAlignContent AlignSpaceBetween
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    alignContentSpaceAround =
      let childStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setAlignContent AlignSpaceAround
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    alignContentSpaceEvenly =
      let childStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setAlignContent AlignSpaceEvenly
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    measureCallback =
      let measureFn _ = MeasureOutput 80 30 Nothing
          child = withKey "m1" (withMeasure measureFn (node defaultStyle))
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    gapColumnWithColumn =
      let childStyle = defaultStyle & setSize (DimPoints 40) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Column
            & setGaps 10 0
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    aspectRatioFromHeight =
      let childStyle = defaultStyle
            & setHeight (DimPoints 40)
            & setAspectRatio (Just 2.0)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Column
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    wrapReverse =
      let childStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap WrapReverse
            & setAlignContent AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    alignContentSingleLine =
      let childStyle = defaultStyle & setSize (DimPoints 40) (DimPoints 20)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setAlignContent AlignSpaceBetween
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    percentPaddingWidth =
      let childStyle = defaultStyle & setSize (DimPoints 50) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle & setPaddingAll (ValPercent 10)
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    borderStartEndRtl =
      let childStyle = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle
            & setDirection RTL
            & setBorderStartEnd 2 4
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    autoMarginMainAxis =
      let childStyle = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setMarginLR ValAuto ValAuto
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    autoMarginCrossAxis =
      let childStyle = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setMarginLTRB (ValPoints 0) ValAuto (ValPoints 0) ValAuto
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    absoluteMinMaxClamp =
      let absStyle = defaultStyle
            & setPositionType PositionAbsolute
            & setSize (DimPoints 10) (DimPoints 10)
            & setMinWidth (DimPoints 30)
            & setMinHeight (DimPoints 20)
            & setPositionLTRB (ValPoints 0) (ValPoints 0) ValAuto ValAuto
          absNode = withKey "abs" (node absStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [absNode] root

    explicitZeroCrossNoStretch =
      let c1Style = defaultStyle & setSize (DimPoints 20) (DimPoints 0)
          c2Style = defaultStyle & setSize (DimPoints 20) (DimPoints 20)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignStretch
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    minimalApiSmoke =
      let childStyle = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1] root

    relativePositionOffset =
      let c1Style = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setPositionType PositionRelative
            & setPositionLTRB (ValPoints 10) (ValPoints 5) ValAuto ValAuto
          c2Style = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    orderProperty =
      let c1Style = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setOrder 2
          c2Style = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setOrder (-1)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    minSizeAuto =
      let measureFn _ = MeasureOutput 80 10 Nothing
          c1 = withKey "c1" (withMeasure measureFn (node defaultStyle))
          c2 = withKey "c2" (withMeasure measureFn (node defaultStyle))
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    flexBasisAutoContent =
      let innerChildStyle = defaultStyle & setSize (DimPoints 30) (DimPoints 10)
          inner1 = withKey "i1" (node innerChildStyle)
          inner2 = withKey "i2" (node innerChildStyle)
          flexItemStyle = defaultStyle & setFlexDirection Row
          flexItem = withKey "c1" (withChildren [inner1, inner2] (node flexItemStyle))
          sibling = withKey "c2" (node (defaultStyle & setSize (DimPoints 20) (DimPoints 10)))
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [flexItem, sibling] root

    flexBasisPercent =
      let c1Style = defaultStyle
            & setFlexBasis (DimPercent 50)
            & setFlexGrow 0
            & setFlexShrink 0
            & setHeight (DimPoints 10)
          c2Style = defaultStyle
            & setFlexBasis (DimPercent 25)
            & setFlexGrow 0
            & setFlexShrink 0
            & setHeight (DimPoints 10)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    absoluteStaticPosition =
      let n1 = withKey "n1" (node (defaultStyle & setSize (DimPoints 20) (DimPoints 10)))
          absStyle = defaultStyle
            & setPositionType PositionAbsolute
            & setSize (DimPoints 20) (DimPoints 10)
          absNode = withKey "abs" (node absStyle)
          n2 = withKey "n2" (node (defaultStyle & setSize (DimPoints 20) (DimPoints 10)))
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [n1, absNode, n2] root

    boxSizingContentBox =
      let childStyle = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setPaddingAll (ValPoints 5)
            & setBorderAll 2
          child = withKey "child" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    boxSizingBorderBox =
      let childStyle = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setPaddingAll (ValPoints 5)
            & setBorderAll 2
            & setBoxSizing BorderBox
          child = withKey "child" (node childStyle)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    flexItemPaddingOuter =
      let c1Style = defaultStyle
            & setSize (DimPoints 20) (DimPoints 10)
            & setPaddingLTRB (ValPoints 10) (ValPoints 0) (ValPoints 10) (ValPoints 0)
          c2Style = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    stretchRespectsMax =
      let c1Style = defaultStyle
            & setWidth (DimPoints 20)
            & setMaxHeight (DimPoints 30)
          c2Style = defaultStyle & setWidth (DimPoints 20)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignStretch
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    intrinsicMinMaxContent =
      let measureFn input =
            let w = case widthMode input of
                  MeasureAtMost -> 30
                  MeasureUndefined -> 80
                  MeasureExactly -> availableWidth input
                h = case heightMode input of
                  MeasureAtMost -> 20
                  MeasureUndefined -> 20
                  MeasureExactly -> availableHeight input
            in MeasureOutput w h Nothing
          c1Style = defaultStyle & setWidth DimMinContent
          c2Style = defaultStyle & setWidth DimMaxContent
          c1 = withKey "c1" (withMeasure measureFn (node c1Style))
          c2 = withKey "c2" (withMeasure measureFn (node c2Style))
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    intrinsicCrossNoStretch =
      let measureFn input =
            let h = case heightMode input of
                  MeasureUndefined -> 25
                  MeasureAtMost -> 25
                  MeasureExactly -> availableHeight input
                w = case widthMode input of
                  MeasureExactly -> availableWidth input
                  _ -> 20
            in MeasureOutput w h Nothing
          childStyle = defaultStyle
            & setWidth (DimPoints 20)
            & setHeight DimMaxContent
          child = withKey "c1" (withMeasure measureFn (node childStyle))
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignStretch
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    alignContentStretchMultiline =
      let measureFn _ = MeasureOutput 40 10 Nothing
          childStyle = defaultStyle & setWidth (DimPoints 40)
          c1 = withKey "c1" (withMeasure measureFn (node childStyle))
          c2 = withKey "c2" (withMeasure measureFn (node childStyle))
          c3 = withKey "c3" (withMeasure measureFn (node childStyle))
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setAlignContent AlignStretch
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    containerMinContentWidth =
      let childStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setWidth DimMinContent
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    containerMaxContentWidth =
      let childStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setWidth DimMaxContent
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    overflowHiddenAllowsShrink =
      let measureFn _ = MeasureOutput 80 10 Nothing
          childStyle = defaultStyle
            & setOverflow OverflowHidden
          c1 = withKey "c1" (withMeasure measureFn (node childStyle))
          c2 = withKey "c2" (withMeasure measureFn (node childStyle))
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    writingModeVerticalRL =
      let childStyle = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle
            & setWritingMode VerticalRL
            & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    writingModeVerticalLR =
      let childStyle = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          rootStyle = defaultStyle
            & setWritingMode VerticalLR
            & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    fitContentContainerClamp =
      let innerChildStyle = defaultStyle & setSize (DimPoints 60) (DimPoints 10)
          i1 = withKey "i1" (node innerChildStyle)
          i2 = withKey "i2" (node innerChildStyle)
          containerStyle = defaultStyle
            & setFlexDirection Row
            & setFlexWrap Wrap
            & setWidth (DimFitContent Nothing)
          container = withKey "c1" (withChildren [i1, i2] (node containerStyle))
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [container] root

    justifySpaceEvenly =
      let childStyle = defaultStyle & setSize (DimPoints 10) (DimPoints 10)
          c1 = withKey "c1" (node childStyle)
          c2 = withKey "c2" (node childStyle)
          c3 = withKey "c3" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setJustifyContent JustifySpaceEvenly
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2, c3] root

    directionInheritanceRtl =
      let grandStyle = defaultStyle & setSize (DimPoints 10) (DimPoints 10)
          grand = withKey "grand" (node grandStyle)
          childStyle = defaultStyle
            & setBorderStartEnd 10 2
            & setSize (DimPoints 40) (DimPoints 20)
          child = withKey "child" (withChildren [grand] (node childStyle))
          rootStyle = defaultStyle
            & setDirection RTL
            & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    absoluteAutoSizeMeasure =
      let measureFn _ = MeasureOutput 40 30 Nothing
          absStyle = defaultStyle
            & setPositionType PositionAbsolute
            & setPositionLTRB (ValPoints 0) (ValPoints 0) ValAuto ValAuto
          absNode = withKey "abs" (withMeasure measureFn (node absStyle))
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [absNode] root

    baselineNonBaselineTall =
      let c1Style = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c2Style = defaultStyle
            & setSize (DimPoints 20) (DimPoints 50)
            & setAlignSelf AlignFlexStart
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignBaseline
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

    percentMarginUsesWidth =
      let childStyle = defaultStyle
            & setSize (DimPoints 20) (DimPoints 20)
            & setMarginLTRB (ValPoints 0) (ValPercent 10) (ValPoints 0) (ValPoints 0)
          child = withKey "c1" (node childStyle)
          rootStyle = defaultStyle
            & setFlexDirection Row
            & setAlignItems AlignFlexStart
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    writingModeStartEnd =
      let grandStyle = defaultStyle & setSize (DimPoints 10) (DimPoints 10)
          grand = withKey "grand" (node grandStyle)
          childStyle = defaultStyle
            & setWritingMode VerticalRL
            & setBorderStartEnd 7 1
            & setSize (DimPoints 30) (DimPoints 30)
          child = withKey "child" (withChildren [grand] (node childStyle))
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    explicitZeroBorderOverride =
      let childStyle = defaultStyle & setSize (DimPoints 10) (DimPoints 10)
          child = withKey "child" (node childStyle)
          rootStyle = defaultStyle
            & setBorderAll 5
            & setBorderStartEnd 0 5
          root = withKey "root" (node rootStyle)
      in withChildren [child] root

    displayNoneIgnored =
      let c1Style = defaultStyle
            & setDisplay DisplayNone
            & setSize (DimPoints 50) (DimPoints 10)
          c2Style = defaultStyle & setSize (DimPoints 20) (DimPoints 10)
          c1 = withKey "c1" (node c1Style)
          c2 = withKey "c2" (node c2Style)
          rootStyle = defaultStyle & setFlexDirection Row
          root = withKey "root" (node rootStyle)
      in withChildren [c1, c2] root

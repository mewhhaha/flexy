module Properties (propertyTests) where

import Data.Foldable (toList)
import Test.QuickCheck (Positive(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC

import Flexy

data LayoutSpec
  = FixedLeaf !Float !Float
  | MeasuredLeaf !Float !Float
  | Container !Direction !Wrap !Justify !Align !Float !Float [LayoutSpec]
  | Flexed !Float !Float !Float !Float LayoutSpec
  deriving (Show)

instance QC.Arbitrary LayoutSpec where
  arbitrary = QC.sized (generateLayoutSpec . min 4)

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "grow fills the content width" growFillsWidth
  , QC.testProperty "shrink never produces negative widths" shrinkIsNonNegative
  , QC.testProperty "percentages resolve against parent content" percentageUsesContent
  , QC.testProperty "measurement follows the flex-assigned width" measurementFollowsAssignedWidth
  , QC.testProperty "nested intrinsic height follows assigned child widths" nestedHeightFollowsChildWidths
  , QC.testProperty "generated rectangles have finite coordinates and non-negative sizes" geometryIsValid
  , QC.testProperty "layout preserves tree values in traversal order" valuesArePreserved
  , QC.testProperty "style composition is associative" styleCompositionIsAssociative
  , QC.testProperty "empty styles are a composition identity" emptyStyleIsIdentity
  , QC.testProperty "a wide sibling list lays out completely" wideTreeCompletes
  , QC.testProperty "deep nesting lays out completely" deepTreeCompletes
  ]

growFillsWidth :: Positive Float -> Positive Float -> Positive Float -> QC.Property
growFillsWidth (Positive rawWidth) (Positive rawFirstGrow) (Positive rawSecondGrow) =
  QC.counterexample ("child widths were " <> show childWidths) $
    approximately (sum childWidths) viewportWidth
  where
    viewportWidth = bounded 1 10000 rawWidth
    firstGrow = bounded 0.01 100 rawFirstGrow
    secondGrow = bounded 0.01 100 rawSecondGrow
    child factor = styled (grow factor <> basis (Points 0)) (leaf ())
    tree = layout (Size viewportWidth 20) (row () [child firstGrow, child secondGrow])
    childWidths = map (rectWidth . bounds) (children tree)

shrinkIsNonNegative :: Positive Float -> Positive Float -> QC.Property
shrinkIsNonNegative (Positive rawFirstWidth) (Positive rawSecondWidth) =
  QC.counterexample ("child widths were " <> show childWidths) $
    QC.conjoin
      [ QC.property (all (>= 0) childWidths)
      , approximately (sum childWidths) 1
      ]
  where
    firstWidth = bounded 1 1000 rawFirstWidth
    secondWidth = bounded 1 1000 rawSecondWidth
    child childWidth = styled (width (Points childWidth)) (leaf ())
    tree = layout (Size 1 10) (row () [child firstWidth, child secondWidth])
    childWidths = map (rectWidth . bounds) (children tree)

percentageUsesContent :: Positive Float -> QC.Property
percentageUsesContent (Positive rawWidth) =
  case children tree of
    [childLayout] -> rectWidth (bounds childLayout) QC.=== contentWidth * 0.25
    actual -> QC.counterexample ("expected one child, got " <> show (length actual)) False
  where
    viewportWidth = bounded 40 10000 rawWidth
    contentWidth = viewportWidth - 20
    child = styled (width (Percent 0.25)) (leaf ())
    root = styled (padding (axisEdges 10 0)) (row () [child])
    tree = layout (Size viewportWidth 10) root

measurementFollowsAssignedWidth :: Positive Float -> QC.Property
measurementFollowsAssignedWidth (Positive rawWidth) =
  QC.conjoin (map crossMatchesMain (children tree))
  where
    viewportWidth = bounded 1 1000 rawWidth
    measureText constraints = Size 1000 (maybe 0 id (availableWidth constraints))
    child = measured measureText ()
    tree = layout (Size viewportWidth 1000) (styled (align AlignStart) (row () [child, child]))
    crossMatchesMain childLayout =
      approximately (rectHeight (bounds childLayout)) (rectWidth (bounds childLayout))

nestedHeightFollowsChildWidths :: Positive Float -> QC.Property
nestedHeightFollowsChildWidths (Positive rawWidth) =
  case children tree of
    [nestedLayout] -> approximately (rectHeight (bounds nestedLayout)) (viewportWidth / 2)
    actual -> QC.counterexample ("expected one nested row, got " <> show (length actual)) False
  where
    viewportWidth = bounded 1 1000 rawWidth
    measureText constraints = Size 1000 (maybe 0 id (availableWidth constraints))
    nested =
      styled (width (Points viewportWidth) <> align AlignStart) $
        row () [measured measureText (), measured measureText ()]
    tree = layout (Size viewportWidth 1000) (styled (align AlignStart) (row () [nested]))

geometryIsValid :: QC.NonNegative Float -> QC.NonNegative Float -> LayoutSpec -> QC.Property
geometryIsValid (QC.NonNegative rawWidth) (QC.NonNegative rawHeight) spec =
  QC.counterexample failure (all valid rectangles)
  where
    viewportWidth = bounded 0 10000 rawWidth
    viewportHeight = bounded 0 10000 rawHeight
    rectangles = map bounds (layouts (layout (Size viewportWidth viewportHeight) (nodeFromSpec spec)))
    failure = "invalid rectangles for " <> show spec <> ": " <> show rectangles
    valid rectangle =
      all finite [rectX rectangle, rectY rectangle]
        && all nonNegativeFinite [rectWidth rectangle, rectHeight rectangle]
    finite number = not (isNaN number || isInfinite number)
    nonNegativeFinite number = finite number && number >= 0

valuesArePreserved :: [Int] -> QC.Property
valuesArePreserved values =
  toList result QC.=== 0 : values
  where
    node = row 0 (map leaf values)
    result = layout (Size 100 10) node

styleCompositionIsAssociative :: Float -> Float -> Float -> QC.Property
styleCompositionIsAssociative first second third =
  ((width (Points first) <> height (Points second)) <> width (Points third))
    QC.=== (width (Points first) <> (height (Points second) <> width (Points third)))

emptyStyleIsIdentity :: Float -> QC.Property
emptyStyleIsIdentity rawWidth =
  QC.conjoin [mempty <> style QC.=== style, style <> mempty QC.=== style]
  where
    style = width (Points rawWidth) <> grow 2 <> align AlignCenter

wideTreeCompletes :: QC.Property
wideTreeCompletes = QC.once (length (children result) QC.=== siblingCount)
  where
    siblingCount = 10000
    result = layout (Size (fromIntegral siblingCount) 1) (row () (replicate siblingCount (sized (Size 1 1) ())))

deepTreeCompletes :: QC.Property
deepTreeCompletes = QC.once (length (layouts result) QC.=== nestingDepth + 1)
  where
    nestingDepth = 100
    nested 0 = sized (Size 1 1) ()
    nested depth = styled (align AlignStart) (row () [nested (depth - 1)])
    result = layout (Size 100 100) (nested nestingDepth)

layouts :: Layout a -> [Layout a]
layouts tree = tree : concatMap layouts (children tree)

bounded :: Float -> Float -> Float -> Float
bounded lower upper = max lower . min upper

approximately :: Float -> Float -> QC.Property
approximately actual expected =
  QC.counterexample ("expected " <> show expected <> ", got " <> show actual) $
    abs (actual - expected) <= 0.0001 * max 1 (abs expected)

generateLayoutSpec :: Int -> QC.Gen LayoutSpec
generateLayoutSpec depth
  | depth <= 0 = generateLeafSpec
  | otherwise =
      QC.frequency
        [ (4, generateLeafSpec)
        , (5, generateContainerSpec depth)
        , (2, generateFlexedSpec depth)
        ]

generateLeafSpec :: QC.Gen LayoutSpec
generateLeafSpec = do
  intrinsicWidth <- QC.choose (0, 200)
  intrinsicHeight <- QC.choose (0, 200)
  QC.oneof
    [ pure (FixedLeaf intrinsicWidth intrinsicHeight)
    , pure (MeasuredLeaf intrinsicWidth intrinsicHeight)
    ]

generateContainerSpec :: Int -> QC.Gen LayoutSpec
generateContainerSpec depth = do
  direction' <- QC.elements [Row, RowReverse, Column, ColumnReverse]
  wrap' <- QC.elements [NoWrap, Wrap]
  justification <- QC.elements [Start, Center, End, SpaceBetween, SpaceAround, SpaceEvenly]
  alignment <- QC.elements [AlignStart, AlignCenter, AlignEnd, Stretch]
  gap' <- QC.choose (0, 20)
  padding' <- QC.choose (0, 20)
  childCount <- QC.chooseInt (0, 4)
  childSpecs <- QC.vectorOf childCount (generateLayoutSpec (depth - 1))
  pure (Container direction' wrap' justification alignment gap' padding' childSpecs)

generateFlexedSpec :: Int -> QC.Gen LayoutSpec
generateFlexedSpec depth = do
  growFactor <- QC.choose (0, 4)
  shrinkFactor <- QC.choose (0, 4)
  basisSize <- QC.choose (0, 200)
  marginSize <- QC.choose (-20, 20)
  childSpec <- generateLayoutSpec (depth - 1)
  pure (Flexed growFactor shrinkFactor basisSize marginSize childSpec)

nodeFromSpec :: LayoutSpec -> Node ()
nodeFromSpec spec =
  case spec of
    FixedLeaf intrinsicWidth intrinsicHeight -> sized (Size intrinsicWidth intrinsicHeight) ()
    MeasuredLeaf naturalWidth lineHeight -> measured (measure naturalWidth lineHeight) ()
    Container direction' wrap' justification alignment gap' padding' childSpecs ->
      styled containerStyle (row () (map nodeFromSpec childSpecs))
      where
        containerStyle =
          direction direction'
            <> wrapping wrap'
            <> justify justification
            <> align alignment
            <> gap gap'
            <> padding (allEdges padding')
    Flexed growFactor shrinkFactor basisSize marginSize childSpec ->
      styled
        (flex growFactor shrinkFactor (Points basisSize) <> margin (allEdges marginSize))
        (nodeFromSpec childSpec)
  where
    measure naturalWidth lineHeight constraints =
      Size assignedWidth (lineHeight * lineCount)
      where
        assignedWidth = maybe naturalWidth (max 1 . min naturalWidth) (availableWidth constraints)
        lineCount = fromIntegral (ceiling (naturalWidth / assignedWidth) :: Int)

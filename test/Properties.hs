module Properties (propertyTests) where

import Data.Foldable (toList)
import Test.QuickCheck (Positive(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC

import Flexy

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "grow fills the content width" growFillsWidth
  , QC.testProperty "shrink never produces negative widths" shrinkIsNonNegative
  , QC.testProperty "percentages resolve against parent content" percentageUsesContent
  , QC.testProperty "measurement follows the flex-assigned width" measurementFollowsAssignedWidth
  , QC.testProperty "nested intrinsic height follows assigned child widths" nestedHeightFollowsChildWidths
  , QC.testProperty "all generated geometry is finite and non-negative" geometryIsValid
  , QC.testProperty "layout preserves tree values in traversal order" valuesArePreserved
  , QC.testProperty "style composition is associative" styleCompositionIsAssociative
  , QC.testProperty "empty styles are a composition identity" emptyStyleIsIdentity
  , QC.testProperty "a wide sibling list lays out completely" wideTreeCompletes
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

geometryIsValid :: QC.NonNegative Float -> QC.NonNegative Float -> QC.Property
geometryIsValid (QC.NonNegative rawWidth) (QC.NonNegative rawHeight) =
  QC.counterexample ("invalid rectangles: " <> show rectangles) (all valid rectangles)
  where
    viewportWidth = bounded 0 10000 rawWidth
    viewportHeight = bounded 0 10000 rawHeight
    node =
      styled (padding (allEdges 3) <> gap 2 <> wrapping Wrap) $
        row ()
          [ styled (flex 1 1 (Points 20) <> height (Percent 0.5)) (leaf ())
          , styled (flex 2 1 (Points 40) <> minWidth (Points 5)) (leaf ())
          , styled (width (Points 30) <> height (Points 7)) (leaf ())
          ]
    rectangles = map bounds (layouts (layout (Size viewportWidth viewportHeight) node))
    valid rectangle = all validNumber [rectX rectangle, rectY rectangle, rectWidth rectangle, rectHeight rectangle]
    validNumber number = not (isNaN number || isInfinite number) && number >= 0

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

layouts :: Layout a -> [Layout a]
layouts tree = tree : concatMap layouts (children tree)

bounded :: Float -> Float -> Float -> Float
bounded lower upper = max lower . min upper

approximately :: Float -> Float -> QC.Property
approximately actual expected =
  QC.counterexample ("expected " <> show expected <> ", got " <> show actual) $
    abs (actual - expected) <= 0.0001 * max 1 (abs expected)

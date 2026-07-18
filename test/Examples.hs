module Examples (exampleTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC

import Flexy

exampleTests :: TestTree
exampleTests = testGroup "examples"
  [ example "styles compose from left to right" stylesCompose
  , example "grow divides remaining space by factor" growDividesSpace
  , example "shrink divides overflow by weighted basis" shrinkDividesOverflow
  , example "minimum sizes freeze before remaining space is divided" minimumFreezes
  , example "maximum sizes freeze before remaining space is divided" maximumFreezes
  , example "padding, gaps, and alignment position a column" columnGeometry
  , example "margins offset a child on both axes" marginsOffsetChild
  , example "wrapping starts a new flex line" wrappingStartsLine
  , example "space-evenly includes both outer spaces" spaceEvenly
  , example "row-reverse positions children from the right" rowReverseGeometry
  , example "column-reverse positions children from the bottom" columnReverseGeometry
  , example "measurements receive the parent content constraint" measurementConstraint
  , example "nested bounds use absolute coordinates" nestedCoordinates
  , example "functor preserves geometry while changing values" functorPreservesGeometry
  ]

example :: String -> QC.Property -> TestTree
example name = QC.testProperty name . QC.once

stylesCompose :: QC.Property
stylesCompose =
  childBounds tree QC.=== Just (Rect 0 0 30 40)
  where
    child = styled (width (Points 10) <> width (Points 30) <> height (Points 40)) (leaf ())
    tree = layout (Size 100 40) (row () [child])

growDividesSpace :: QC.Property
growDividesSpace =
  map bounds (children tree) QC.=== [Rect 0 0 75 20, Rect 75 0 225 20]
  where
    first = styled (flex 1 1 (Points 0)) (leaf "first")
    second = styled (flex 3 1 (Points 0)) (leaf "second")
    tree = layout (Size 300 20) (row "root" [first, second])

shrinkDividesOverflow :: QC.Property
shrinkDividesOverflow =
  map bounds (children tree) QC.=== [Rect 0 0 60 20, Rect 60 0 40 20]
  where
    first = styled (width (Points 120) <> height (Points 20)) (leaf "first")
    second = styled (width (Points 80) <> height (Points 20)) (leaf "second")
    tree = layout (Size 100 20) (row "root" [first, second])

minimumFreezes :: QC.Property
minimumFreezes =
  map (rectWidth . bounds) (children tree) QC.=== [80, 20]
  where
    first = styled (width (Points 100) <> minWidth (Points 80)) (leaf "first")
    second = styled (width (Points 100)) (leaf "second")
    tree = layout (Size 100 10) (row "root" [first, second])

maximumFreezes :: QC.Property
maximumFreezes =
  map (rectWidth . bounds) (children tree) QC.=== [20, 80]
  where
    first = styled (grow 1 <> basis (Points 0) <> maxWidth (Points 20)) (leaf "first")
    second = styled (grow 1 <> basis (Points 0)) (leaf "second")
    tree = layout (Size 100 10) (row "root" [first, second])

columnGeometry :: QC.Property
columnGeometry =
  map bounds (children tree) QC.=== [Rect 40 10 20 20, Rect 40 35 20 20]
  where
    box name = styled (width (Points 20) <> height (Points 20)) (leaf name)
    root = styled (padding (edges 10 20 10 20) <> gap 5 <> align AlignCenter) (column "root" [box "a", box "b"])
    tree = layout (Size 100 80) root

marginsOffsetChild :: QC.Property
marginsOffsetChild =
  childBounds tree QC.=== Just (Rect 11 3 20 10)
  where
    child = styled (width (Points 20) <> height (Points 10) <> margin (edges 3 7 5 11)) (leaf ())
    tree = layout (Size 100 30) (styled (align AlignStart) (row () [child]))

wrappingStartsLine :: QC.Property
wrappingStartsLine =
  map bounds (children tree) QC.===
    [ Rect 0 0 40 10
    , Rect 45 0 40 10
    , Rect 0 15 40 10
    ]
  where
    box name = styled (width (Points 40) <> height (Points 10) <> alignSelf AlignStart) (leaf name)
    root = styled (wrapping Wrap <> gap 5 <> align AlignStart) (row "root" [box "a", box "b", box "c"])
    tree = layout (Size 100 20) root

spaceEvenly :: QC.Property
spaceEvenly =
  map bounds (children tree) QC.=== [Rect 20 0 20 10, Rect 60 0 20 10]
  where
    box name = styled (width (Points 20) <> height (Points 10)) (leaf name)
    tree = layout (Size 100 10) (styled (justify SpaceEvenly) (row "root" [box "a", box "b"]))

rowReverseGeometry :: QC.Property
rowReverseGeometry =
  map bounds (children tree) QC.=== [Rect 80 0 20 10, Rect 50 0 20 10]
  where
    box name = styled (width (Points 20) <> height (Points 10)) (leaf name)
    root = styled (direction RowReverse <> gap 10) (row "root" [box "a", box "b"])
    tree = layout (Size 100 10) root

columnReverseGeometry :: QC.Property
columnReverseGeometry =
  map bounds (children tree) QC.=== [Rect 0 80 20 20, Rect 0 50 20 20]
  where
    box name = styled (width (Points 20) <> height (Points 20)) (leaf name)
    root = styled (direction ColumnReverse <> gap 10 <> align AlignStart) (column "root" [box "a", box "b"])
    tree = layout (Size 20 100) root

measurementConstraint :: QC.Property
measurementConstraint =
  childBounds tree QC.=== Just (Rect 10 0 80 10)
  where
    measureText constraints = Size (maybe 200 id (availableWidth constraints)) 10
    child = measured measureText "text"
    root = styled (padding (axisEdges 10 0) <> align AlignStart) (row "root" [child])
    tree = layout (Size 100 20) root

nestedCoordinates :: QC.Property
nestedCoordinates =
  case children tree of
    [bodyLayout] -> childBounds bodyLayout QC.=== Just (Rect 15 12 10 8)
    actual -> QC.counterexample ("expected one body, got " <> show (length actual)) False
  where
    inner = styled (width (Points 10) <> height (Points 8)) (leaf "inner")
    body = styled (padding (allEdges 5)) (row "body" [inner])
    root = styled (padding (edges 7 0 0 10)) (row "root" [body])
    tree = layout (Size 100 50) root

functorPreservesGeometry :: QC.Property
functorPreservesGeometry =
  stripValues original QC.=== stripValues changed
  where
    node = row (1 :: Int) [styled (grow 1) (leaf 2), leaf 3]
    original = layout (Size 100 20) node
    changed = layout (Size 100 20) (show <$> node)

childBounds :: Layout a -> Maybe Rect
childBounds tree = bounds <$> onlyChild (children tree)

onlyChild :: [a] -> Maybe a
onlyChild [single] = Just single
onlyChild _ = Nothing

stripValues :: Layout a -> Layout ()
stripValues = fmap (const ())

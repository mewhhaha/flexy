# Flexy

Flexy is a small, pure Haskell library for flexbox-style layout. It turns an
immutable tree into rectangles and keeps your application values attached to
the result.

The API follows the shape of the problem:

- trees compose from `row`, `column`, and leaves;
- styles compose with `(<>)`;
- `Functor` changes the values without rebuilding the layout tree;
- layout is a deterministic, pure function;
- strict `Float` geometry and a linear flex pass keep the common path cheap.

Flexy deliberately is not a CSS engine. It implements the useful flexbox core:
row and column directions, reverse directions, wrapping, grow and shrink,
minimum and maximum sizes, alignment, justification, gaps, padding, margins,
percentages, and intrinsic measurements.

## Example

```haskell
import Flexy

screen :: Node String
screen =
  styled (padding (allEdges 16) <> gap 12) $
    column "screen"
      [ styled (height (Points 48)) (leaf "header")
      , styled (grow 1 <> gap 12) $
          row "body"
            [ styled (width (Points 220)) (leaf "sidebar")
            , styled (grow 1) (leaf "content")
            ]
      ]

screenLayout :: Layout String
screenLayout = layout (Size 900 600) screen
```

Every node has an application-defined value. After layout, each value sits next
to its absolute bounds:

```haskell
draw :: Layout String -> IO ()
draw box = do
  drawRectangle (value box) (bounds box)
  mapM_ draw (children box)
```

## Composable styles

A `Style` describes only the properties it changes. `mempty` changes nothing,
and the right-hand value wins when properties overlap:

```haskell
card :: Style
card = width (Points 240) <> padding (axisEdges 16 12)

selectedCard :: Style
selectedCard = card <> width (Percent 0.5)
```

`Points` are abstract layout units. Percentages are fractions, so
`Percent 0.5` means fifty percent. Widths and heights describe a node's outer
bounds before margin; padding occupies space inside those bounds.

The empty style uses a row, no wrapping, start justification, stretch
alignment, zero growth, shrink factor one, and zero spacing.

## Intrinsic content

Use `sized` for fixed intrinsic content or `measured` for content such as text:

```haskell
title :: Node String
title = measured measureText "A title"

measureText :: Constraints -> Size
measureText constraints =
  Size (maybe 320 (min 320) (availableWidth constraints)) 24
```

A style width or height overrides the intrinsic dimension. The engine passes
content constraints to measurements, then remeasures the cross size after flex
distribution. This keeps wrapping content consistent with its assigned width.
Non-finite or negative sizes are sanitized at the public boundary.

## Development

```bash
cabal test
just demo  # SDL3 example
```

The package depends only on `base`. Its test suite covers examples and layout
invariants with QuickCheck.

## License

MIT. See [LICENSE](LICENSE).

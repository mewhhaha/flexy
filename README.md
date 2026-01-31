# Flexy

Flexy is a pure Haskell flexbox layout engine inspired by Yoga. It provides a small, functional API for building layout trees, running the flexbox algorithm, and inspecting computed layouts.

## Features

- Pure Haskell implementation (no FFI).
- Flexbox layout with wrapping, gaps, baseline alignment, and auto margins.
- Min/max-content sizing, fit-content, aspect-ratio handling, and box-sizing.
- Measure and baseline callbacks for text or custom nodes.
- Deterministic layout output suitable for testing.

## Status

Flexy targets the core flexbox model and several CSS sizing behaviors. It is not a full CSS engine. Overflow is modeled for auto-min sizing only (no clipping/scrolling). Writing modes are supported for axis resolution, but logical properties beyond that are intentionally minimal.

Breaking changes are acceptable while the API stabilizes.

## Install

Add to your `build-depends` once published:

```cabal
build-depends:
    flexy
```

For local development:

```bash
cabal build
```

## Quick start

```haskell
import Flexy

root :: Node
root =
  let childStyle = defaultStyle
        & setFlexGrow 1
        & setFlexBasis (DimPoints 0)
        & setHeight (DimPoints 24)
      a = node childStyle
      b = node childStyle
      rootStyle = defaultStyle
        & setFlexDirection Row
        & setAlignItems AlignFlexStart
  in withChildren [a, b] (node rootStyle)

layout :: LayoutNode
layout = computeLayout defaultConfig (Size (DimPoints 200) (DimPoints 40)) root
```

Inspect computed bounds:

```haskell
let (x, y, w, h) = layoutBounds layout
```

## API overview

The public API is intentionally small and split across a few modules:

- `Flexy` re-exports the public surface for convenient use.
- `Flexy.Style` defines the `Style` record and setter helpers.
- `Flexy.Node` builds the layout tree and attaches measurement/baseline callbacks.
- `Flexy.Layout` runs layout and inspects results.
- `Flexy.Types` contains the core enums and value types.

### Building nodes

- `node` creates a leaf node from a `Style`.
- `withChildren` attaches child nodes.
- `withMeasure` attaches a custom measurement function.
- `withBaseline` attaches a baseline function.
- `withKey` adds a string identifier used for tests and inspection.

### Measuring

Provide a `MeasureFunc` for text or custom rendering:

```haskell
measureText :: MeasureFunc
measureText (MeasureInput w _ h _) =
  MeasureOutput (max 0 w) (max 0 h) Nothing
```

### Running layout

- `computeLayout` calculates a full layout tree.
- `layoutBounds`, `layoutChildren`, and `layoutKey` help inspect the result.

## Testing

```bash
cabal test
```

The test suite includes golden layout fixtures and property-based tests.

## License

MIT. See `LICENSE`.

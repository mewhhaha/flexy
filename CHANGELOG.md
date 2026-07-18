# Changelog

## Unreleased

- Rebuilt Flexy around immutable, value-carrying `Node` and `Layout` trees.
- Made `Style` a composable `Monoid` with right-biased property overlays.
- Replaced the CSS-shaped API with focused row, column, flex, sizing,
  alignment, spacing, wrapping, and intrinsic-measurement primitives.
- Replaced the original layout kernel with a strict, dependency-free flex
  pass supporting proportional grow and shrink with min/max freezing.
- Reduced the supported public surface to the single `Flexy` module.
- Updated the SDL3 example to the new API and corrected its SDL3 boolean
  initialization result.
- Remeasured intrinsic cross sizes after flex distribution so fixed widths,
  padding, and grow or shrink produce correct content heights.
- Included every wrapped line when determining a nested container's intrinsic
  cross size, using the same flex bases as final placement.

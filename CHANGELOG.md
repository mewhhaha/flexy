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
- Reused final line resolution during intrinsic container sizing so nested
  measured content reflects child shrink and per-line assigned widths.
- Sanitized every emitted rectangle so finite-but-extreme spacing cannot
  overflow child coordinates to non-finite values.
- Preserved center and end alignment when children intentionally overflow,
  while distributed spacing continues to fall back safely.
- Made flex allocation scale-independent and computed weight ratios in wider
  intermediates so small layouts and large finite factors remain accurate.
- Kept flex totals and redistribution in wider intermediates so very large
  bases shrink proportionally without overflowing or losing the target size.
- Made the SDL3 example release SDL, window, and renderer resources when an
  exception interrupts initialization or rendering.
- Made the SDL3 example stop with SDL's error evidence when a hint or rendering
  operation fails instead of continuing after a discarded status.

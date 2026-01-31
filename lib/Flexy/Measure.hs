-- | Measurement types and callbacks for custom content.
module Flexy.Measure
  ( MeasureMode(..)
  , MeasureInput(..)
  , MeasureOutput(..)
  , MeasureFunc
  , BaselineFunc
  ) where

-- | Measurement constraint mode.
data MeasureMode
  = MeasureUndefined -- ^ No constraint.
  | MeasureExactly   -- ^ Must match the provided size exactly.
  | MeasureAtMost    -- ^ Must not exceed the provided size.
  deriving (Eq, Show)

-- | Input to measurement callbacks.
data MeasureInput = MeasureInput
  { availableWidth :: Float -- ^ Width constraint value.
  , widthMode :: MeasureMode -- ^ Width constraint mode.
  , availableHeight :: Float -- ^ Height constraint value.
  , heightMode :: MeasureMode -- ^ Height constraint mode.
  } deriving (Eq, Show)

-- | Output from measurement callbacks.
data MeasureOutput = MeasureOutput
  { measuredWidth :: Float -- ^ Measured width.
  , measuredHeight :: Float -- ^ Measured height.
  , measuredBaseline :: Maybe Float -- ^ Optional baseline offset from the top.
  } deriving (Eq, Show)

-- | Measurement callback used by leaf nodes without children.
type MeasureFunc = MeasureInput -> MeasureOutput

-- | Baseline callback (width -> height -> baseline).
type BaselineFunc = Float -> Float -> Float

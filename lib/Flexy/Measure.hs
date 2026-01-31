module Flexy.Measure
  ( MeasureMode(..)
  , MeasureInput(..)
  , MeasureOutput(..)
  , MeasureFunc
  , BaselineFunc
  ) where

-- | Measurement constraint mode.
data MeasureMode
  = MeasureUndefined
  | MeasureExactly
  | MeasureAtMost
  deriving (Eq, Show)

-- | Input to measurement callbacks.
data MeasureInput = MeasureInput
  { availableWidth :: Float
  , widthMode :: MeasureMode
  , availableHeight :: Float
  , heightMode :: MeasureMode
  } deriving (Eq, Show)

-- | Output from measurement callbacks.
data MeasureOutput = MeasureOutput
  { measuredWidth :: Float
  , measuredHeight :: Float
  , measuredBaseline :: Maybe Float
  } deriving (Eq, Show)

-- | Measurement callback.
type MeasureFunc = MeasureInput -> MeasureOutput

-- | Baseline callback (width -> height -> baseline).
type BaselineFunc = Float -> Float -> Float

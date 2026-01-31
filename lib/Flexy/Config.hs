-- | Layout configuration used by the layout engine.
module Flexy.Config
  ( LayoutConfig(..)
  , defaultConfig
  ) where

-- | Layout configuration for the layout engine.
newtype LayoutConfig = LayoutConfig
  { pointScaleFactor :: Maybe Float -- ^ Optional rounding scale factor.
  } deriving (Eq, Show)

-- | Default configuration with no rounding and no point scaling.
defaultConfig :: LayoutConfig
defaultConfig = LayoutConfig { pointScaleFactor = Nothing }

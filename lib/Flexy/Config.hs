module Flexy.Config
  ( LayoutConfig(..)
  , defaultConfig
  ) where

-- | Layout configuration.
newtype LayoutConfig = LayoutConfig
  { pointScaleFactor :: Maybe Float
  } deriving (Eq, Show)

-- | Default configuration with no rounding.
defaultConfig :: LayoutConfig
defaultConfig = LayoutConfig { pointScaleFactor = Nothing }

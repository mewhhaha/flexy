-- | A small, pure flexbox layout library.
--
-- Styles are values. Combine them with '<>':
--
-- @
-- card = styled (width (Points 240) <> padding (allEdges 16))
-- @
module Flexy
  ( -- * Geometry
    Size(..)
  , Rect(..)
  , Constraints(..)
  , Edges(..)
  , allEdges
  , axisEdges
  , edges

    -- * Styles
  , Style
  , Length(..)
  , Direction(..)
  , Wrap(..)
  , Justify(..)
  , Align(..)
  , width
  , height
  , minWidth
  , minHeight
  , maxWidth
  , maxHeight
  , direction
  , wrapping
  , justify
  , align
  , alignSelf
  , grow
  , shrink
  , basis
  , flex
  , gap
  , padding
  , margin

    -- * Trees
  , Measure
  , Node
  , leaf
  , sized
  , measured
  , row
  , column
  , styled

    -- * Layout
  , Layout(..)
  , layout
  ) where

import Flexy.Core
import Flexy.Internal.Layout (layout)

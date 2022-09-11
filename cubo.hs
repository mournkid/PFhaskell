module Geometria.Cubo (volume, area) where

import qualified Geometria.Cuboide as Cuboide

volume :: Float -> Float
volume lado = Cuboide.volume lado lado lado

area :: Float -> Float
area lado = Cuboide.area lado lado lado


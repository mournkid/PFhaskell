module Geometria.Cuboide (volume, area) where

volume :: Float -> Float -> Float -> Float
volume a b c = retArea a b * c

area :: Float ->Float -> Float -> Float
area a b c = retArea a b * 2 + retArea a c * 2 + retArea c b * 2

retArea :: Float -> Float -> Float
retArea a b = a*b

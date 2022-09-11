module Geometria (volEsfera, areaEsfera,volCubo, areaCubo, volCuboide, areaCuboide) where
import qualified Geometria.Esfera as Esfera
import qualified Geometria.Cuboide as Cuboide
import qualified Geometria.Cubo as Cubo

volEsfera :: Float -> Float
volEsfera raio = (4.0/3.0) * pi * (raio ^ 3)

areaEsfera :: Float -> Float
areaEsfera raio = 4 * pi * (raio ^2)

volCubo :: Float -> Float
volCubo lado = volCuboide lado lado lado

areaCubo :: Float -> Float
areaCubo lado = areaCuboide lado lado lado

volCuboide :: Float -> Float -> Float -> Float
volCuboide a b c = retArea a b * c

areaCuboide :: Float ->Float -> Float -> Float
areaCuboide a b c = retArea a b * 2 + retArea a c * 2 + retArea c b * 2

retArea :: Float -> Float -> Float
retArea a b = a*b





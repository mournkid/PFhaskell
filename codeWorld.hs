{-bola :: Double -> Picture
bola t = dilated (t*0.2) (solidCircle 3)-}

{-roda :: Double -> Picture
roda t = ferro1 & ferro2 & pneu
 where pneu = translated (-10 + 2*t) (0) (rotated (-pi/3 * t)(solidCircle 2)) 
       ferro1 = translated (-10 + 2*t) (0) (rotated (-pi/3*t) (colored gray (solidRectangle 0.5 3.8)))
       ferro2 = translated (-10 + 2*t) (0) (rotated (-pi/3*t) (colored gray (solidRectangle 3.8 0.5)))-}

{-pneu :: Double ->Picture 
pneu t = rotated (pi/9 * t)(solidCircle 2)
  
ferro1 :: Double -> Picture
ferro1 t = rotated (pi/9 * t) (colored gray (solidRectangle 0.5 3.8))
        
ferro2 :: Double -> Picture 
ferro2 t = rotated (pi/9 * t) (colored gray (solidRectangle 3.8 0.5))-}
--boca = curve [(-1,0.8),(0,0),(1.1,1.1),(0,1),(-1,1.1),(-1,0.8)]

import CodeWorld 

main = drawingOf (emoji & coordinatePlane)  

emoji = olho1 & olho2 & boca & sobrancelha1 & sobrancelha2 & cara 
  where cara = coloured (brighter 0.2 yellow) (solidCircle 4) 
        olho1 = translated (-1.7) (0) (solidCircle 1)
        olho2 =  translated (1.7) (0) (solidCircle 1)
        boca = solidClosedCurve[(-1,-2.8),(0,-2.2),(1,-2.7),(0,-2.5),(-1,-2.8)]
        sobrancelha1 = blank
        sobrancelha2 = blank
        
import CodeWorld 

main = drawingOf (emoji & coordinatePlane)  

emoji = olho1 & olho2 & boca & sobrancelha1 & sobrancelha2 & cara 
  where cara = coloured (brighter 0.2 yellow) (solidCircle 4) 
        olho1 = translated (-1.5) (0) (solidCircle 1)
        olho2 =  translated (1.5) (0) (solidCircle 1)
        boca = solidClosedCurve[(-1,-2.8),(0,-2.2),(1,-2.7),(0,-2.5),(-1,-2.8)]
        sobrancelha1 = thickCurve 0.1 [(-3.2,0.8),(-2.5,1),(-1.7,1.7)]
        sobrancelha2 = blank
        

import CodeWorld 

main = drawingOf (emoji & coordinatePlane)  

emoji = sombra1 & sombra2 & olho1 & olho2 & boca & sobrancelha1 & sobrancelha2 & cara 
  where cara = coloured (brighter 0.2 yellow) (solidCircle 4) 
        olho1 = translated (-1.5) (0) (solidCircle 1)
        olho2 =  translated (1.5) (0) (solidCircle 1)
        boca = colored brown (solidClosedCurve[(-1,-2.8),(0,-2.2),(1,-2.7),(0,-2.5),(-1,-2.8)])
        sobrancelha1 = colored brown (thickCurve 0.2 [(-3.2,0.8),(-2.5,1),(-1.7,1.7)])
        sobrancelha2 = colored brown (thickCurve 0.2 [(3.2,0.8),(2.5,1),(1.7,1.7)])
        sombra1 =rotated (pi/7) (translated (-1.4) (0.9) (scaled 1 0.8 (colored white (solidCircle 0.7))))
        sombra2 =rotated (pi/7) (translated (1.3) (-0.4) (scaled 1 0.8 (colored white (solidCircle 0.7))))


{-# LANGUAGE ExtendedDefaultRules #-}

import P5hs
import Prelude hiding ((==),(/==),(>),(<),(>=),(<=))
import qualified Prelude as P

t1_2d = listEnumToFunc [bbbbox (x*100) | x <- map makeValue [0..5]]
    where spd = (makeTidalParam "cps")
          cn x= (200*) $ sin $ (0.05 * ) (spd + (frameCount*0.05)+ x)
          bbox x = do
                  translate 200 200 200
                  fill (255 * (x/25)) (255 * (x/55)) (255 * (x/95))
                  rect (cn x) (cn x) (negate $ cn x) (cn x)
          bbbox x = do
            push
            translate (-120) (-160) (-200)
            bbox x
            pop
          bbbbox x' = do
            push
            translate (-100 + x') (-15) 0
            listEnumToFunc [bbbox (x*5) | x <- map makeValue [0..2]]
            pop



t1_3d = do
  translateX ((lfo' 0.25)*200)
  fill 255 255 255
  rotateX (20 + (makeTidalParam "begin")*40)
  listEnumToFunc (take (20) $ cycle [b2])
    where boxtrain = listEnumToFunc boxes
          boxes = [box' x | x <- fmap makeValue [1..3]]
          box' x = do
            push
            rotateY $ (fcConstant * x)*0.0001
            rotate $ (fcConstant ** 2 * x)*0.000005
            rotateZ $ (fcConstant * x)*0.0001
            rotateX $ (fcConstant * x)*0.001
            translate xshift (90 * (x**2)) (makeJSVar "undefined")
            box 200 0 (200) 200 blank
            circle 0 ((frameCount**0.3 + frameCount*0.2)/200) 100
            fill ((x*10) + 255*(makeTidalParam "begin")) ((x*10) + 255*(makeTidalParam "begin")) (255*(makeTidalParam "begin"))
            torus 200 20 (x**2)
            pop
          fcConstant = ((frameCount) / 5)
          spd = (sin (fcConstant*0.00016))
          xshift = (sin (fcConstant*0.0001))*2
          b2 = do
            boxtrain
            box 200 0 200 200 blank
            translate (-200) (30) 100
            rotateZ 20

t1_loop = do
  translate (-1000) (-500) 0
  foriInRange 0 (i < (mod (frameCount // (mod (frameCount // 800) 8)) 40)) 1 $ do
    rect 200 20 (20*i) (20*(mod i 40))


t1_matrix = do
  matrix [[-1.0,0.0,0.0],[(-8),1,0.0],[0.0,0.0,1.0]]
  matrix [[1.0,0.0,0.0],[(-8),1,0.0],[0.0,0.0,1.0]]
  matrix [[-1.0,0.0,0.0],[(8),1,0.0],[0.0,0.0,1.0]]
  matrix [[-1.0,2.0,0.0],[(8),1,0.0],[0.0,0.0,1.0]]
  matrix [[-1.0,-2.0,0.0],[(8),1,0.0],[0.0,0.0,1.0]]
  matrix [[1.0,2.0,0.0],[(8),1,0.0],[0.0,0.0,1.0]]
  matrix [[1.0,-2.0,0.0],[(8),1,0.0],[0.0,0.0,1.0]]
  push
  translate (100) (-100) 0
  fill ((255*) $ makeTidalParam "pan") 0 0
  matrix [[1.0,0.0,0.0],[(4),1,0.0],[0.0,0.0,1.0]]
  pop
    where lfo x = sin (frameCount * (0.01 * x))
          matrix x = do
            push
            translateY ((lfo 2)*200)
            translateX ((lfo 3)*(-200))
            applymatrix x
            text (makeJSVar "absolutely disgusting tp3") 0 0 blank blank
            pop

t1_text = do
  translate (-1800) (500) (-1000)
  rotateX (3.14159265)
  push
  fill (300* (makeTidalParamFor "s" "808bd" "gain")) (230* (makeTidalParamFor "s" "808bd" "gain")) (230* (makeTidalParamFor "s" "808bd" "gain"))
  listEnumToFunc [hey (x*400) (x/8) | x <- map makeValue [-5..20]]
  pop
  translate (2000) (000) (000)
  rotateY (180)
  rotateX (3.14159265)
    where spd = 0.15
          lfo x = sin (frameCount * (0.01 * x))
          hey y offset = do
            push
            translate 0 y (-50)
            objScale (round' $ abs $ ((10 *)) $ (lfo 0.1)) blank blank
            rotateX (185)
            text (makeJSVar "tp3 -- p5hs") 0 (2) blank blank
            rotateY ((frameCount * spd) + offset)
            pop


main :: IO ()
main = do
  putStrLn "2d example"
  prettyRender t1_2d
  putStrLn "3d example"
  prettyRender t1_3d
  putStrLn "loop example"
  prettyRender t1_loop
  putStrLn "matrix example"
  prettyRender t1_matrix
  putStrLn "text example"
  prettyRender t1_text



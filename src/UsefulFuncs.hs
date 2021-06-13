module UsefulFuncs where

import Exported.ExportedFunctions
import Exported.ExportedVariables
import HigherOrder.P5Bool
import P5Expressions
import P5Render

translateX x = translate x 0 0

translateY y = translate 0 y 0

translateZ :: (Show a, Num a, Renderer a) => ArgEx a -> ListWriter.ListM RenderAble
translateZ z = translate 0 0 z

osc = sin frameCount

lfo' x = sin ((frameCount) * (makeValue (0.0001 * x)))

inrange i' maxi = foriInRange 0 (i' #<= maxi) 1

(//) :: Integral a => a -> a -> a
(//) = div

js string = makeJSVar' string
-- ^ ^ this is for when you wanna quickly call some jsVariable that isn't predefined in this library
--  ie' (2 + frameCount) is the same as (2 + (js "frameCount"))
--  personally, I think this is a bit messier,
--  plus the type JSVar won'd be reflected in any error messages

module P5FunctionSend where

import P5Render
import P5Expressions
import Sound.Tidal.Context
import qualified Data.Map as Map_

changeFunc' stream list = sendFunc' list
  where toEvent' ws we ps pe v = Event (Sound.Tidal.Context.Context []) (Just $ Sound.Tidal.Context.Arc ws we) (Sound.Tidal.Context.Arc ps pe) v
          -- where [ws',we',ps',pe'] = map toRational [ws,we,ps,pe]
        makeFakeMap list_ = Map_.fromList list_
        makeFuncHelp :: [(JavaScript,Value)] -> ControlPattern
        makeFuncHelp y = Pattern $ fakeEvent (makeFakeMap y:: ControlMap)
          where fakeEvent a notARealArgument = [(toEvent' 0 1 0 1) a]
        makeFunc :: [(JavaScript,Value)] -> [ControlPattern]
        makeFunc x = [makeFuncHelp x]
        sendFunc' = mapM_ (streamFirst stream) . makeFunc

changeFunc stream func newFunction = changeFunc' stream list
  where list = [(func, VS (render newFunction))]

resetFunc stream func = changeFunc stream func (makeJSVar "")

makeDraw stream newFunction = changeFunc stream "draw" newFunction

makeLoad stream newFunction = changeFunc stream "load" newFunction

makeImage stream imageURL = do
  changeFunc' stream list
  return $ makeJSVar (removePunc imageURL)
    where varName = removePunc imageURL
          imageURLVar = makeJSVar imageURL
          list = [("imageName",VS varName),("imageURL",VS imageURL)]

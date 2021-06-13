{-# LANGUAGE FlexibleContexts #-}

module Modules.DebugFuncs where

import ListWriter
import Modules.P53D
import Modules.P5Shapes
import Modules.P5Transform
import P5Enviornment
import P5Expressions
import P5Funcs
import P5Render

data Debug a = ConsoleLog (ArgEx a)
  deriving (Show, Eq)

instance (Show a, Renderer a) => Renderer (Debug a) where
  render (ConsoleLog a) = "console.log(eval(" ++ logStr ++ "));"
    where
      logStr = varFunc a

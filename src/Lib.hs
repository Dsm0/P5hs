{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

module Lib where

import Control.Monad.Writer (MonadWriter (..), Writer (..), runWriter, tell)
import Data.List
import Data.List.Split
import qualified Data.Map as Map_
import Data.Typeable
import Exported.ExportedFunctions
import Exported.ExportedVariables
import HigherOrder.P5Bool
import HigherOrder.P5Loops
import ListWriter
import Modules.DebugFuncs
import Modules.P53D
import Modules.P5Attributes
import Modules.P5Color
import Modules.P5Image
import Modules.P5Setting
import Modules.P5Shapes
import Modules.P5Structure
import Modules.P5Text
import Modules.P5Transform
import P5Audio
import P5Enviornment
import P5Expressions
import P5Funcs
import P5JSRenderFuncs
import P5Render
import Sound.Tidal.Context
import Text.Printf
import Turtle
import UsefulFuncs

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

module P5hs (module P) where

import Control.Monad.Writer (MonadWriter (..), Writer (..), runWriter, tell)
import Data.List
import Data.List.Split
import qualified Data.Map as Map_
import Data.Typeable
import Exported.ExportedFunctions as P
import Exported.ExportedVariables as P
import HigherOrder.P5Bool as P
import HigherOrder.P5Loops as P
import ListWriter
import Modules.DebugFuncs as P
import Modules.P53D as P
import Modules.P5Attributes as P
import Modules.P5Color as P
import Modules.P5Image as P
import Modules.P5Setting as P
import Modules.P5Shapes as P
import Modules.P5Structure as P
import Modules.P5Text as P
import Modules.P5Transform as P
import P5Audio as P
import P5Enviornment as P
import P5Expressions as P
import P5Funcs as P
import P5JSRenderFuncs as P
import P5Render as P
import qualified Sound.Tidal.Context as P
import Text.Printf
import Turtle
import UsefulFuncs as P

prettyRender :: (Renderer a) => a -> IO ()
prettyRender = putStrLn . render

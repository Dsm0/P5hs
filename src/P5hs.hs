{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE Trustworthy #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE DeriveFunctor #-}

module P5hs (module P) where

import Data.List
import Text.Printf
import Data.Typeable

import Control.Monad.Writer (Writer(..), runWriter, tell, MonadWriter(..))

import Modules.P5Transform as P5
import Modules.P5Shapes as P5
import Modules.P53D as P5
import Modules.P5Color as P5
import Modules.DebugFuncs as P5
import Modules.P5Structure as P5
import Modules.P5Setting as P5
import Modules.P5Text as P5
import Modules.P5Image as P5
import Modules.P5Attributes as P5

import P5Enviornment as P5
import P5Expressions as P5
import P5Render as P5
import P5JSRenderFuncs as P5
import P5Funcs as P5
import P5Audio as P5
import Exported.ExportedFunctions as P5
import Exported.ExportedVariables as P5
import ListWriter

import HigherOrder.P5Bool as P5
import HigherOrder.P5Loops as P5

import UsefulFuncs as P

prettyRender :: (Renderer a) => a -> IO ()
prettyRender = putStrLn . render

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

import Modules.P5Transform as P
import Modules.P5Shapes as P
import Modules.P53D as P
import Modules.P5Color as P
import Modules.DebugFuncs as P
import Modules.P5Structure as P
import Modules.P5Setting as P
import Modules.P5Text as P
import Modules.P5Image as P
import Modules.P5Attributes as P

import P5Enviornment as P
import P5Expressions as P
import P5Render as P
import P5JSRenderFuncs as P
import P5Funcs as P
import P5Audio as P
import Exported.ExportedFunctions as P
import Exported.ExportedVariables as P
import ListWriter

import HigherOrder.P5Bool as P
import HigherOrder.P5Loops as P

import UsefulFuncs as P

prettyRender :: (Renderer a) => a -> IO ()
prettyRender = putStrLn . render

module Modules.Hydra where

import P5Expressions
import P5Render

data HydraExpression a = HydraExpression (HydraObject a) (HydraAttribute a)
  deriving (Eq, Show)

data HydraObject a = HydraObject a
  deriving (Eq, Show)

data HydraAttribute a = HydraAttribute a
  deriving (Eq, Show)

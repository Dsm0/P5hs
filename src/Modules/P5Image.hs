module Modules.P5Image where

import P5Expressions
import P5Render

data Image a
  = Image (ArgEx a) (ArgEx a) (ArgEx a) (ArgEx a) (ArgEx a)
  deriving (Eq, Show)

instance (Renderer a) => Renderer (Image a) where
  -- basically removes punctuation from the url so that when you paste in
  -- an image url and assign it to a variable,
  -- you can also pass it to the image function
  render (Image imageURL x y width height) = "image(" ++ img ++ " , " ++ args ++ ")"
    where
      args = betweenBrackets [x, y, width, height]
      img = "assets[\"images\"][\"" ++ removePunc (varFunc imageURL) ++ "\"]"

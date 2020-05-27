{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
module HigherOrder.P5Bool where

import P5Expressions
import P5Render
import ListWriter
import qualified Data.Fixed
import P5JSRenderFuncs
import Prelude hiding ((>),(<),(==),(/=),(<=),(>=))
import qualified Prelude as P

-- the core boolean operators are overridden for the sake of quick composition + lack of confusion when writing functions that need to compile to JS
-- if you want to evaluate a equality haskell-side, use P.==
(==) :: (ArgEx a) -> (ArgEx a) -> P5BoolConstructor a
(==) argex0 argex1 = P5Eq argex0 argex1
(!=) :: (ArgEx a) -> (ArgEx a) -> P5BoolConstructor a
(!=) argex0 argex1 = P5NEq argex0 argex1
(>) :: (ArgEx a) -> (ArgEx a) -> P5BoolConstructor a
(>) argex0 argex1 = P5Gt argex0 argex1
(<) :: (ArgEx a) -> (ArgEx a) -> P5BoolConstructor a
(<) argex0 argex1 = P5Lt argex0 argex1
(>=) :: (ArgEx a) -> (ArgEx a) -> P5BoolConstructor a
(>=) argex0 argex1 = P5Gt argex0 argex1
(<=) :: (ArgEx a) -> (ArgEx a) -> P5BoolConstructor a
(<=) argex0 argex1 = P5Lt argex0 argex1

data P5BoolConstructor a =
    P5Eq (ArgEx a) (ArgEx a)
  | P5NEq (ArgEx a) (ArgEx a)
  | P5Gt (ArgEx a) (ArgEx a)
  | P5Lt (ArgEx a) (ArgEx a)
  | P5GtEq (ArgEx a) (ArgEx a)
  | P5LtEq (ArgEx a) (ArgEx a)
  deriving(Eq,Show)

instance (Renderer a) => Renderer (P5BoolConstructor a) where
  render (P5Eq argex0 argex1) = jsEq f0 f1
    where (f0,f1) = (varFunc argex0, varFunc argex1)
  render (P5NEq argex0 argex1) = jsNEq f0 f1
    where (f0,f1) = (varFunc argex0, varFunc argex1)
  render (P5Gt argex0 argex1) = jsGt f0 f1
    where (f0,f1) = (varFunc argex0, varFunc argex1)
  render (P5Lt argex0 argex1) = jsLt f0 f1
    where (f0,f1) = (varFunc argex0, varFunc argex1)
  render (P5GtEq argex0 argex1) = jsGt f0 f1
    where (f0,f1) = (varFunc argex0, varFunc argex1)
  render (P5LtEq argex0 argex1) = jsLt f0 f1
    where (f0,f1) = (varFunc argex0, varFunc argex1)

evalP5Bool :: (Eq a, Ord a) => P5BoolConstructor a -> Bool
evalP5Bool (P5Eq a b) = (P.==) a b
evalP5Bool (P5NEq a b) = (P./=) a b
evalP5Bool (P5Gt a b) = (P.>) a b
evalP5Bool (P5Lt a b) = (P.<) a b
evalP5Bool (P5LtEq a b) = (P.>=) a b
evalP5Bool (P5GtEq a b) = (P.<=) a b

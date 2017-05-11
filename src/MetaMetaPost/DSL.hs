{-# LANGUAGE OverloadedStrings #-}
module MetaMetaPost.DSL where

import Bound
import Data.Foldable (toList)
import Data.Void (Void, vacuous)
import Data.List (foldl')

import MetaMetaPost.Color
import MetaMetaPost.STLC
import MetaMetaPost.Types

-- | Check that an expression is closed.
--
-- If it's not, make it an 'Error'.
closed_ :: Show a => Expr SomePrim () a -> Expr SomePrim () b
closed_ e = case closed e of
    Just e' -> e'
    Nothing -> Prim (SomePrim (Error $ "not closed: " ++ show (toList e)))

lam_ :: Eq a => a -> Expr p () a -> Expr p () a
lam_ v b = Lam () (abstract1 v b)

let_ :: Eq a => a -> Expr p () a -> Expr p () a -> Expr p () a
let_ v e b = Let () e (abstract1 v b)

infixl 8 $$
($$) :: Expr p ty a -> Expr p ty a -> Expr p ty a
($$) = App

dim_ :: Rational -> Expr SomePrim ty a
dim_ = Prim . SomePrim . Num . (* 7200) . (/ 254)

num_ :: Rational -> Expr SomePrim ty a
num_ = Prim . SomePrim . Num

str_ :: String -> Expr SomePrim ty a
str_ = Prim . SomePrim . Str

clr_ :: Color -> Expr SomePrim ty a
clr_ = Prim . SomePrim . Clr

pair_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
pair_ = arity2 Pair

pairdim_ :: Rational -> Rational -> Expr SomePrim ty a
pairdim_ x y = pair_ (dim_ x) (dim_ y)

line_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
line_ = arity2 Line

scaled_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
scaled_ = arity2 Scaled

path_ :: [Expr SomePrim ty a] -> Expr SomePrim ty a
path_ []  = Prim (SomePrim (Error "Empty path"))
path_ [_] = Prim (SomePrim (Error "Signular path"))
path_ (x:y:zs) = foldl' (arity2 Line') (line_ x y) zs

cyclic_ :: Expr SomePrim ty a -> Expr SomePrim ty a
cyclic_ =  arity1 Cycle

draw_ :: Expr SomePrim ty a -> Expr SomePrim ty a
draw_ = arity1 Draw

drawWithColor_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
drawWithColor_ = arity2 DrawC

drawArrow_ :: Expr SomePrim ty a -> Expr SomePrim ty a
drawArrow_ = arity1 DrawA

drawArrowWithColor_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
drawArrowWithColor_ = arity2 DrawAC

semicolons_ :: [Expr SomePrim ty a] -> Expr SomePrim ty a
semicolons_ = foldl1 semicolon_

unfill_ :: Expr SomePrim ty a -> Expr SomePrim ty a
unfill_ = arity1 Unfill

bbox_ :: Expr SomePrim ty a -> Expr SomePrim ty a
bbox_ = arity1 Bbox

image_ :: Expr SomePrim ty a -> Expr SomePrim ty a
image_ = arity1 Image

semicolon_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
semicolon_ = arity2 Semicolon

intersectionpoint_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
intersectionpoint_ = arity2 InterPoint

label_ :: Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim ty a
label_ = arity2 Label

-- | "Unfill" the background of image with the background color (white).
background_ :: Expr SomePrim () Void -> Expr SomePrim () a
background_ x = closed_ $
    let_ "x" (scaled_ (image_ (vacuous x)) (num_ 2)) $
    semicolon_ (unfill_ (bbox_ "x")) (draw_ "x")

arity1 :: Prim Arity1 -> Expr SomePrim ty a -> Expr SomePrim ty a
arity1 p x = Prim (SomePrim p) $$ x

arity2 :: Prim Arity2 -> Expr SomePrim ty a -> Expr SomePrim ty a -> Expr SomePrim  ty a
arity2 p x y = Prim (SomePrim p) $$ x $$ y

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module MetaMetaPost.PrettyPrinter (
    prettyprint,
    -- * Internals
    Doc,
    ) where

import Bound
import Data.Semigroup
import Data.String
import qualified Text.PrettyPrint.Leijen as PP

import MetaMetaPost.Color
import MetaMetaPost.Types

-- | (Almost) Pretty print 'MP' as a /MetaPost/ code.
prettyprint :: MP (Int, Scalar) Doc -> PP.Doc
prettyprint e = PP.vsep $
    [ PP.text "prologues := 3;"
    ]
    ++
    map colorDef [minBound .. maxBound]
    ++
    [ PP.text "beginfig(1);"
    , PP.text "path p[];"
    -- , PP.text "pair z[];" -- z is predefined
    , PP.text "picture q[];"
    , PP.text "numeric x[];"
    , unDoc (pp e) PP.<> PP.text ";"
    , PP.text "endfig;"
    , PP.text "end"
    ]
  where
    colorDef c = PP.text $ concat
        [ "color "
        , colorName c
        , "; "
        , colorName c
        , " := "
        , colorTriple c
        , ";"
        ]

-- | Pretty print the 'MP'
pp :: MP (Int, Scalar) Doc -> Doc
pp (MPApp app) = ppa app
pp (MPLet (i, s) e b) =
    let x = prettyName i s
    in Doc False $
        unDoc (x <> " = " <> ppa e <> ";") PP.<$$>
        unDoc (pp (instantiate1 (pure x) b))

ppa :: MPApp Doc -> Doc
ppa (MPVar d) = d
ppa (MPApp0 (Num r)) =
    fromString (show (realToFrac r :: Double))
ppa (MPApp0 (Str s)) =
    fromString (show s)
ppa (MPApp0 (Clr c)) =
    fromString (colorName c)
ppa (MPApp0 (Error s)) =
    parens ("abort " <> fromString s)
ppa (MPApp1 Cycle x) =
    parens (ppa x) <> "--cycle"
ppa (MPApp1 Draw x) =
    "draw " <> parens (ppa x)
ppa (MPApp2 DrawC x c) =
    "draw " <> parens (ppa x) <> " withcolor " <> parens (ppa c)
ppa (MPApp1 DrawA x) =
    "drawarrow " <> parens (ppa x)
ppa (MPApp2 DrawAC x c) =
    "drawarrow " <> parens (ppa x) <> " withcolor " <> parens (ppa c)
ppa (MPApp1 Unfill x) =
    "unfill " <> parens (ppa x)
ppa (MPApp1 XPart x) =
    fn "xpart" (ppa x)
ppa (MPApp1 YPart x) =
    fn "ypart" (ppa x)
ppa (MPApp1 Image x) =
    fn "image" (mapDoc PP.align (ppa x))
ppa (MPApp1 Bbox x) =
    fn "bbox" (ppa x)
ppa (MPApp2 Label s x) =
    fn "thelabel" (ppa s <> ","  <> ppa x)
ppa (MPApp2 Scaled x y) =
    parens (ppa x) <> " scaled " <> parens (ppa y)
ppa (MPApp2 Line x y) =
    parens (ppa x) <> "--" <> parens (ppa y)
ppa (MPApp2 Line' x y) =
    parens (ppa x) <> "--" <> parens (ppa y)
ppa (MPApp2 Pair x y) =
    parens (ppa x <> "," <> ppa y)
ppa (MPApp2 Semicolon x y) = Doc False $
    unDoc (ppa x) PP.<> PP.text ";" PP.<$$>
    unDoc (ppa y)
ppa (MPApp2 InterPoint x y) =
    parens (ppa x) <> " intersectionpoint " <> parens (ppa y)

prettyName :: Int -> Scalar -> Doc
prettyName i SNum  = Doc True $ PP.text ("x" ++ show i)
prettyName i SPair = Doc True $ PP.text ("z" ++ show i)
prettyName i SPath = Doc True $ PP.text ("p" ++ show i)
prettyName i SPict = Doc True $ PP.text ("q" ++ show i)
prettyName _ _     = Doc True $ PP.text "whatever"

-------------------------------------------------------------------------------
-- Parens wrapper
-------------------------------------------------------------------------------

-- | A wrapper adound @wl-pprint@ 'PP.Doc' tracking whether we have
-- already wrapped it in parentheses.
data Doc = Doc !Bool PP.Doc

unDoc :: Doc -> PP.Doc
unDoc (Doc _ d) = d

mapDoc :: (PP.Doc -> PP.Doc) -> Doc -> Doc
mapDoc f (Doc p d) = Doc p (f d)

instance IsString Doc where
    fromString = Doc True . PP.text

instance Semigroup Doc where
    Doc _ a <> Doc _ b = Doc False (a PP.<> b)

fn :: String -> Doc -> Doc
fn n d = Doc True $ PP.hcat [ PP.text n, PP.text "(", unDoc d, PP.text ")"]

parens :: Doc -> Doc
parens (Doc False d)  = Doc True $ PP.text "(" PP.<> d PP.<> PP.text ")"
parens x@(Doc True _) = x

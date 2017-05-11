{-# LANGUAGE OverloadedStrings #-}
module MetaMetaPost.Demos where

import Data.Foldable (traverse_)
import Text.Printf (printf)
import qualified System.Clock as Clock

import MetaMetaPost

-- | A box.
box1 :: Expr SomePrim ty a
box1 =
    let z0 = pair_ (dim_ 0) (dim_ 0)
        z1 = pair_ (dim_ 1) (dim_ 0)
        z2 = pair_ (dim_ 1) (dim_ 1)
        z3 = pair_ (dim_ 0) (dim_ 1)
    in draw_ $ cyclic_ $ path_ [z0, z1, z2, z3]

-- | A bit differently defined box.
box2 :: Expr SomePrim () a
box2 = closed_ $
    let_ "z0" (pair_ (dim_ 0) (dim_ 0)) $
    let_ "z1" (pair_ (dim_ 1) (dim_ 0)) $
    let_ "z2" (pair_ (dim_ 1) (dim_ 1)) $
    let_ "z3" (pair_ (dim_ 0) (dim_ 1)) $
    draw_ $ cyclic_ $ path_ ["z0", "z1", "z2", "z3"]

-- | MetaMetaPost module graph.
modules :: UExpr' a
modules = closed_ $
    foldr defineVertex (semicolons_ drawings) vertexes
  where
    drawings :: [UExpr' String]
    drawings =
        map (drawArrow_ . uncurry edge_) edges ++
        map (flip drawArrowWithColor_ (clr_ ColorBlue) . uncurry edge_) edges2 ++
        map (draw_ . vertex_) vertexes

    edge_ :: String -> String -> UExpr' String
    edge_ a b =
        let_ "p" (line_ (Var a) (Var b)) $
        line_ (intersectionpoint_ "p" (bbox_ $ Var $ "label-" ++ a))
              (intersectionpoint_ "p" (bbox_ $ Var $ "label-" ++ b))

    vertex_ :: (String, a, a) -> UExpr' String
    vertex_ (v, _, _) = Var ("label-" ++ v)

    defineVertex :: (String, Rational, Rational) -> UExpr' String -> UExpr' String
    defineVertex (v, x, y) e =
        let_ v (pairdim_ x y) $
        let_ ("label-" ++ v) (label_ (str_ v) (Var v)) $
        e

    vertexes :: [(String, Rational, Rational)]
    vertexes = map (\(a,b,c) -> (a, 3 * b, 0.6 * negate c))
        [ (,,) "Types" 0 7
        , (,,) "STLC"  0 5
        , (,,) "TypeChecker" 1 4
        , (,,) "TypeInferrer" 1 2
        , (,,) "Simplifier" 1 6
        , (,,) "PrettyPrinter" 1 8
        , (,,) "Doc" 1 10
        , (,,) "DSL" 1 0
        ]

    edges :: [(String, String)]
    edges =
        [ (,) "STLC" "Types"
        , (,) "STLC" "TypeChecker"
        , (,) "STLC" "TypeInferrer"
        , (,) "STLC" "Simplifier"
        , (,) "Types" "Simplifier"
        , (,) "Types" "PrettyPrinter"
        ]

    edges2 :: [(String, String)]
    edges2 =
        [ (,) "DSL" "TypeInferrer"
        , (,) "TypeInferrer" "TypeChecker"
        , (,) "TypeChecker" "Simplifier"
        , (,) "Simplifier" "PrettyPrinter"
        , (,) "PrettyPrinter" "Doc"
        ]

-- | Write the demos to @output/.@
writeDemos :: IO ()
writeDemos = traverse_ f demos where
    f (name, expr) = do
        putStrLn $ "Demo: " ++ name
        printf "size %d\n" (exprSize expr)
        c <- Clock.getTime Clock.Monotonic
        metametapostIO ("output/" ++ name ++ ".mp") (background_ expr)
        c' <- Clock.getTime Clock.Monotonic
        printf "took %.06fs\n" $ (realToFrac $ Clock.toNanoSecs $ Clock.diffTimeSpec c' c :: Double) / 1e9

    demos =
        [ (,) "box1" box1
        , (,) "box2" box2
        , (,) "modules" modules
        ]

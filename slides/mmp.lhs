\usetheme{default}

%% Fonts
\usefonttheme{serif}
\usepackage{mathpazo}
\usepackage{helvet}
\usepackage{courier}
\usepackage{inconsolata}
\usepackage{microtype}

%% Colors
\setbeamercolor{structure}{fg=futugreen}
\definecolor{futugreen}{HTML}{266826}

%% Some packages
\usepackage{hyperref}

%% lhs2TeX
%include polycode.fmt
%include forall.fmt
%format s_0 = "\Varid{s}_0 "
%format s_1 = "\Varid{s}_1 "
%format s_2 = "\Varid{s}_2 "
%format x_1 = "\Varid{x}_1 "
%format x_2 = "\Varid{x}_2 "
%format ... = "\ldots "
%format subst_2 = "\Varid{subst}_2"
%format >>>= = ">\mskip-8mu>\mskip-8mu>\mskip-8mu>\mskip-7mu= "
%format :-> = "\mathbin{:\!\to}"
%format b1 = "\Varid{b}_1"
%format b2 = "\Varid{b}_2"

\newcommand{\at}{\makeatletter |@|\makeatother}

\title{MetaMetaPost}
\subtitle{HaskHEL meetup at Nitor}
\author{Oleg Grenrus}
\institute{Futurice}
\date{2017-05-11}

\begin{document}

%if 0
\begin{code}
-- stack --resolver=nightly-2017-04-01 mmp.lhs
\end{code}
%endif

\begin{frame}
\titlepage
\end{frame}


\begin{frame}
\frametitle{HaskHEL}
\begin{itemize}
\item \texttt{/join \#haskhel} on freenode
\item We need talks: topics, presenters\ldots
\item \url{https://github.com/haskhel/events}
\end{itemize}
\end{frame}


\begin{frame}
\frametitle{Motivation}
\includegraphics[width=\textwidth]{figures/optics-hierarchy}

{\footnotesize
From \url{http://oleg.fi/gists/posts/2017-04-18-glassery.html}
}
\end{frame}

\note[itemize]{
\item We want (or need) to draw pictures like this
}


\begin{frame}
I was asked in a \href{https://www.reddit.com/r/haskell/comments/6627nn/glassery_an_optics_zoo/dgfo0ou/}{Reddit comment:}

\includegraphics[width=\textwidth]{figures/reddit-comment}
\end{frame}

\note[itemize]{
\item The answer is: MetaPost; and for the previous picture I used Haskell as a macro language
\item Also, this raster PNG looks terrible
}


\begin{frame}
\frametitle{The spec is nice\ldots}
\begin{spec}
nodes :: [(String, Float, Float)]
nodes =
    [ n "Equality"  1.5   0  -- name x y
    , n "Iso"       1.5   1
    , n "Lens"      0.75  2
    ...

edges :: [(String, String, [String])]
edges =
    [ e "Equality"  "Iso"   ["black"]  -- from to colors
    , e "Iso"       "Lens"  ["strong"]
    ...
\end{spec}
\end{frame}

\note[itemize]{
\item Quick but not so dirty specification of the graph
\item Gave possibility to incrementally improve
\item |n| and |e| are tuple constructors
}


\begin{frame}
\frametitle{\ldots but the backend isn't}
\scalebox{0.7}{\parbox{\linewidth}{
\begin{spec}
...

for_ (zip [0 :: Int ..] edges) $ \(k, (a,b,cs)) -> do
    let  i = findNode a
         j = findNode b
    printf "p%d=clippath(z%d..z%d, q%d, q%d);\n" k i j i j
    when (k == 15) $ do
         printf "h1 = p9 intersectionpoint p15;\n"
         printf "w1 = fullcircle xscaled (1.2*1.5mm) yscaled 1.2mm shifted h1;\n"
    when (k == 12) $ do
         printf "h2 = p10 intersectionpoint p12;\n"
         printf "w2 = fullcircle xscaled (1.2*1.5mm) yscaled 1.2mm shifted h2;\n"
    printf "u%d=unitvector(direction 0 of p%d);\n" k k
    for_ (zip (offsets cs) cs) $ \((o, a), c) -> do
         let p = printf "(p%d shifted (%s*u%d rotated 90))" k o k :: String

...
\end{spec}
}}
\end{frame}

\note[itemize]{
\item Writing |String|s directly to the output.
\item Doing once is ok, won't repeat.
}


\begin{frame}
\frametitle{Better solution: DSL}
\begin{itemize}
\item \emph{Embedded} so one can use \emph{Haskell} as a macro language
\item \emph{Typed} to catch errors early
\item $\to$ \emph{MetaMetaPost}
\end{itemize}
\end{frame}

\note[itemize]{
\item MetaMetaPost is still a toy implementation, as usual the error reporting is a weak spot
}


\begin{frame}
\frametitle{Example}
\begin{spec}
box2 :: Expr SomePrim () a
box2 = closed_ $ background_ $
    let_ "z0" (pair_ (dim_ 0) (dim_ 0)) $
    let_ "z1" (pair_ (dim_ 1) (dim_ 0)) $
    let_ "z2" (pair_ (dim_ 1) (dim_ 1)) $
    let_ "z3" (pair_ (dim_ 0) (dim_ 1)) $
    draw_ $ cyclic_ $ path_ ["z0", "z1", "z2", "z3"]
\end{spec}

\begin{center}
\includegraphics{figures/box2}
\end{center}
\end{frame}


\begin{frame}
\frametitle{Example output}
\begin{verbatim}
beginfig(1);
path p[];
picture q[];
numeric x[];
z0 = (0.0,0.0);
z1 = (28.346456692913385,0.0);
z2 = (28.346456692913385,28.346456692913385);
z3 = (0.0,28.346456692913385);
q4 = image(draw ((((z0--z1)--z2)--z3)--cycle)) scaled 2.0;
unfill bbox(q4);
draw q4;
endfig;
\end{verbatim}
\end{frame}

\note[itemize]{
\item $28.$ is trying to be $1\textrm{cm}$.
}


\begin{frame}
\frametitle{How it works}
\begin{center}
\includegraphics{figures/modules}
\end{center}
\end{frame}

\note[itemize]{
\item Steps like in a "real" compiler.
\item STLC is quite generic definition of Simply typed lambda calculus
\item Types contains MetaPost specifics
\item DSL module is used to build the expression |Expr|
\item TypeInferrer infers a type of an expression, and in TypeChecker we check it's one we want
\item Simplifier simplifies the expression into one suitable for the output: |MP|
\item And PrettyPrinter outputs it.
}


\begin{frame}
\frametitle{STLC}
\begin{spec}
data Expr p ty a
    =  Var a
    |  App (Expr p ty a) (Expr p ty a)
    |  Lam ty (Scope () (Expr p ty) a)
    |  Let ty (Expr p ty a) (Scope () (Expr p ty) a)
    |  Prim p

data Type s
    =  TyScalar s
    |  Type s :-> Type s
\end{spec}
\end{frame}

\note[itemize]{
\item an example definition of Expr from \texttt{bound}
\item amended with |Let| and |Prim|
\item We try to persist |Let|s to te output
\item |Prim| are for primitives
\item |Lam|bda abstraction isn't actually used but it's there for completeness
}


\begin{frame}
\frametitle{Types}
\begin{spec}
type Ty = Type Scalar

data Scalar
    =  SNum
    |  SStr
    |  SPair
    |  SPath
    |  SPict
    |  SColor
    |  SStmt
\end{spec}
\end{frame}


\begin{frame}
\frametitle{Primitives}
\begin{spec}
data Prim arity where
  Num    :: Rational  ->  Prim Arity0
  Str    :: String    ->  Prim Arity0
  Clr    :: Color     ->  Prim Arity0
  Error  :: String    ->  Prim Arity0
  Bbox   ::               Prim Arity1
  Cycle  ::               Prim Arity1
  Draw   ::               Prim Arity1
  Line   ::               Prim Arity2
  -- and more (around 20 atm)
\end{spec}
\end{frame}

\note[itemize]{
\item Something one could use \emph{TemplateHaskell} to generate: \emph{MetaMetaMetaPost}
}


\begin{frame}
\frametitle{DSL}
A lot of straight-forward "macros" to write |Expr|s:

\begin{spec}
unfill_   ::  Expr SomePrim ty a
          ->  Expr SomePrim ty a
unfill_ = arity1 Unfill

arity1  ::  Prim Arity1
        ->  Expr SomePrim ty a
        ->  Expr SomePrim ty a
arity1 p x = Prim (SomePrim p) $$ x

infixl 8 $$
($$) :: Expr p ty a -> Expr p ty a -> Expr p ty a
($$) = App
\end{spec}
\end{frame}

\note[itemize]{
\item |$$| has different infix definition than |$|.
}

\begin{frame}
\frametitle{TypeChecker}
\begin{spec}
primTy :: Prim arity -> [Ty]
primTy (Num _)    =  [ TyScalar SNum ]
primTy (Str _)    =  [ TyScalar SStr ]
primTy (Clr _)    =  [ TyScalar SColor ]
primTy (Error _)  =  [ ]
primTy Draw       =  [ TyScalar SPath  :-> TyScalar SStmt
                     , TyScalar SPict  :-> TyScalar SStmt
                     ]
primTy Line       =  [ TyScalar SPair  :-> TyScalar SPair
                                       :-> TyScalar SPath ]
\end{spec}
\end{frame}

\note[itemize]{
\item Type definitions for primitives
\item We use lists to implement ad-hoc polymorphism: bad idea.
\item |Error| is of type $\forall \alpha. \alpha$, though STLC cannot express that directly
}


\begin{frame}
\frametitle{TypeChecker 2}
\begin{spec}
typecheck  :: (Eq s, Show s, Show p)
           => (p -> [Type s])
           -> Expr p (Type s) (Type s)
           -> Either String (NonEmpty (Type s))
\end{spec}

\begin{spec}
go (Var ty) = pure ty
go (Lam ty b) = do
     ty' <- go (instantiate1 (Var ty) b)
     pure (ty :-> ty')
go (App f x) = do
    tyF  <- go f
    tyX  <- go x
    case tyF of
        a :-> b
            | a == tyX   -> pure b
            | otherwise  -> throwError $ "cannot match"
        _ -> throwError $ "type of f isn't a function"
\end{spec}
\end{frame}

\note[itemize]{
\item first argument, type of primitives (|primTy|)
\item second argument the expression
\item we can see how errors are made
\item better errors: use the context, provide more context
\item |Var| case as simple, as here "free" variables tell their type directly
\item |Lam| is also quite simple, as expression is annotated with a type, so we instantiate and proceed.
\item |App| is a bit more complicated: we type-check the subexpressions, so we get their types; and t hen check they align.
}


\begin{frame}
\frametitle{TypeInferrer}
\begin{spec}
typeinfer  ::  forall p s a. (Show p, Show a, Show s, Ord s)
           =>  Type s
           ->  (p -> [Type s])
           ->  Expr p () a
           ->  Either [String] (Expr p (Type s) a)
typeinfer defType pr expr = runInfer $ do
    e0  <- bitraverse (\() -> newVar) pure expr
    e1  <- bitraverse pure (\_ -> newVar) e0
    _   <- walk e1
    IS _ constraints' _ <- get
    let constraints = appEndo constraints' []
    solution <- solve defType constraints
    case bitraverse (lower solution) pure e0 of
        Nothing -> error "unsolved something"
        Just x -> pure x
\end{spec}
\end{frame}

\note[itemize]{
\item type of |typeinfer| is similar with the type of |typecheck|, the differences:
\item an argument for "default type" (as we cannot have $\forall \alpha.\alpha$).
\item input is untyped term (|ty = ()|), and output is typed one (|ty = Type s|).
\item 1st step (|e0|): allocate unsolved type variables (in |Let| and |Lam|)
\item 2nd step (|e1|): allocate unsolved type variables for free variables
\item usually the term is closed so it's no op; but we can use free variables for typed holes for example!
\item 3rd step (|constraints|): collect the type constraints from the |Expr| tree. |walk| is quite simialr to |go| in |typecheck|.
\item 4th step (|solution|): solve the constraint problem. That's where unification happens
\item finalize by looking up the values of type variables.
\item 1st and 2nd step could be combined, but then we'll loose |a|.
}


\begin{frame}
\frametitle{MP}
The |MP| is |let x = ... in let y = ... in ...|
\begin{spec}
data MP ty a
    =  MPApp (MPApp a)
    |  MPLet ty (MPApp a) (Scope () (MP ty) a)
\end{spec}
where the \ldots are variables or applications of primitives:
\begin{spec}
data MPApp a
    =  MPVar a
    |  MPApp0 (Prim Arity0)
    |  MPApp1 (Prim Arity1) (MPApp a)
    |  MPApp2 (Prim Arity2) (MPApp a) (MPApp a)
\end{spec}
\end{frame}

\note[itemize]{
\item Pretty printing this is quite straight-forward
}


\begin{frame}
\frametitle{Simplifier}
Converts well typed |Expr| into |MP|.
\begin{spec}
simplify :: Expr SomePrim (Type s) a -> Either String (MP s a)
\end{spec}
Performs rewrites:
\begin{itemize}
\item $\beta$-reduction: |App (Lam _ b) e| (= |(\x -> b) e|) into |instantiate1 b e|
\item Floats lets to top:
  |f (let x = e in b)|
  $\mapsto$
  |let x = e in f b|
\item Re-assocs lets (flatten):\\
  |let x = (let y = e in b1) in b2|
  $\mapsto$
  |let y = e in let x = b1 in b2|
\end{itemize}

\end{frame}

\note[itemize]{
\item It can fail, because we can't be sure |Expr| is actually well-typed
\item GHC rewrites |(\x -> b) e| into |let x = e in b|; we however have
special meaning for |let| so we just instantiate (but we could rewrite to |let|!)
}

\begin{frame}
\begin{center}
{\Large Make languages to make languages to \ldots}

{\LARGE \ldots PROFIT!}
\end{center}
\end{frame}


\end{document}

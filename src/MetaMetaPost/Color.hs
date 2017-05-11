module MetaMetaPost.Color where

-- | Predefined colors.
data Color
    = ColorBlack
    | ColorRed
    | ColorGreen
    | ColorBlue
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | RGB triple suitable for /MetaPost/ output.
colorTriple :: Color -> String
colorTriple ColorBlack = "(0,0,0)"
colorTriple ColorRed   = "(1,0,0)"
colorTriple ColorGreen = "(0,1,0)"
colorTriple ColorBlue  = "(0,0,1)"

-- | Unique name suitable for variable name in /MetaPost/ output.
colorName :: Color -> String
colorName ColorBlack = "blackC"
colorName ColorRed   = "redC"
colorName ColorGreen = "greenC"
colorName ColorBlue  = "blueC"

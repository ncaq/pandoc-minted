import           Data.Maybe
import           Data.Monoid
import           Text.Pandoc
import           Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter toMinted

toMinted :: Block -> Block
toMinted = bottomUp toMintedBlock . bottomUp toMintedInline

toMintedBlock :: Block -> Block
toMintedBlock (CodeBlock (identity, classes, namevals) contents) =
  RawBlock (Format "latex") $ unlines
  [ "\\begin{minted}[breakautoindent=false, breaklines=true, linenos=true]{" <> lang <> "}"
  , contents
  , "\\end{minted}"
  ]
  where label = if identity /= "" then "\\label{" <> identity <> "}" else ""
        lang = if classes /= [] then head classes else "\\mintlang"
        caption = fromMaybe "" $ lookup "caption" namevals
toMintedBlock x = x

toMintedInline :: Inline -> Inline
toMintedInline (Code (_, classes, _) contents) =
  RawInline (Format "latex") $ "\\mintinline{" <> lang <> "}{" <> contents <> "}"
  where lang = if classes /= [] then head classes else "\\mintlang"
toMintedInline x = x

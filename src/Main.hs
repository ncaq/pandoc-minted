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
    let label = if identity /= "" then "\\label{" <> identity <> "}" else ""
        lang = if classes /= [] then head classes else "\\mintlang"
        caption = fromMaybe "" $ lookup "caption" namevals
    in RawBlock (Format "latex") $ unlines
       [ "\\begin{minted}[linenos=true]{" <> lang <> "}"
       , contents
       , "\\end{minted}"
       , "\\captionof{listing}{" <> caption <> label <> "}"
       ]
toMintedBlock x = x

toMintedInline :: Inline -> Inline
toMintedInline (Code (_, classes, _) contents) =
    let lang = if classes /= [] then head classes else "\\mintlang"
    in RawInline (Format "latex") $ "\\mintinline{" <> lang <> "}{" <>contents <> "}"
toMintedInline x = x

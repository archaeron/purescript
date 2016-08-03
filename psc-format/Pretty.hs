module Pretty where

import Prelude ((.), ($), fmap)
import Text.PrettyPrint.ANSI.Leijen

(<|>) :: Doc -> Doc -> Doc
a <|> b = group $ flatAlt b a

infixl 5 <|>

prettyTupled :: [Doc] -> Doc
prettyTupled docs = wide parens docs <|> skinny parens docs

wide :: (Doc -> Doc) -> [Doc] -> Doc
wide surround = surround . hcat . punctuate (text ", ")

skinny :: (Doc -> Doc) -> [Doc] -> Doc
skinny surround = surround . hcat . punctuate (char ',') . fmap (\x -> text " " <> x <> linebreak)

listify :: [Doc] -> Doc
listify = cat . punctuate (comma <> space)
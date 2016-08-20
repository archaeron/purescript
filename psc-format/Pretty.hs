module Pretty where

import Prelude ((.), ($), (++), fmap)
import Text.PrettyPrint.ANSI.Leijen

(<|>) :: Doc -> Doc -> Doc
a <|> b = group $ flatAlt b a

infixl 5 <|>

prettyTupled :: [Doc] -> Doc
prettyTupled docs = wide parens docs <|> skinny parens docs

wide :: (Doc -> Doc) -> [Doc] -> Doc
wide surround = surround . hcat . punctuate (text ", ")

skinny :: (Doc -> Doc) -> [Doc] -> Doc
skinny surround =
    surround . hcat . punctuate comma . fmap (\x -> space <> x <> linebreak)

listify :: [Doc] -> Doc
listify = cat . punctuate (comma <> space)

prettyEncloseSep :: Doc -> Doc -> [Doc] -> Doc
prettyEncloseSep l r y = case y of
    [] -> empty
    (x : xs) -> group (vcat (empty : l <+> x : fmap (comma <+>) xs) <$> r)

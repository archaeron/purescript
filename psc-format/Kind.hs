{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kind where

import Text.PrettyPrint.ANSI.Leijen (Pretty, (<+>), pretty, text)

import Language.PureScript.Kinds

instance Pretty Kind where
    pretty k = case k of
        KUnknown a -> text "KUnknown" <+> pretty a
        Star -> text "*"
        Bang -> text "!"
        Row kind -> text "#" <+> pretty kind
        FunKind kind1 kind2 -> pretty kind1 <+> text "->" <+> pretty kind2
        Symbol -> text "Symbol"

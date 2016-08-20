{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binder where

import Prelude

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.AST.Binders

import Comments ()
import Literals ()
import Names ()
import Types ()

instance Pretty Binder where
    pretty NullBinder = text "_"
    pretty (LiteralBinder literalBinder) = pretty literalBinder
    pretty (VarBinder ident) = pretty ident
    pretty (ConstructorBinder constructorName binders) = pretty constructorName <> bs
        where
            bs = case binders of
                [] -> empty
                _ -> space <> prettyList binders
    pretty (OpBinder _valueOpName) = text "OpBinder"
    pretty (BinaryNoParensBinder _binder1 _binder2 _binder3) =
        text "BinaryNoParensBinder"
    pretty (ParensInBinder binder) = parens . pretty $ binder
    pretty (NamedBinder ident binder) = pretty ident <> text "@" <> pretty binder
    pretty (PositionedBinder _ comments binder) = comments' <> pretty binder
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline
    pretty (TypedBinder typ binder) = pretty binder <+> text "::" <+> pretty typ

    prettyList = sep . fmap pretty

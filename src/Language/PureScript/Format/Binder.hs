{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Binder where

import           Prelude

import           Language.PureScript.Format.Pretty as PP

import           Language.PureScript.AST.Binders

import           Language.PureScript.Format.Comments ()
import           Language.PureScript.Format.Literals ()
import           Language.PureScript.Format.Names    ()
import           Language.PureScript.Format.Types    ()
import           Language.PureScript.AST.Literals  (Literal (..))
import           Language.PureScript.Format.Symbols

instance PrettyAnn Binder where
    pretty NullBinder = underscoreSym
    pretty (LiteralBinder literalBinder) = prettyLiteralBinder literalBinder
    pretty (VarBinder ident) = pretty ident
    pretty (ConstructorBinder constructorName binders) =
        pretty constructorName <> bs
        where
            bs = case binders of
                [] -> mempty
                _  -> space <> prettyList binders
    pretty (OpBinder valueOpName) =
        pretty valueOpName
    pretty (BinaryNoParensBinder binder1 binder2 binder3) =
        pretty binder2
        <+> pretty binder1
        <+> pretty binder3
    pretty (ParensInBinder binder) = parens . pretty $ binder
    pretty (NamedBinder ident binder) = pretty ident <> atSym <> pretty binder
    pretty (PositionedBinder _pos comments binder) = comments' <> pretty binder
        where
            comments'
                | null comments = mempty
                | otherwise = vsep (fmap pretty comments) <> hardline
    pretty (TypedBinder typ binder) = pretty binder <+> doublecolonSym <+> pretty typ

    prettyList = sep . fmap pretty

prettyLiteralBinder :: PrettyAnn a => Literal a -> Doc Ann
prettyLiteralBinder (ObjectLiteral x) =
    collection (lbrace <> space) (space <> rbrace) comma $ fmap (pretty . snd) x
prettyLiteralBinder b = pretty b

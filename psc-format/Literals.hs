{-# OPTIONS_GHC -fno-warn-orphans #-}

module Literals where

import Prelude (Either(Left, Right), (++), ($), fmap)

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.AST.Literals (Literal (..))

import Pretty

instance Pretty a => Pretty (Literal a) where
    pretty (NumericLiteral integerOrDouble) = case integerOrDouble of
      Left integer' -> pretty integer'
      Right number -> pretty number
    pretty (StringLiteral s) = text ("\"" ++ s ++ "\"")
    pretty (CharLiteral c) = text ['\'', c, '\'']
    pretty (BooleanLiteral b) = text $ if b then "true" else "false"
    pretty (ArrayLiteral vs) = list $ fmap pretty vs
    pretty (ObjectLiteral os) = text "{" <+> listify (fmap (\(key, val) -> text key <+> text ":" <+> pretty val) os) <+> text "}"

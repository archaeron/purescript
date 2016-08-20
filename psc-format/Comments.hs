{-# OPTIONS_GHC -fno-warn-orphans #-}

module Comments where

import Prelude

import Text.PrettyPrint.ANSI.Leijen

import Language.PureScript.Comments

instance Pretty Comment where
    pretty (LineComment s) = text "--" <> pretty s
    pretty (BlockComment s) = text "{-" <> pretty s <> text "-}"

    prettyList = vcat . fmap pretty

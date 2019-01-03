{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Comments where

import           Prelude

import           Language.PureScript.Format.Pretty

import           Language.PureScript.Comments

instance PrettyAnn Comment where
    pretty (LineComment s)  = comment $ "--" <> pretty s
    pretty (BlockComment s) = comment $ "{-" <> pretty s <> "-}"

    prettyList = sep . fmap pretty

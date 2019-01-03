module Language.PureScript.Format.Collections where

import Language.PureScript.Format.Pretty
import Language.PureScript.Format.Symbols
import Data.Monoid (mempty)

pipedCollection :: [Doc Ann] -> Doc Ann
pipedCollection =
    collection
        (pipeSym <> space)
        mempty
        space

tupleCollection :: [Doc Ann] -> Doc Ann
tupleCollection =
    collection
        lparen
        rparen
        comma
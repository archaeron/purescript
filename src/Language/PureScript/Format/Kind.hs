{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Kind where

import           Language.PureScript.Format.Pretty

import           Language.PureScript.Format.Names  ()
import           Language.PureScript.Kinds

instance PrettyAnn Kind where
    pretty k = case k of
        KUnknown a          -> "KUnknown" <+> pretty a
        Row kind            -> "#" <+> pretty kind
        FunKind kind1 kind2 -> pretty kind1 <+> "->" <+> pretty kind2
        NamedKind namedKind -> pretty namedKind
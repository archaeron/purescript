{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Names where

import           Data.List                         (intersperse)
import           Prelude

import           Language.PureScript.Format.Pretty as PP
import           Language.PureScript.Names

instance PrettyAnn (ProperName a) where
    pretty = pretty . runProperName

instance PrettyAnn ModuleName where
    pretty (ModuleName moduleName) =
        foldl mappend mempty
            . intersperse dot
            $ map pretty moduleName

instance PrettyAnn Ident where
    pretty (Ident i)                = pretty i
    pretty (GenIdent _mstring _int) = PP.dbg "<genIdent>"

instance PrettyAnn (OpName a) where
    pretty (OpName name) =
        pretty name


instance PrettyAnn a => PrettyAnn (Qualified a) where
    pretty (Qualified mN n) =
        moduleName <> pretty n
        where
            moduleName =
                case mN of
                    Just name -> pretty name <> dot
                    Nothing   -> mempty


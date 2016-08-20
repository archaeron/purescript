{-# OPTIONS_GHC -fno-warn-orphans #-}

module Names where

import Prelude
import Data.List (intersperse)

import Text.PrettyPrint.ANSI.Leijen as PP

import Language.PureScript.Names

instance Pretty (ProperName a) where
    pretty = text . runProperName

instance Pretty ModuleName where
    pretty (ModuleName moduleName) =
        foldl (PP.<>) PP.empty
            . intersperse dot
            $ map pretty moduleName

instance Pretty Ident where
    pretty (Ident i) = text i
    pretty (GenIdent _mstring _int) = text "genIdent"

instance Pretty (OpName a) where
    pretty (OpName name) = text name

instance Pretty a => Pretty (Qualified a) where
    pretty (Qualified mN n) =
        moduleName <> pretty n
        where
            moduleName =
                case mN of
                    Just name -> pretty name <> dot
                    Nothing -> text ""

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Types
    ( ppTypeList
    ) where

import           Prelude

import qualified Data.Text                         as T

import           Language.PureScript.Format.Pretty as PP
import qualified Language.PureScript.Kinds         as KK
import           Language.PureScript.Label
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Language.PureScript.Format.Symbols
import           Language.PureScript.Format.Kind   ()
import           Language.PureScript.Format.Names  ()
import           Language.PureScript.Format.Keywords
-- import           Language.PureScript.Format.Pretty

-- https://github.com/purescript/purescript/blob/f6f4de900a5e1705a3356e60b2d8d3589eb7d68d/src/Language/PureScript/Pretty/Types.hs#L28-L39
instance PrettyAnn Type where
    pretty (TypeWildcard _) = underscoreSym
    pretty (TypeVar var) = pretty var
    pretty (TypeLevelString s) = string $ "\"" <> pretty s <> "\""
    pretty (PrettyPrintObject row) =
        prettyPrintRowWith '{' '}' row
    pretty (TypeConstructor (Qualified moduleName properName)) =
        case moduleName of
            Nothing -> typeConstructor $ pretty $ runProperName properName
            Just moduleN -> pretty moduleN <> dot <> pretty (runProperName properName)
    pretty (TUnknown u) = pretty '_' <> pretty u
    pretty (Skolem name s _ _) = pretty name <> dbg (T.pack (show s ++ "skolem"))
    pretty REmpty = "()"
    pretty (TypeApp (TypeConstructor (Qualified _ (ProperName "Record"))) s) =
        prettyPrintRowWith '{' '}' s
    pretty (TypeApp (TypeConstructor (Qualified _ (ProperName "Function"))) name) =
        pretty name <+> forwardArrowSym
    pretty (TypeApp t s) =
        pretty t
        <+> pretty s
    pretty row@RCons{} = prettyPrintRowWith '(' ')' row
    pretty (TypeOp op) = pretty $ showQualified runOpName op
    pretty (BinaryNoParensType op l r) =
        pretty l <+> operator (pretty op) <+> pretty r
    pretty (ParensInType typ) = parens $ pretty typ
    pretty (ForAll s t _) =
        ppForAll s t []
    pretty (ConstrainedType constraint typ) =
        pretty constraint <+> doubleForwardArrowSym <+> pretty typ
        -- where
        --     constraints'
        --         | length constraints == 1 = pretty (head constraints)
        --         | otherwise = parens (listify (fmap pretty constraints))
    pretty (KindedType typ kind) = pretty typ <+> doublecolonSym <+> pretty kind
    pretty (PrettyPrintFunction _typ1 _typ2) = dbg "<PrettyPrintFunction>"
    pretty (PrettyPrintForAll _xs _typ) = dbg "<PrettyPrintForall>"
    pretty (ProxyType typ) = dbg typ

    prettyList = hsep . fmap pretty

instance PrettyAnn Constraint where
    pretty (Constraint class' args _data) =
        pretty class' <+> sep (fmap pretty args)

-- | Pretty print something with a "forall" in front.
ppForAll :: T.Text -> Type -> [T.Text] -> Doc Ann
ppForAll typeVar typ vars =
    case typ of
        ForAll s t _ ->
            ppForAll s t $ typeVar : vars
        _ ->
            forallKw <+> typeVars <> dotSym <+> pretty typ
          where
            typeVars = pretty $ T.unwords $ typeVar : vars

ppTypeList :: [(T.Text, Maybe KK.Kind)] -> Doc Ann
ppTypeList =
    sep . fmap (\(s, kind) ->
            case kind of
                Nothing -> pretty s
                Just k -> lparen <+> pretty s <+> doublecolonSym <+> pretty k <+> rparen)

prettyPrintRowWith :: Char -> Char -> Type -> Doc Ann
prettyPrintRowWith op cl = group . uncurry listToDoc . toList []
    where
        open = punctuation $ pretty op
        close = punctuation $ pretty cl

        tailToPs :: Type -> Doc Ann
        tailToPs REmpty = mempty
        tailToPs other  = pipeSym <+> pretty other

        nameAndTypeToPs :: Doc Ann -> Label -> Type -> Doc Ann
        nameAndTypeToPs start name ty =
            start <> pretty ' ' <> pretty name <+> doublecolonSym <+> pretty ty

        listToDoc :: [(Label, Type)] -> Type -> Doc Ann
        listToDoc [] REmpty = open <> close
        listToDoc [] rest = open <+> tailToPs rest <+> close
        listToDoc ts rest =
            sep
                (zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else comma) nm ty)
                    ts [0 :: Int ..] ++ tail' rest ++ [close])
            where
                tail' REmpty = []
                tail' _      = [tailToPs rest]

        toList :: [(Label, Type)] -> Type -> ([(Label, Type)], Type)
        toList tys (RCons name ty row) = toList ((name, ty) :tys) row
        toList tys r                   = (reverse tys, r)

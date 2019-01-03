{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Literals where

import           Prelude                          (Either (Left, Right), fmap,
                                                   ($), (==))
import Data.Monoid (mempty)
import qualified Data.Text as T
-- import           Language.PureScript.Format.Pretty
import           Language.PureScript.Format.Pretty as PP
import           Language.PureScript.AST.Literals  (Literal (..))
import Language.PureScript.Format.Symbols
-- import qualified Language.PureScript.PSString      as String
import Data.List (zip, length)

instance PrettyAnn a => PrettyAnn (Literal a) where
    pretty (NumericLiteral integerOrDouble) = case integerOrDouble of
      Left integer' -> pretty integer'
      Right number  -> pretty number
    pretty (StringLiteral s) = string $ "\"" <> pretty s <> "\""
    pretty (CharLiteral c) = pretty $ T.pack ['\'', c, '\'']
    pretty (BooleanLiteral b) = if b then "true" else "false"
    pretty (ArrayLiteral vs) =
        collection lbracket rbracket comma (fmap pretty vs)
    pretty (ObjectLiteral os) = 
        group (lbrace
        <> vsep (fmap (\(i, (key, val)) -> 
                (if i == 0 then mempty else comma)
                <+> pretty key 
                <> colonSym
                <+> pretty val
            ) (zip [0..length os] os))
        <> softline
        <> rbrace) 
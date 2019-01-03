module Language.PureScript.Format.Module
    ( pprintModule
    ) where

import           Data.List                               (sortBy)
import           Prelude                                 hiding ((<$>))

-- import qualified Language.PureScript                     as P
import           Language.PureScript.AST.Declarations

import           Language.PureScript.Format.Pretty
import           Language.PureScript.Names

import           Data.Monoid (mempty)
import           Language.PureScript.Format.Comments     ()
import           Language.PureScript.Format.Declarations ()
import           Language.PureScript.Format.Names        ()
import           Language.PureScript.Format.Keywords
import           Language.PureScript.Format.Collections

vSpace :: Doc Ann
vSpace = hardline <> hardline

pprintModule :: Module -> Doc Ann
pprintModule (Module _sourceSpan comments moduleName declarations exports) =
    comments'
    <> moduleDecl moduleName exports
    <> vSpace
    <> vsep (fmap pretty (sortBy sorter imports'))
    <> vSpace
    <> pprintDecls decls'
    where
        -- (imports', decls') = span isImport declarations
        (imports', decls') = span isImport declarations
        isImport decl = case decl of
            ImportDeclaration{} -> True
            _                   -> False
        sorter _decl1 _decl2 =
            --todo
            GT
        comments'
            | null comments = mempty
            | otherwise = vsep (fmap pretty comments) <> hardline

-- | Pretty prints the module declaration
-- @module Foo (bar, baz) where@.
moduleDecl :: ModuleName -> Maybe [DeclarationRef] -> Doc Ann
moduleDecl moduleName exports = group $
    moduleKw <+> pretty moduleName
    <> (indent (line <> (moduleExports exports) <+> whereKw))
 where
    exportOpen = flatAlt "( " "("
    
    moduleExports :: Maybe [DeclarationRef] -> Doc Ann
    moduleExports = \case
        Nothing -> mempty
        Just refs ->
            (
            exportOpen
            <> vsep (fmap (\(i, x) ->
                if i == 0 then
                    pretty x
                else
                    comma <> space <> pretty x
                ) (zip [0..(length refs)] refs))
            <> line' <> rparen
            )

pprintDecls :: [Declaration] -> Doc Ann
pprintDecls decls = case decls of
    [] -> mempty
    [decl] -> pretty decl
    x@(TypeDeclaration{}) : y@(ValueDeclaration{}) : xs ->
        pretty x <> hardline <> pretty y <> hardline <> pprintDecls xs
    x : xs ->
        pretty x <> hardline <> pprintDecls xs

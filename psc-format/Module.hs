module Module
    ( pprintModule
    ) where

import Prelude hiding ((<$>))
import Data.List (sortBy)

import qualified Language.PureScript as P
import Language.PureScript.AST.Declarations
import Language.PureScript.Parser.Declarations

import Text.PrettyPrint.ANSI.Leijen

import Config
import Names ()
import Declarations ()
import Comments ()

vSpace :: Doc
vSpace = hardline <> hardline

pprintModule :: Module -> Doc
pprintModule (Module _sourceSpan comments moduleName declarations exports) =
    comments'
    <> text "module"
    <+> pretty moduleName
    <> exports'
    <+> text "where"
    <> vSpace
    <> vcat (fmap pretty (sortBy sorter imports'))
    <> vSpace
    <> pprintDecls decls'
    <> hardline
    where
        (imports', decls') = span isImport declarations
        isImport decl = case decl of
            PositionedDeclaration _ _ ImportDeclaration{} -> True
            _ -> False
        sorter decl1 decl2 = case (decl1, decl2) of
            (PositionedDeclaration _ _ (ImportDeclaration moduleName1 _ _), PositionedDeclaration _ _ (ImportDeclaration moduleName2 _ _))
                | name1 == "Prelude" -> LT
                | otherwise -> name1 `compare` P.runModuleName moduleName2
                where
                    name1 = P.runModuleName moduleName1
            _ -> GT
        comments'
            | null comments = empty
            | otherwise = vsep (fmap pretty comments) <> hardline
        exports' = case exports of
            Nothing -> empty
            Just refs ->
                space
                <$> indent indentationLevel (makeList (fmap pretty refs))
        makeList docs = case docs of
            [] -> parens line
            x : xs ->
                parens (space <> vcat (x : fmap ((comma <> space) <>) xs) <> line)

pprintDecls :: [Declaration] -> Doc
pprintDecls decls = case decls of
    [] -> empty
    [decl] -> pretty decl
    x@(PositionedDeclaration _ _ TypeDeclaration{}) : y@(PositionedDeclaration _ _ ValueDeclaration{}) : xs ->
        pretty x <$> pretty y <$> hardline <> pprintDecls xs
    x : xs ->
        pretty x <$> hardline <> pprintDecls xs

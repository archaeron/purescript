{-# OPTIONS_GHC -fno-warn-orphans #-}

module Declarations
    (
    ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen as PP

import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Operators
import Language.PureScript.Names
import Language.PureScript.Environment (DataDeclType (..))
import Language.PureScript.Pretty.Values (prettyPrintBinder)

import Names ()
import Types
import Binder ()
import Config
import Literals ()
import Pretty
import Comments ()

printAbs :: Ident -> Expr -> Bool -> Doc
printAbs arg val isFirstAbs =
    case (val, isFirstAbs) of
        (Abs (Left argN) valN, True) ->
            backslash <> text (showIdent arg) <+> printAbs argN valN False
        (_, True) ->
            backslash <> text (showIdent arg) <+> text "->" <+> pretty val
        _ ->
            text (showIdent arg) <+> text "->" <+> pretty val

instance Pretty DeclarationRef where
    pretty (TypeRef properName ns) =
        pretty properName <> constructors
        where
            constructors = case ns of
                Nothing ->
                    text "(..)"
                Just [] ->
                    PP.empty
                Just properNames ->
                    tupled (fmap pretty properNames)
    pretty (TypeOpRef (OpName opName)) = text "type" <+> parens (pretty opName)
    pretty (ValueRef ident) = pretty ident
    pretty (ValueOpRef opName) = parens $ pretty opName
    pretty (TypeClassRef properName) = text "class" <+> pretty properName
    pretty (TypeInstanceRef _ident) = text "TypeInstanceRef"
    pretty (ModuleRef moduleName) = text "module" <+> pretty moduleName
    pretty (ReExportRef _moduleName _ref) = text "ReExportRef"
    pretty (PositionedDeclarationRef _sourceSpan comments declarationRef) =
        comments' <> pretty declarationRef
        where
            comments'
                | null comments = empty
                | otherwise = vsep (fmap pretty comments) <> hardline

    prettyList = prettyTupled . fmap pretty

instance Pretty Declaration where
    pretty decl = case decl of
        DataDeclaration dataDeclType properName lT constructors ->
            nest indentationLevel
                ( text dataLabel
                <+> pretty properName
                <> leftTypes
                <> constructors'
                )
            where
                dataLabel =
                    case dataDeclType of
                        Data -> "data"
                        Newtype -> "newtype"
                leftTypes
                    | null lT = empty
                    | otherwise = space <> ppTypeList lT
                constructors' = case constructors of
                    [] -> empty
                    [x] -> space <> equals <+> formatConstructor x
                    x : xs ->
                        empty
                        <$> equals
                        <+> formatConstructor x
                        <$> vsep (fmap (\c -> char '|' <+> formatConstructor c) xs)
                formatConstructor (n, ts) = pretty n <> ts'
                    where
                        ts'
                            | null ts = empty
                            | otherwise = space <> hsep (fmap pretty ts)
        DataBindingGroupDeclaration _declarations ->
            text "DataBindingGroupDeclaration"
        TypeSynonymDeclaration propertyName params typ ->
            nest indentationLevel
                ( text "type"
                <+> pretty propertyName
                <> params'
                <+> equals
                <+> pretty typ
                )
            where
                params'
                    | null params = empty
                    | otherwise = space <> ppTypeList params
        TypeDeclaration ident typ ->
            nest indentationLevel (pretty ident </> text "::" <+> pretty typ)
        ValueDeclaration ident _nameKind binders expr ->
            pretty ident <> binders' <> body
            where
                body = case expr of
                    Left exprs ->
                        empty
                        <$> indent indentationLevel (vsep (fmap printGuardExpr exprs))
                    Right expression ->
                        nest indentationLevel
                          ( space
                          <> equals
                          </> pretty expression
                          )
                binders' = case binders of
                    [] -> PP.empty
                    _ -> space <> prettyList binders
                printGuardExpr (guard, expr') =
                    nest indentationLevel
                      ( char '|'
                      <+> pretty guard
                      <+> equals
                      </> pretty expr'
                      )
        BindingGroupDeclaration _is -> text "BindingGroupDeclaration"
        ExternDeclaration tdent typ ->
            nest indentationLevel
              ( text "foreign"
              <+> text "import"
              <+> pretty tdent
              </> text "::"
              <+> pretty typ
              )
        ExternDataDeclaration properName kin ->
            nest indentationLevel
              ( text "foreign"
              <+> text "import"
              <+> text "data"
              <+> pretty properName
              </> text "::"
              <+> pretty kin
              )
        FixityDeclaration fixity ->
            case fixity of
                Left valueFixity -> pretty valueFixity
                Right typeFixity -> pretty typeFixity
        ImportDeclaration moduleName importDeclarationType qualifiedModuleName ->
            text "import" <+> pretty moduleName <> importBody
            where
                importBody = case qualifiedModuleName of
                    Nothing -> pretty importDeclarationType
                    Just qualifiedModuleName' -> pretty importDeclarationType <+> text "as" <+> pretty qualifiedModuleName'
        TypeClassDeclaration properName a constraints declarations ->
            text "class"
            <> constraints'
            <+> pretty properName
            <+> ppTypeList a
            <+> text "where"
            <$> indent indentationLevel (vsep (fmap pretty declarations))
            where
                constraints'
                    | null constraints = empty
                    | length constraints == 1 =
                        space
                        <> pretty (head constraints)
                        <+> text "<="
                    | otherwise =
                        space
                        <> parens (listify (fmap pretty constraints))
                        <+> text "<="
        TypeInstanceDeclaration ident constraints qualified types body ->
            case body of
                DerivedInstance -> text "derive" <+> header
                ExplicitInstance declarations ->
                    header
                    <+> text "where"
                    <$> indent indentationLevel (vsep (fmap pretty declarations))
            where
                header =
                    text "instance"
                    <+> pretty ident
                    <+> text "::"
                    <> constraints'
                    <+> pretty qualified
                    <+> pretty (head types)
                constraints'
                    | null constraints = empty
                    | length constraints == 1 =
                        space
                        <> pretty (head constraints)
                        <+> text "=>"
                    | otherwise =
                        space
                        <> parens (listify (fmap pretty constraints))
                        <+> text "=>"
        PositionedDeclaration _sourceSpan comments declaration ->
            comments' <> pretty declaration
            where
                comments'
                    | null comments = empty
                    | otherwise = vsep (fmap pretty comments) <> hardline

instance Pretty ValueFixity where
    pretty (ValueFixity fixity (Qualified module' identOrConstructor) opName) =
        pretty fixity <+> pretty name <+> text "as" <+> pretty opName
        where
            name = case identOrConstructor of
                Left ident -> Qualified module' (pretty ident)
                Right constructor -> Qualified module' (pretty constructor)

instance Pretty TypeFixity where
    pretty (TypeFixity fixity typeName opName) =
        pretty fixity <+> pretty typeName <+> text "as" <+> pretty opName

instance Pretty Fixity where
    pretty (Fixity associativity precedence) =
        text (showAssoc associativity) <+> pretty precedence

instance Pretty Expr where
    pretty (Literal literal) = pretty literal
    pretty (UnaryMinus expr) = text "-" <> pretty expr
    pretty (BinaryNoParens op left right) = pretty left <+> pretty op <+> pretty right
    pretty (Parens expr) = parens $ pretty expr
    pretty (ObjectGetter s) = text ("_." ++ s)
    pretty (Accessor field expr) = pretty expr <> dot <> pretty field
    pretty (ObjectUpdate o ps) =
        pretty o
        <+> lbrace
        <+> listify (fmap (\(key, val) -> text key <+> equals <+> pretty val) ps)
        <+> rbrace
    pretty (Abs (Left arg) val) = printAbs arg val True
    pretty (Abs (Right arg) val) =
        text ('\\' : prettyPrintBinder arg)
        <+> text "->"
        <$> indent indentationLevel (pretty val)
    pretty (App expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Var qualified) = pretty qualified
    pretty (Op qualified) = pretty qualified
    pretty (IfThenElse expr1 expr2 expr3) =
        sep
          [ text "if" <+> pretty expr1
          , text "then" <+> pretty expr2
          , text "else" <+> pretty expr3
          ]
    pretty (Constructor qualified) = pretty qualified
    pretty (Case exprs caseAlternatives) =
        line
        <> text "case"
        <+> listify (fmap pretty exprs)
        <+> text "of"
        <$> PP.indent indentationLevel (vsep (fmap pretty caseAlternatives))
    pretty (TypedValue _bool expr typ) =
        pretty expr <+> text "::" <+> pretty typ
    pretty (Let decls expr) =
        pretty expr
        <$> text "where"
        <$> vsep (fmap pretty decls)
    pretty (Do doNotationElements) =
        line
        <> text "do"
        <$> indent indentationLevel (vsep (fmap pretty doNotationElements))
    pretty (TypeClassDictionaryConstructorApp _qualified _expr) =
        text "TypeClassDictionaryConstructorApp"
    pretty (TypeClassDictionary _constraint _a) = text "TypeClassDictionary"
    pretty (TypeClassDictionaryAccessor _qualified _ident) =
       text "TypeClassDictionaryAccessor"
    pretty (SuperClassDictionary _qualified _types) =
        text "SuperClassDictionary"
    pretty AnonymousArgument = text "_"
    pretty (Hole hole) = text ("?" ++ hole)
    pretty (PositionedValue _sourceSpan comments expr) =
      comments' <> pretty expr
      where
          comments'
              | null comments = empty
              | otherwise = vsep (fmap pretty comments) <> hardline

instance Pretty ImportDeclarationType where
    pretty Implicit = PP.empty
    pretty (Explicit refs) = PP.space <> (prettyTupled . fmap pretty $ refs)
    pretty (Hiding refs) = text "hiding" <+> (tupled . fmap pretty $ refs)

instance Pretty DoNotationElement where
    pretty (DoNotationValue expr) = pretty expr
    pretty (DoNotationBind binder expr) = pretty binder <+> text "<-" <+> pretty expr
    pretty (DoNotationLet declarations) =
        nest 4 (text "let" <+> vsep (fmap pretty declarations))
    pretty (PositionedDoNotationElement _sourceSpan comments element) =
      comments' <> pretty element
      where
          comments'
              | null comments = empty
              | otherwise = vsep (fmap pretty comments) <> hardline

instance Pretty CaseAlternative where
    pretty (CaseAlternative caseAlternativeBinders' caseAlternativeResult') =
        case caseAlternativeResult' of
            Left exprs ->
                binders
                <$> indent indentationLevel (vsep (fmap printGuardExpr exprs))
            Right expr ->
                nest indentationLevel (binders <+> text "->" </> pretty expr)
        where
            binders = listify (fmap pretty caseAlternativeBinders')
            printGuardExpr (guard, expr) =
                text "|" <+> pretty guard <+> text "->" <+> pretty expr

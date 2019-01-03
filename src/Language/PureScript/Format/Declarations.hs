{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Format.Declarations
    (
    ) where

import           Prelude

import           Language.PureScript.Format.Pretty    (pretty, (<+>))
import qualified Language.PureScript.Format.Pretty    as PP

import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.Operators
import           Language.PureScript.Environment      (DataDeclType (..))
import           Language.PureScript.Names

import           Data.Monoid                          ((<>))
import           Language.PureScript.Format.Binder    ()
import           Language.PureScript.Format.Comments  ()
import           Language.PureScript.Format.Literals  ()
import           Language.PureScript.Format.Names     ()
import           Language.PureScript.Format.Types
import           Language.PureScript.Format.Keywords
import           Language.PureScript.Format.Collections
import           Language.PureScript.Format.Symbols
import qualified Language.PureScript as P
import qualified Language.PureScript.AST.Binders as B
-- import qualified Data.Text as T

-- parses lambda expression and returns (arguments, body) of expr
printAbs :: B.Binder -> Expr -> PP.Doc PP.Ann
printAbs binder expr =
    backslashSym
    <> pretty binder
    <+> case expr of
        Abs binder2 expr2 ->
            printAbs' binder2 expr2
        _ ->
            forwardArrowSym
            <+> pretty expr

printAbs' :: B.Binder -> Expr -> PP.Doc PP.Ann
printAbs' binder expr =
    case expr of
        Abs binder2 expr2 ->
            pretty binder
            <+> printAbs' binder2 expr2
        _ -> pretty binder
            <> PP.space
            <> forwardArrowSym
            <+> pretty expr


instance PP.PrettyAnn DeclarationRef where
    pretty (TypeRef _span properName maybeCtors) =
        pretty properName <> constructors
        where
            constructors = case maybeCtors of
                Nothing ->
                    "(..)"
                Just [] ->
                    mempty
                Just properNames ->
                    PP.tupled $ fmap pretty properNames

    pretty (TypeOpRef _ident (OpName opName)) = typeKw <+> PP.parens (pretty opName)
    pretty (ValueRef _idenSpan ident) = pretty ident
    pretty (ValueOpRef _idenSpan opName) = PP.parens $ pretty opName
    pretty (TypeClassRef _idenSpan properName) = classKw <+> pretty properName
    pretty (TypeInstanceRef _idenSpan _ident) = PP.dbg "<TypeInstanceRef>"
    pretty (ModuleRef _idenSpan moduleName) = moduleKw <+> pretty moduleName
    pretty (ReExportRef _idenSpan _moduleName _ref) = PP.dbg "<ReExportRef>"
    -- pretty (PositionedDeclarationRef _sourceSpan comments declarationRef) =
    --     comments' <> pretty declarationRef
    --     where
    --         comments'
    --             | null comments = mempty
    --             | otherwise =
    --                 PP.hvsep (fmap pretty comments) <> PP.hardLine
    pretty (KindRef _idenSpan _) = PP.dbg "<KindRef>"

instance PP.PrettyAnn Declaration where
    pretty decl = case decl of
        DataDeclaration _sourceAnn dataDeclType properName lT constructors ->
            PP.hardline
            <> PP.indent
                ( dataLabel
                <+> pretty properName
                <> leftTypes
                <> PP.line
                <> constructors'
                )
            where
                dataLabel =
                    case dataDeclType of
                        Data    -> dataKw
                        Newtype -> newtypeKw
                leftTypes
                    | null lT = mempty
                    | otherwise = PP.space <> ppTypeList lT
                constructors' = case constructors of
                    [] -> mempty
                    [x] -> PP.space <> equalsSym <+> formatConstructor x
                    x : xs ->
                        mempty
                        <> equalsSym
                        <+> formatConstructor x
                        <> PP.hardline
                        <> PP.vsep (fmap (\c -> pipeSym <+> formatConstructor c) xs)
                formatConstructor (n, ts) = pretty n <> ts'
                    where
                        ts'
                            | null ts = mempty
                            | otherwise = PP.space <> PP.hsep (fmap pretty ts)
        DataBindingGroupDeclaration _declarations ->
            PP.dbg "<DataBindingGroupDeclaration>"
        TypeSynonymDeclaration _sourceAnnot propertyName params typ ->
            PP.hardline <>
            (PP.indent $
                typeKw
                <+> pretty propertyName
                <> params'
                <+> equalsSym
                <+> PP.group (pretty typ))
            where
                params'
                    | null params = mempty
                    | otherwise = PP.space <> ppTypeList params
        TypeDeclaration td ->
            PP.hardline <> PP.indent (pretty (tydeclIdent td) <+> doublecolonSym <+> pretty (tydeclType td))

        ValueDeclaration valueDeclarationData ->
            prettyValueDeclarationData valueDeclarationData
        BindingGroupDeclaration _is -> "<BindingGroupDeclaration>"
        ExternDeclaration _sourceSpan tdent typ ->
            PP.indent
              ( foreignKw
              <+> importKw
              <+> pretty tdent
              <> doublecolonSym
              <+> pretty typ
              )
        ExternDataDeclaration _sourceSpan properName kin ->
            PP.indent
              ( foreignKw
              <+> importKw
              <+> dataKw
              <+> pretty properName
              <> doublecolonSym
              <+> pretty kin
              )
        FixityDeclaration _sourceSpan fixity ->
            case fixity of
                Left valueFixity -> pretty valueFixity
                Right typeFixity -> pretty typeFixity
        ImportDeclaration _sourceSpan moduleName importDeclarationType qualifiedModuleName ->
            importKw <+> pretty moduleName <> importBody
            where
                importBody = case qualifiedModuleName of
                    Nothing -> pretty importDeclarationType
                    Just qualifiedModuleName' ->
                        pretty importDeclarationType <+> asKw <+> pretty qualifiedModuleName'
        TypeClassDeclaration _sourceSpan className params constraints funcDeps declarations ->
            classKw
            <> _constraints'
            -- this crashes, see examples/passing/Applicative.purs
            <+> pretty className
            <+> ppTypeList params
            <+> printFuncDeps funcDeps
            <> whereExpr
            where
                whereExpr =
                    case declarations of
                        [] -> mempty
                        dcls ->
                            PP.space
                            <> whereKw
                            <+> PP.indent (PP.sep (fmap pretty dcls))
                _constraints' =
                    case constraints of
                        [] -> mempty
                        [c] ->
                            PP.space
                            <> pretty c
                            <+> leqSym
                        cs ->
                            PP.space
                            <> tupleCollection (fmap pretty cs)
                            <+> leqSym
                printFuncDeps [] = mempty
                printFuncDeps _xs =
                    pipeSym
                    <+> PP.collection mempty mempty PP.comma (map (\(P.FunctionalDependency from to) ->
                        pretty (fst (params !! head from))
                        <+> forwardArrowSym
                        <+> pretty (fst (params !! head to))
                        ) funcDeps)

        TypeInstanceDeclaration _sourceAnn idents _chainIndex name constraints className types body ->
            case body of
                DerivedInstance -> deriveKw <+> header
                ExplicitInstance declarations ->
                    case declarations of
                        [] -> header
                        _ ->
                            header
                            <+> PP.group (whereKw
                            <+> PP.indent
                                (PP.hardline
                                <> PP.group (PP.sep $ fmap pretty declarations)))
                NewtypeInstance -> PP.dbg "<NewtypeInstance>"
                NewtypeInstanceWithDictionary _ -> PP.dbg "<NewtypeInstanceWithDictionary>"
            where
                header =
                    instanceKw
                    <+> PP.sep (fmap pretty idents)
                    <+> doublecolonSym
                    <> constraints'
                    <+> PP.pretty className
                    <+> PP.prettyList types
                constraints' =
                    case constraints of
                        [] -> mempty
                        [c] ->
                            PP.space
                            <> pretty c
                            <+> doubleForwardArrowSym
                        cs ->
                            PP.space
                            <> PP.parens (PP.sep (fmap pretty cs))
                            <+> doubleForwardArrowSym
        -- PositionedDeclaration _sourceSpan comments declaration ->
        --     comments' <> pretty declaration
        --     where
        --         comments'
        --             | null comments = mempty
        --             | otherwise =
        --                 PP.vsep (fmap pretty comments) <> PP.hardLine
        BoundValueDeclaration _sourceSpan _ _ -> "<BoundValueDeclaration>"
        ExternKindDeclaration _sourceSpan _ -> "<ExternKindDeclaration>"

prettyValueDeclarationData :: ValueDeclarationData [GuardedExpr] -> PP.Doc PP.Ann
prettyValueDeclarationData (ValueDeclarationData sourceAnn ident declName binders expr) =
    pretty ident <> binders' <> (PP.sep $ fmap PP.pretty expr)
  where
    binders' = case binders of
        [] -> mempty
        _  -> PP.space <> PP.prettyList binders

instance PP.PrettyAnn ValueFixity where
    pretty (ValueFixity fixity (Qualified module' identOrConstructor) opName) =
        pretty fixity <+> PP.dbg name <+> atKw <+> pretty opName
        where
            name = case identOrConstructor of
                Left ident        ->
                    Qualified module' (pretty ident)
                Right constructor ->
                    Qualified module' (pretty constructor)

instance PP.PrettyAnn TypeFixity where
    pretty (TypeFixity fixity typeName opName) =
        pretty fixity <+> typeKw <+> pretty typeName <+> asKw <+> pretty opName

instance PP.PrettyAnn Fixity where
    pretty (Fixity associativity precedence) =
        PP.dbg (showAssoc associativity) <+> pretty precedence

instance PP.PrettyAnn Expr where

    pretty (Literal literal) =
        pretty literal
    pretty (Proxy p) = PP.dbg p
    pretty (Ado a b) = PP.dbg a <+> PP.dbg b
    pretty (UnaryMinus expr) = dashSym <> pretty expr
    pretty (BinaryNoParens op left right) =
        pretty left <+> printOp op <+> pretty right
        where
        printOp (Op (Qualified mN a)) =
            case mN of
                Just m -> pretty m <> PP.dot <> pretty a
                Nothing -> PP.operator $ pretty a
        -- printOp (BinaryNoParens _ _ _) = PP.dbg "<BinaryNoParens>"
        -- printOp (Parens _) = PP.dbg "<Parens>"
        -- printOp (Literal _) = PP.dbg "<Literal>"
        -- printOp (UnaryMinus _) = PP.dbg "<UnaryMinus>"
        -- printOp (Accessor _ _) = PP.dbg "<Accessor>"
        -- printOp (ObjectUpdate _ _) = PP.dbg "<ObjectUpdate>"
        -- printOp (ObjectUpdateNested _ _) = PP.dbg "<ObjectUpdateNested>"
        -- printOp (Abs _ _) = PP.dbg "<Abs>"
        -- printOp (App _ _) = PP.dbg "<App>"
        -- printOp (Var _) = PP.dbg "<Var>"
        -- printOp (IfThenElse _ _ _) = PP.dbg "<IfThenElse>"
        -- printOp (Constructor _) = PP.dbg "<Constructor>"
        -- printOp (Case _ _) = PP.dbg "<Case>"
        -- printOp (TypedValue _ _ _) = PP.dbg "<TypedValue>"
        -- printOp (Let _ _) = PP.dbg "<Let>"
        -- printOp (Do _) = PP.dbg "<Do>"
        -- printOp (TypeClassDictionaryConstructorApp _ _) = PP.dbg "<TypeClassDictionaryConstructorApp>"
        -- printOp (TypeClassDictionary _ _ _) = PP.dbg "<TypeClassDictionary>"
        -- printOp (TypeClassDictionaryAccessor _ _) = PP.dbg "<TypeClassDictionaryAccessor>"
        -- printOp (DeferredDictionary _ _) = PP.dbg "<UnaDeferredDictionaryryMinus>"
        -- printOp (Hole _) = PP.dbg "<Hole>"
        -- printOp AnonymousArgument = PP.dbg "<AnonymousArgument>"
        printOp (PositionedValue _ _mN a) =
            printOp a
        printOp expr =
            PP.surround (PP.pretty expr) backtickSym backtickSym

    pretty (Parens expr) =
        PP.parens $ pretty expr
    pretty (Accessor field expr) = pretty expr <> PP.dot <> pretty field
    pretty (ObjectUpdate o ps) =
        pretty o
        <+> PP.lbrace
        <+> PP.sep (fmap (\(key, val) -> pretty key <+> equalsSym <+> pretty val) ps)
        <+> PP.rbrace
    pretty (ObjectUpdateNested _ _) = PP.dbg "<ObjectUpdateNested>"
    pretty (Abs arg exprBody) =
        printAbs arg exprBody
    pretty (App expr1 expr2) =
        pretty expr1
        <+> pretty expr2
    pretty (Var qualified) = pretty qualified
    pretty (Op qualified) =
        prettyOp qualified
        where
            prettyOp (Qualified mN n) =
                case mN of
                    Just m ->
                        pretty m <> PP.dot <> PP.lparen <> pretty n <> PP.rparen
                    Nothing ->
                        PP.lparen <> pretty n <> PP.rparen
    pretty (IfThenElse expr1 expr2 expr3) =
        PP.vsep
          [ ifKw <+> pretty expr1
          , thenKw <+> pretty expr2
          , elseKw <+> pretty expr3
          ]
    pretty (Constructor c) =
        pretty c

        -- pretty qualified
    pretty (Case exprs caseAlternatives) =
        caseKw
        <+> case exprs of
                [e] -> pretty e
                _ -> PP.collection mempty mempty PP.comma (fmap pretty exprs)
        <+> ofKw
        <> PP.indent (
            PP.hardline
            <> (PP.vsep (pretty <$> caseAlternatives)))
    pretty (TypedValue _bool expr typ) =
        pretty expr <+> doublecolonSym <+> pretty typ
    pretty (Let decls expr) =
        PP.group (PP.indent (PP.group (pretty expr)))
        <> PP.halfindent (PP.hardline <> whereKw)
        <> PP.indent (PP.hardline <+> PP.group (PP.vsep (fmap pretty decls)))
    pretty (Do doNotationElements) =
        doKw <>
            (PP.indent $ PP.vsep (PP.hardline : (fmap pretty doNotationElements)))
    pretty (TypeClassDictionaryConstructorApp _qualified _expr) =
        PP.dbg "<TypeClassDictionaryConstructorApp>"
    pretty (TypeClassDictionary _constraint _a _errorMessageHints) = PP.dbg "<TypeClassDictionary>"
    pretty (TypeClassDictionaryAccessor _qualified _ident) =
       PP.dbg "<TypeClassDictionaryAccessor>"
    pretty AnonymousArgument = underscoreSym
    pretty (Hole hole) = questionmarkSym <> pretty hole
    pretty (PositionedValue _sourceSpan comments expr) =
      comments'
      <> pretty expr
      where
          comments'
              | null comments = mempty
              | otherwise = PP.vsep (fmap pretty comments) <> PP.hardline
    pretty (DeferredDictionary _ _) = PP.dbg "<DeferredDictionary>"

instance PP.PrettyAnn ImportDeclarationType where
    pretty Implicit        = mempty
    pretty (Explicit refs) = PP.space <> (PP.tupled . fmap pretty $ refs)
    pretty (Hiding refs)   = PP.space <> hidingKw <+> (PP.tupled . fmap pretty $ refs)

instance PP.PrettyAnn DoNotationElement where
    pretty (DoNotationValue expr) = pretty expr
    pretty (DoNotationBind binder expr) = pretty binder <+> backwardArrowSym <+> (PP.indent $ pretty expr)
    pretty (DoNotationLet declarations) =
            letKw
            <+> PP.group (PP.sep (PP.hardline : (fmap ppDecl declarations)))
        where
            ppDecl decl =
                pretty decl

    pretty (PositionedDoNotationElement _sourceSpan comments element) =
      comments' <> pretty element
      where
          comments'
              | null comments = mempty
              | otherwise = PP.sep (fmap pretty comments) <> PP.hardline

instance PP.PrettyAnn CaseAlternative where
    pretty (CaseAlternative caseAlternativeBinders guardedExpressions) =
        binders
        <+> foldl (\acc x -> acc <> printGuardedExpr x) mempty guardedExpressions
        where
            binders = PP.collection mempty mempty PP.comma $ fmap pretty caseAlternativeBinders
            printGuardedExpr (GuardedExpr guards expr) =
                case guards of
                    [] -> PP.indent (forwardArrowSym <+> pretty expr)
                    _  ->
                        PP.group (PP.indent (pipedCollection (pretty <$> guards)))
                        <+> forwardArrowSym
                        <+> pretty expr
                        <> PP.hardline

instance PP.PrettyAnn GuardedExpr where
     pretty (GuardedExpr guards expr) =
        case guards of
            [] ->
                PP.space
                <> equalsSym
                <+> (pretty expr)
                <> PP.hardline
            guards'  ->
                pipedCollection (pretty <$> guards')
                <+> equalsSym
                <+> pretty expr



instance PP.PrettyAnn Guard where
     pretty (ConditionGuard guard) = PP.pretty guard
     pretty (PatternGuard _a _b) =
            pretty _a <+> backwardArrowSym <+> pretty _b


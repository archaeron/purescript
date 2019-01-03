{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module TestFormat where

import           Control.Applicative
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Text                  as T
import           Data.Text.Lazy             as TL
import           Data.Traversable           (for)
import qualified Language.PureScript        as P
import qualified Language.PureScript.Format as Format
import           Prelude
-- import           System.Directory           (getCurrentDirectory)
import           System.FilePath            (FilePath)
import           System.FilePath.Glob       (glob)
import           System.IO.UTF8             (readUTF8File, writeUTF8File)
import Language.PureScript.AST as A
import Text.Pretty.Simple

parse :: forall m. MonadError P.MultipleErrors m => String -> m [(FilePath, P.Module)]
parse file = P.parseModulesFromFiles id [("Main", T.pack file)]

dummyPos :: P.SourcePos
dummyPos = P.SourcePos 0 0

dummySrcSpan :: P.SourceSpan
dummySrcSpan = P.SourceSpan "-" dummyPos dummyPos

dummySrcAnn :: A.SourceAnn
dummySrcAnn = (dummySrcSpan, [])

withDefaultSourceAnnot :: P.Declaration -> P.Declaration
withDefaultSourceAnnot decl =
  case decl of
    P.DataDeclaration _sourceAnn dataDeclType properName lT constructors ->
      P.DataDeclaration dummySrcAnn dataDeclType properName lT constructors
    P.DataBindingGroupDeclaration _declarations ->
      P.DataBindingGroupDeclaration _declarations
    P.TypeSynonymDeclaration _sourceAnn propertyName params typ ->
      P.TypeSynonymDeclaration dummySrcAnn propertyName params typ
    P.TypeDeclaration typeDeclaration ->
      P.TypeDeclaration $ typeDeclaration
        { tydeclSourceAnn = dummySrcAnn
        }
    P.ValueDeclaration valueDeclaration ->
      P.ValueDeclaration $ valueDeclaration
        { valdeclSourceAnn = dummySrcAnn
        }
    P.BindingGroupDeclaration is ->
      P.BindingGroupDeclaration is
    P.ExternDeclaration _sourceAnn tdent typ ->
      P.ExternDeclaration dummySrcAnn tdent typ
    P.ExternDataDeclaration _sourceAnn properName kin ->
      P.ExternDataDeclaration dummySrcAnn properName kin
    P.FixityDeclaration _sourceAnn fixity ->
      P.FixityDeclaration dummySrcAnn fixity
    P.ImportDeclaration _sourceAnn moduleName importDeclarationType qualifiedModuleName ->
      P.ImportDeclaration dummySrcAnn moduleName importDeclarationType qualifiedModuleName
    P.TypeClassDeclaration _sourceAnn className params constraints _funcDeps declarations ->
      P.TypeClassDeclaration dummySrcAnn className params constraints _funcDeps declarations
    P.TypeInstanceDeclaration _sourceAnn idents chainIndex name constraints className types body ->
      P.TypeInstanceDeclaration dummySrcAnn idents chainIndex name constraints className types body
    P.BoundValueDeclaration _sourceAnn a b ->
      P.BoundValueDeclaration dummySrcAnn a b
    P.ExternKindDeclaration _sourceAnn a ->
      P.ExternKindDeclaration dummySrcAnn a

replacePositionedType :: P.Type -> P.Type
replacePositionedType (P.TypeWildcard _) = P.TypeWildcard dummySrcSpan
replacePositionedType (P.Skolem t i s _) = P.Skolem t i s Nothing
replacePositionedType (P.TypeApp t1 t2) = P.TypeApp (replacePositionedType t1) (replacePositionedType t2)
replacePositionedType t = t


removePositionInformation :: [(FilePath, P.Module)] -> [P.Declaration]
removePositionInformation v =
  let
    (f, _, _) = P.everywhereOnValues replaceDecl replaceExpr replaceBinder
    replaceDecl decl =
      withDefaultSourceAnnot decl
    replaceExpr expr =
      case expr of
          P.PositionedValue _ comments expression ->
            case expression of
              --  special case for positioned do notation elements, also clear position
              P.Do listOfDoNotationElements ->
                P.Do $
                  (\(P.PositionedDoNotationElement _ comment element) ->
                    P.PositionedDoNotationElement dummySrcSpan comment element
                  ) <$> listOfDoNotationElements
              _ -> P.PositionedValue dummySrcSpan comments expression
          _ -> expr

    replaceBinder binder =
      case binder of
          P.PositionedBinder _ comments pbinder ->
            P.PositionedBinder dummySrcSpan comments (replaceBinder pbinder)
          P.TypedBinder type' binder' ->
            P.TypedBinder (replacePositionedType type') binder'
          P.ConstructorBinder pName binders ->
            P.ConstructorBinder pName (fmap replaceBinder binders)
          -- P.LiteralBinder expr ->
          --   P.LiteralBinder (replaceExpr expr)
          P.BinaryNoParensBinder b1 b2 b3 ->
            P.BinaryNoParensBinder (replaceBinder b1) (replaceBinder b2) (replaceBinder b3)
          P.ParensInBinder b1 ->
            P.ParensInBinder (replaceBinder b1)
          P.NamedBinder i b1 ->
            P.NamedBinder i (replaceBinder b1)
          _ ->
            binder
  in
    case v of
      [v_] -> f <$> moduleToDecls (snd v_)
      _ -> []

moduleToDecls :: P.Module -> [P.Declaration]
moduleToDecls (P.Module _ _ _mn decls _exps) = P.flattenDecls decls

roundTrip :: String -> String -> IO ()
roundTrip filename file =
    case parse file of
        Right v -> do
            let formatted = Format.format filename file
            case parse formatted of
              Right v_ -> do
                let origAST = removePositionInformation v
                let formattedAST = removePositionInformation v_
                if origAST /= formattedAST then do
                    writeFile (filename ++ ".formatted") formatted
                    writeFile (filename ++ "-formatted.ast") (TL.unpack $ pShowNoColor formattedAST)
                    writeFile (filename ++ "-orig.ast") (TL.unpack $ pShowNoColor origAST)
                    putStrLn $ "failed: " ++ filename
                    -- pPrint origAST
                    -- pPrint formattedAST
                else
                  putStrLn $ "success: " ++ filename
              Left e -> do
                putStrLn $ "failed: " ++ filename
                print e
        Left e ->
            error $ "parse of original file failed, this should never happen: \n" ++ show e

main :: IO ()
main = do
  putStrLn "Test that the formatter returns same AST"
  inputFiles <-
    glob "examples/passing/**/*.purs"
    -- glob "examples/passing/1664.purs"

  inputs <- for (Prelude.take 10 inputFiles) $ \filename ->
     liftIO (readUTF8File filename)

  for (Prelude.zip inputFiles inputs) $ uncurry roundTrip
  putStrLn "formatter tests ran"


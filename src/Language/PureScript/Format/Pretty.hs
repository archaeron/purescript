module Language.PureScript.Format.Pretty
    ( Ann(..)
    , PrettyAnn (..)
    , module Data.Text.Prettyprint.Doc
    , listify
    , prettyEncloseSep
    , keyword
    , punctuation
    , string
    , collection
    , comment
    , typeConstructor
    , dbg
    , operator
    , indent
    , halfindent
    , renderPlain
    , renderAnnotated
    ) where

import           Control.Applicative

import           Data.String                              (IsString (..))
import qualified Data.Text                                as T
-- import Numeric (showHex)

import           Data.Text.Prettyprint.Doc hiding (Pretty(..), indent)
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Render.Text as P.Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as P.Terminal

import           Language.PureScript.Label
import           Language.PureScript.PSString
-- import Data.Word (Word16)

import           Prelude
-- import Debug.Trace
-- Pretty Annotations
data Ann
    = AnnConstructor
    | AnnTypeAnnConstructor
    | AnnComment
    | AnnOperator
    | AnnSyntax
    | AnnString
    | AnnNumber
    | AnnKeyword
    | AnnPunctuation
    | AnnDBG
    deriving (Eq, Ord, Show)

palette :: Ann -> P.Terminal.AnsiStyle
palette AnnConstructor = P.Terminal.color P.Terminal.Blue
palette AnnTypeAnnConstructor = P.Terminal.colorDull P.Terminal.Red
palette AnnComment = P.Terminal.colorDull P.Terminal.White
palette AnnOperator = P.Terminal.color P.Terminal.Yellow
palette AnnSyntax = P.Terminal.color P.Terminal.Black
palette AnnString = P.Terminal.color P.Terminal.Green
palette AnnNumber = P.Terminal.colorDull P.Terminal.Blue
palette AnnKeyword = P.Terminal.color P.Terminal.Red
palette AnnPunctuation = P.Terminal.colorDull P.Terminal.Yellow
palette AnnDBG = P.Terminal.color P.Terminal.White <> P.Terminal.bold

-- updateColor :: StateT [PrettyAnn] IO ()
-- updateColor =
--   lift . setSGR =<< mconcat . map toSGR . reverse <$> get

-- openTag :: PrettyAnn -> StateT [PrettyAnn] IO ()
-- openTag ann = modify (ann:) >> updateColor

-- closeTag :: PrettyAnn -> StateT [PrettyAnn] IO ()
-- closeTag _  = modify tail   >> updateColor

-- renderAnnotation :: PrettyAnn -> StateT [PrettyAnn] IO () -> StateT [PrettyAnn] IO ()
-- renderAnnotation a o = openTag a >> o >> closeTag a

class PrettyAnn a where
    pretty :: a -> Doc Ann
    
    prettyList :: [a] -> Doc Ann
    prettyList = list . map pretty

(<+>) :: Doc ann -> Doc ann -> Doc ann
(<+>) l r = l <> space <> r

collection :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
collection open close separator values = case values of
    [] -> open <> close
    _ ->
        open
            <> (concatWith (\x y -> x <> softline <> separator <> y) values)
            <> close

listify :: [Doc ann] -> Doc ann
listify = collection lbracket rbracket comma

prettyEncloseSep :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
prettyEncloseSep l r xs = 
    case xs of
    [] -> lparen <> rparen
    _  -> collection l r space xs

indent :: Doc ann -> Doc ann
indent = nest 4

halfindent :: Doc ann -> Doc ann
halfindent = nest 2

tupled :: [Doc ann] -> Doc ann
tupled = collection lparen rparen comma

keyword :: T.Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

string :: Doc Ann -> Doc Ann
string = annotate AnnString

comment :: Doc Ann -> Doc Ann
comment = annotate AnnComment

punctuation :: Doc Ann -> Doc Ann
punctuation = annotate AnnPunctuation

operator :: Doc Ann -> Doc Ann
operator = annotate AnnOperator 

typeConstructor :: Doc Ann -> Doc Ann
typeConstructor = annotate AnnTypeAnnConstructor

dbg :: Show a => a -> Doc Ann
dbg = annotate AnnDBG . pretty . T.pack . show

layout :: Doc ann -> SimpleDocStream ann
layout = layoutPretty (LayoutOptions (AvailablePerLine 80 0.8))

renderPlain :: Doc ann -> T.Text
renderPlain =
    P.Text.renderStrict . layout

renderAnnotated :: Doc Ann -> T.Text
renderAnnotated = P.Terminal.renderStrict . layout . reAnnotate palette

instance PrettyAnn T.Text where
    pretty = P.pretty

instance PrettyAnn Int where
    pretty = annotate AnnNumber . P.pretty

instance PrettyAnn Integer where
    pretty = annotate AnnNumber . P.pretty

instance PrettyAnn Double where
    pretty = annotate AnnNumber . P.pretty

instance PrettyAnn Char where
    pretty = P.pretty

instance PrettyAnn PSString where
    pretty a = 
      case decodeString a of
        Just t -> P.pretty t
        Nothing -> dbg $ P.pretty $ T.pack "<lone surrogate>"

instance PrettyAnn Label where
    pretty = pretty . runLabel

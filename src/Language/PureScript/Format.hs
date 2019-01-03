module Language.PureScript.Format
    ( format
    , replFormat
    , printColoured
    ) where

import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T.IO
import           Prelude

import           Language.PureScript.Format.Pretty

import qualified Language.PureScript               as P

import           Language.PureScript.Format.Module (pprintModule)
import           System.FilePath                   (FilePath)

format :: FilePath -> String -> String
format filename inputFile =
    case P.parseModulesFromFiles id [(filename, T.pack inputFile)] of
        Right v ->
            T.unpack $ renderPlain $ vsep  $ fmap (\(_, m) -> pprintModule m) v
        Left e ->
            P.prettyPrintMultipleErrors P.defaultPPEOptions e

printColoured :: String -> String -> IO ()
printColoured filename inputFile =
    case P.parseModulesFromFiles id [(filename, T.pack inputFile)] of
        Right v ->
            T.IO.putStrLn $ renderAnnotated $ vsep $ fmap (\(_, m) -> pprintModule m) v
        Left e ->
            putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions e

replFormat :: String -> IO ()
replFormat p = do
    c <- readFile p
    putStrLn $ format p c
    -- printColoured p c

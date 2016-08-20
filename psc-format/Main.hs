-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main
    ( main
    ) where

import Prelude
import System.Exit (exitFailure)
import System.IO (stderr, stdout, hPutStrLn, hSetEncoding, utf8)

import Text.PrettyPrint.ANSI.Leijen (displayS, renderPretty, vsep)
import Options.Applicative

import qualified Language.PureScript as P

import Module (pprintModule)

data Config = Config
  { _input  :: String
  , _output :: Maybe String
  }

config :: Parser Config
config = Config
    <$> strOption
        ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Specify path to input file"
        )
    <*> optional
        ( strOption
            ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Specify path to output file"
            )
        )

runFormatter :: Config -> IO ()
runFormatter (Config i o) = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    inputFile <- readFile i
    case P.parseModulesFromFiles id [("Main", inputFile)] of
        Right v -> do
            case o of
                Nothing -> putStrLn output
                Just o' -> writeFile o' output
            where
                output =
                    displayS (renderPretty 0.9 80 . vsep $ fmap (\(_, m) -> pprintModule m) v) ""
        Left e -> do
            hPutStrLn stderr (P.prettyPrintMultipleErrors P.defaultPPEOptions e)
            exitFailure

main :: IO ()
main = execParser opts >>= runFormatter
    where
        opts = info (helper <*> config)
            ( fullDesc
            <> progDesc "Run this program to format a purs file. "
            <> header "psc-format - Format PureScript files"
            )

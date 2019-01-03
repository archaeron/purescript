{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | Fromat compiled PureScript files.
module Command.Format (command) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Foldable              (for_)
import           Data.Maybe                 (Maybe)
import           Data.Monoid                ((<>))
import           Data.Traversable           (for)

import           Language.PureScript.Bundle
import           Language.PureScript.Format as Format
import           Options.Applicative        (Parser)
import qualified Options.Applicative        as Opts
import           Prelude
import           System.Exit                (exitFailure)
import           System.FilePath            (FilePath)
import           System.FilePath.Glob       (glob)
import           System.IO                  (IO, hPutStr, hPutStrLn, stderr)
import           System.IO.UTF8             (readUTF8File, writeUTF8File)

-- | Command line options.
data Options = Options
  { optionsInputFiles :: [FilePath]
  , rules             :: Maybe FilePath
  , replace           :: Bool
  } deriving Show

-- | The main application function.
-- This function parses the input files and formats them accordingly to rules
app :: (MonadError ErrorMessage m, MonadIO m) => Options -> m [(FilePath, String)]
app Options{..} = do
  inputFiles <- concat <$> mapM (liftIO . glob) optionsInputFiles
  when (null inputFiles) . liftIO $ do
    hPutStrLn stderr "purs format: No input file(s)."
    exitFailure

  inputs <- for inputFiles $ \filename ->
     liftIO (readUTF8File filename)
  let withPath = zip inputFiles
  let formatted = uncurry Format.format <$> withPath inputs
  return (withPath formatted)

-- | Command line options parser.
options :: Parser Options
options = Options <$> some inputFile
                  <*> optional rules
                  <*> replace
  where
  inputFile :: Parser FilePath
  inputFile = Opts.strArgument $
       Opts.metavar "FILE"
    <> Opts.help "The input .purs file(s)"

  replace :: Parser Bool
  replace = Opts.switch ( Opts.long "replace" <> Opts.short 'r' <> Opts.help "Whether to replace input file with formatted file or output to stdout" )

  rules :: Parser FilePath
  rules = Opts.strOption $
       Opts.long "rules"
    <> Opts.help "not implemented -- rules"

-- | Make it go.
command :: Opts.Parser (IO ())
command = run <$> (Opts.helper <*> options) where
  run :: Options -> IO ()
  run opts@Options { replace = replace } = do
    output <- runExceptT (app opts)
    case output of
      Left err -> do
        hPutStr stderr (unlines (printErrorMessage err))
        exitFailure
      Right formatted ->
        if replace then
          for_ formatted $ uncurry writeUTF8File
        else
          for_ formatted $ \(_, content) -> putStrLn content

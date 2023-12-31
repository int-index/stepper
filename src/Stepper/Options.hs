{-# LANGUAGE ApplicativeDo #-}

-- Command-line options
module Stepper.Options
  ( Command(..),
    Options(..),
    parseOptions,
  ) where

import Data.Text (Text)
import Options.Applicative hiding (command)
import qualified Options.Applicative as Opts

data Command =
    CommandInteract
  | CommandSvg { outPath :: FilePath }

data Options =
  Options {
    command :: Command,
    srcPath :: FilePath,
    entryPoint :: Text
  }

pOptions :: Parser Options
pOptions = do
  let defaultCommand = CommandInteract
  command <-
    (<|> pure defaultCommand) $
    hsubparser $
      Opts.command "interact" (info pCommandInteract (progDesc "run an interactive GUI")) <>
      Opts.command "svg" (info pCommandSvg (progDesc "generate SVG files"))
  srcPath <- strArgument (metavar "SRC" <> action "file")
  entryPoint <- strOption (short 'e' <> long "entry-point")
  return Options{command, srcPath, entryPoint}

pCommandInteract :: Parser Command
pCommandInteract = pure CommandInteract

pCommandSvg :: Parser Command
pCommandSvg = do
  outPath <- strOption (short 'o' <> metavar "OUT" <> action "directory")
  return CommandSvg{outPath}

parseOptions :: IO Options
parseOptions =
  customExecParser (prefs subparserInline) $
    info (pOptions <**> helper) $
      fullDesc <>
      header "stepper - step-through evaluation of functional programs"
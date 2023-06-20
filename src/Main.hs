module Main where

import Data.IText
import qualified Data.Text.Encoding as Text
import Control.Exception
import System.IO
import System.Exit
import qualified Data.ByteString as ByteString
import Stepper.Options
import Stepper.Interactive
import Stepper.Parser
import Stepper.Renamer
import Stepper.Syntax.Scoped

main :: IO ()
main = do
  Options{command, srcPath, entryPoint} <- parseOptions
  (srcMod, itextPool) <- readSourceFile srcPath
  let (entryPoint', _) = intern entryPoint itextPool
  case command of
    CommandInteract -> runInteractiveApp srcMod entryPoint'
    CommandSvg{outPath} -> do
      hPutStrLn stderr "svg: not implemented"
      exitFailure

readSourceFile :: FilePath -> IO (Module, ITextPool)
readSourceFile srcPath = do
  srcBytes <-
    ByteString.readFile srcPath `catch` \(e :: IOException) -> do
      hPutStrLn stderr $ "Failed to load source file:\n" ++ displayException e
      exitFailure
  srcText <-
    case Text.decodeUtf8' srcBytes of
      Right a -> return a
      Left e -> do
        hPutStrLn stderr $ "Failed to decode source file:\n" ++ displayException e
        exitFailure
  (srcPMod, itextPool) <-
    case parseModule srcPath srcText of
      Right a -> return a
      Left errmsg -> do
          hPutStrLn stderr $ "Failed to parse source file:\n" ++ errmsg
          exitFailure
  case renameModule srcPMod of
      Right a -> return (a, itextPool)
      Left e -> do
        hPutStrLn stderr $ "Failed to rename source file:\n" ++ show e
        exitFailure
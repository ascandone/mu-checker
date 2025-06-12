{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified CCS.LTS
import qualified CCS.Parser
import qualified CCS.Pretty
import qualified CCS.Program
import qualified Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO
import Mu.Verify (FailingSpec (FalsifiedFormula))
import qualified Mu.Verify
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

parseFilePath :: FilePath -> IO CCS.Program.Program
parseFilePath filePath = do
  content <- Data.Text.IO.readFile filePath
  unwrapEither (CCS.Parser.parse filePath content) CCS.Parser.errorBundlePretty

runFile :: String -> IO ()
runFile filePath = do
  parsed <- parseFilePath filePath
  Control.Monad.forM_ (Mu.Verify.verifyProgram parsed) $
    \(def, e) -> case e of
      Left err -> print err
      Right failingsSpecs ->
        Control.Monad.forM_ failingsSpecs $ \(FalsifiedFormula formula) ->
          print (def.name, formula)

stringifyEvt :: Maybe CCS.Program.EventChoice -> String
stringifyEvt evt = case evt of
  Nothing -> "tau"
  Just (CCS.Program.Snd a) -> Text.unpack a ++ "!"
  Just (CCS.Program.Rcv a) -> Text.unpack a ++ "?"

dbgLoop :: Map Text CCS.Program.Definition -> CCS.Program.Process -> IO ()
dbgLoop defs init_ = do
  trs <- unwrapEither (CCS.LTS.getTransitions defs init_) show

  Control.Monad.forM_ (zip [1 :: Int ..] trs) $ \(index, (evt, nextProc)) ->
    putStrLn $ show index ++ ") " ++ stringifyEvt evt ++ " => " ++ Text.unpack (CCS.Pretty.pretty nextProc)
  putStr "> "
  hFlush stdout

  n <- read <$> getLine

  -- TODO handle not found
  let (_, matchingEvent) = trs !! (n - 1)
  dbgLoop defs matchingEvent

dbg :: String -> String -> IO ()
dbg filePath procName = do
  parsed <- parseFilePath filePath
  let defsMap = Map.fromList [(def.name, def) | def <- parsed]
  firstState <- unwrapEither (maybeToEither $ Text.pack procName `Map.lookup` defsMap) $ \() ->
    "Missing initial state: " ++ procName
  dbgLoop defsMap firstState.definition

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", path] -> runFile path
    ["dbg", path, procName] -> dbg path procName
    _ -> do
      putStrLn $ "Wrong number of args: " ++ show args
      exitFailure

unwrapEither :: Either t b -> (t -> String) -> IO b
unwrapEither e asError = case e of
  Left v -> do
    putStrLn (asError v)
    exitFailure
  Right x -> return x

maybeToEither :: Maybe a -> Either () a
maybeToEither m = case m of
  Nothing -> Left ()
  Just x -> Right x

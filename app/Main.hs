{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use snd" #-}

module Main (main) where

import qualified CCS.LTS
import qualified CCS.Parser
import qualified CCS.Pretty
import qualified CCS.Program
import qualified Control.Monad
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Mu.Checker
import qualified Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

parseFilePath :: FilePath -> IO ([Text], CCS.Program.Program)
parseFilePath filePath = do
  content <- Data.Text.IO.readFile filePath
  program <- unwrapEither (CCS.Parser.parse filePath content) CCS.Parser.errorBundlePretty
  return (Text.lines content, program)

selectText :: [Text] -> Parser.Range -> Text
selectText lines_ rng =
  lines_
    & drop (rng.start.line - 1)
    & take 1
    & Text.unlines

isFailing :: Mu.Checker.Check -> Bool
isFailing x = case x of
  Mu.Checker.Pass -> False
  Mu.Checker.FalsifiedFormula _ _ -> True

runFile :: String -> IO ()
runFile filePath = do
  (fileLines, parsed) <- parseFilePath filePath
  let checkTree = Mu.Checker.verifyProgram parsed
  Control.Monad.forM_ checkTree $
    \(def, e) -> case e of
      Left err -> print err
      Right checks -> do
        Control.Monad.when (any isFailing checks) $ do
          Data.Text.IO.putStrLn (def.name <> ":")
        Control.Monad.forM_ checks $ \check -> case check of
          Mu.Checker.Pass -> return ()
          Mu.Checker.FalsifiedFormula rng _formula ->
            let text = selectText fileLines rng
             in Data.Text.IO.putStr ("  ❌ " <> text)

  Data.Text.IO.putStr "\n"
  let allSpecs = concat <$> Control.Monad.forM checkTree (\(_, e) -> e)
  case allSpecs of
    Left _ -> exitFailure
    Right checks -> do
      let total = length checks
      let failing = length $ filter isFailing checks
      let passing = total - failing
      case () of
        _ | total == 0 -> do
          Data.Text.IO.putStr "No specs found"
          exitFailure
        _ | failing > 0 -> do
          Data.Text.IO.putStr (textShow passing <> "/" <> textShow total <> " specs passing (" <> textShow failing <> " failures) ❌")
          exitFailure
        _ -> Data.Text.IO.putStr (textShow passing <> " specs passing " <> "✅")

textShow :: (Show a) => a -> Text
textShow = Text.pack . show

stringifyEvt :: Maybe CCS.Program.Action -> String
stringifyEvt evt = case evt of
  Nothing -> "tau"
  Just (CCS.Program.Action t a args) ->
    Text.unpack $
      a <> mkArgs args <> case t of
        CCS.Program.Rcv -> "!"
        CCS.Program.Snd -> "?"
 where
  mkArgs args = case args of
    [] -> ""
    _ -> "(" <> Text.intercalate "," args <> ")"

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
  (_, parsed) <- parseFilePath filePath
  let defsMap = Map.fromList [(def.name, def) | def <- parsed]
  firstState <- unwrapEither (maybeToEither $ Text.pack procName `Map.lookup` defsMap) $ \() ->
    "Missing initial state: " ++ procName
  dbgLoop defsMap firstState.definition

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["check", path] -> runFile path
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

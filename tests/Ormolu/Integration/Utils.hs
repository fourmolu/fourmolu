{-# LANGUAGE RecordWildCards #-}

module Ormolu.Integration.Utils
  ( getFourmoluExe,
    readProcess,
    ProcessSpec (..),
    proc,
    readFrom,
  )
where

import Control.Monad (when)
import System.Directory (findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), readCreateProcessWithExitCode)
import System.Process qualified as Process

-- | Find a `fourmolu` executable on PATH.
getFourmoluExe :: IO FilePath
getFourmoluExe = findExecutable "fourmolu" >>= maybe (fail "Could not find fourmolu executable") return

-- | Like 'System.Process.readProcess', except without specifying stdin and
-- with better failure messages.
readProcess :: FilePath -> [String] -> IO String
readProcess cmd args = readFrom $ proc cmd args

data ProcessSpec = ProcessSpec
  { procCmd :: FilePath,
    procArgs :: [String],
    procStdin :: String,
    procCwd :: Maybe FilePath,
    procExtraEnv :: [(String, String)]
  }

proc :: FilePath -> [String] -> ProcessSpec
proc cmd args =
  ProcessSpec
    { procCmd = cmd,
      procArgs = args,
      procStdin = "",
      procCwd = Nothing,
      procExtraEnv = []
    }

readFrom :: ProcessSpec -> IO String
readFrom ProcessSpec {..} = do
  currentEnv <- getEnvironment
  (code, stdout, stderr) <-
    readCreateProcessWithExitCode
      (Process.proc procCmd procArgs)
        { cwd = procCwd,
          env = Just $ procExtraEnv <> currentEnv
        }
      procStdin

  when (code /= ExitSuccess) $ do
    putStrLn $ "Command failed: " ++ (unwords . map (\s -> "\"" ++ s ++ "\"")) (procCmd : procArgs)
    putStrLn "========== stdout =========="
    putStrLn stdout
    putStrLn "========== stderr =========="
    putStrLn stderr
    fail "Command failed. See output for more details."

  pure stdout

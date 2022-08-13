module IntegrationUtils
  ( getFourmoluExe,
    readProcess,
  )
where

import Control.Monad (when)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Find a `fourmolu` executable on PATH.
getFourmoluExe :: IO FilePath
getFourmoluExe = findExecutable "fourmolu" >>= maybe (fail "Could not find fourmolu executable") return

-- | Like 'System.Process.readProcess', except without specifying stdin and showing stdout/stderr
-- on failure.
readProcess :: FilePath -> [String] -> IO String
readProcess cmd args = do
  (code, stdout, stderr) <- readProcessWithExitCode cmd args ""

  when (code /= ExitSuccess) $ do
    putStrLn $ "Command failed: " ++ (unwords . map (\s -> "\"" ++ s ++ "\"")) (cmd : args)
    putStrLn "========== stdout =========="
    putStrLn stdout
    putStrLn "========== stderr =========="
    putStrLn stderr
    fail "Command failed. See output for more details."

  return stdout

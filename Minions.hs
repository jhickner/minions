{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Console.ANSI
import System.Console.GetOpt
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import System.IO (stderr, hPutStrLn)

import Control.Concurrent
import Control.Monad

import Data.Time.Clock
import Data.String.Utils
import Text.Printf


data Task = Task
    { taskHost    :: String
    , taskCmd :: [String]
    } deriving Show

data Result = Result
    { resHost    :: String
    , resPayload :: Either String String
    , resTime    :: NominalDiffTime
    } deriving Show

-- | time the given IO action (clock time) and return a tuple 
--   of the execution time and the result
timeIO :: IO a -> IO (NominalDiffTime, a)
timeIO ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    return (diffUTCTime t2 t1, a)

runTasks :: [Task] -> Int -> (Result -> IO a) -> IO ()
runTasks ts tmout handler = do
    out <- newChan
    forM_ ts $ run out
    replicateM_ (length ts) (readChan out >>= handler)
  where run ch t = forkIO $ do
                       res <- runTask t tmout
                       writeChan ch res

runTask :: Task -> Int -> IO Result
runTask Task{..} tmout = do
    (time, res) <- timeIO . timeout tmout $ readProcessWithExitCode "ssh" args [] 
    case res of
      Nothing -> output time $ Left "timed out\n"
      Just (code, stdout', stderr') -> 
        output time $ case code of
            ExitSuccess   -> Right stdout'
            ExitFailure _ -> Left stderr'
  where args = "-T" : taskHost : taskCmd 
        output time payload = return $ Result taskHost payload time

mkTasks :: [String] -> [String] -> [Task]
mkTasks hosts cmd = map f hosts
  where f h = Task h cmd


----------------
-- PRINTING
----------------

putColorLn :: ColorIntensity -> Color -> String -> IO ()
putColorLn intensity color str = do
    setSGR [SetColor Foreground intensity color]
    putStrLn str
    setSGR []

printResult :: Result -> IO ()
printResult Result{..} = do
    putColorLn Dull Blue (resHost ++ t)
    case resPayload of
      Left s  -> putColorLn Vivid Red s
      Right s -> putStrLn s
    where t = printf " (%.1fs)" (realToFrac resTime :: Double) 

printShortResult :: Result -> IO ()
printShortResult Result{..} =
    case resPayload of
      Left s  -> putColorLn Vivid Red $ resHost ++ t ++ rstrip s
      Right s -> putStrLn $ resHost ++ t ++ rstrip s
    where t = printf " (%.1fs): " (realToFrac resTime :: Double) 


----------------
-- ARGS
----------------

data Options = Options
    { oHelp    :: Bool
    , oHosts   :: FilePath
    , oTimeout :: Int
    , oHandler :: Result -> IO ()
    }

defaultOptions :: Options
defaultOptions = Options False "" (10 * 1000000) printResult

options :: [OptDescr (Options -> Options)]
options =
    [ Option [] ["help"]
      (NoArg (\opts -> opts { oHelp = True }))
      "display this help"
    , Option "h" ["hosts"]
      (ReqArg (\f opts -> opts { oHosts = f }) "FILE")
      "FILE containing ssh host names (one per line)"
    , Option "t" ["timeout"] 
      (ReqArg (\f opts -> opts { oTimeout = (read f :: Int) * 1000000 }) 
        "SECONDS")
      "ssh timeout in SECONDS (default 10)"
    , Option "s" ["short"]
      (NoArg (\opts -> opts { oHandler = printShortResult }))
      "display results in short format"
    ]

parseArgs :: IO (Options, [String])
parseArgs = do
    argv <- getArgs
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, es) -> showError $ concat es

showError :: String -> IO a
showError msg = hPutStrLn stderr (msg ++ header) >> exitFailure

header :: String
header = usageInfo "Usage: minions [-hst] command" options

main :: IO ()
main = do
    (Options{..}, cmd) <- parseArgs
    when oHelp (putStrLn header >> exitSuccess)
    when (oHosts == "") (showError "Please specify a hostname file with -h\n")
    when (null cmd) (showError "Please specify a command to run\n")
    hosts <- (filter ((not . null) . strip) . lines) `fmap` readFile oHosts
    runTasks (mkTasks hosts cmd) oTimeout oHandler 

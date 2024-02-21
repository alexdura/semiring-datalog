module Main (main) where

import Options.Applicative
import System.FilePath
import Datalog
import Eval
import Programs
import Context
import Control.Monad
import Data.Semiring


data Options = Options {
  input :: FilePath,
  output :: FilePath,
  step :: Bool
  }

cliOptions :: Parser Options
cliOptions = Options <$>
  strOption (long "input" <> metavar "INPUT" <> showDefault <> value ".") <*>
  strOption (long "output" <> metavar "OUTPUT" <> showDefault <> value ".") <*>
  switch (long "step")


main :: IO ()
main = let opts = info (cliOptions <**> helper)
                  (fullDesc <> progDesc "Generate appropriate compile commands files")
       in handleOptions =<< execParser opts


runProgram :: (GroundTerm a, Ord a, Semiring s, Eq s, Show s) => Program a s -> FilePath -> FilePath -> [String] -> [String] -> IO ()
runProgram p ind outd inrel outrel = do
  let inputFiles = (\r -> (r, ind </> r ++ ".csv")) <$> inrel
      outputFiles = (\r -> (r, outd </> r ++ ".csv")) <$> outrel
  ctx <- foldM (\ctx (name, path) -> loadFromCSV ctx name (const one) path) emptyContext inputFiles
  let ctx' =  eval p ctx
  mapM_ (\(name, path) -> storeToCSV ctx' name path) outputFiles

stepProgram :: (GroundTerm a, Ord a, Show a, Semiring s, Eq s, Show s) => Program a s -> FilePath -> FilePath -> [String] -> [String] -> IO ()
stepProgram p ind outd inrel outrel = do
  let inputFiles = (\r -> (r, ind </> r ++ ".csv")) <$> inrel
      outputFiles = (\r -> (r, outd </> r ++ ".csv")) <$> outrel
  ctx <- foldM (\ctx (name, path) -> loadFromCSV ctx name (const one) path) emptyContext inputFiles
  ctx' <- evalStep p ctx
  mapM_ (\(name, path) -> storeToCSV ctx' name path) outputFiles


data ProgramDesc a s = ProgramDesc {
  inRelations::[String],
  outRelations::[String],
  program :: Program a s
  }

runProgramDesc :: (GroundTerm a, Ord a, Semiring s, Eq s, Show s) => ProgramDesc a s -> FilePath -> FilePath -> IO ()
runProgramDesc pd ind outd = runProgram pd.program ind outd pd.inRelations pd.outRelations

stepProgramDesc :: (GroundTerm a, Ord a, Show a, Semiring s, Eq s, Show s) => ProgramDesc a s -> FilePath -> FilePath -> IO ()
stepProgramDesc pd ind outd = stepProgram pd.program ind outd pd.inRelations pd.outRelations


andersenDesc :: ProgramDesc (Either String Int) Bool
andersenDesc = ProgramDesc ["Alloc", "Move", "Load", "Store", "Call", "VCall", "FormalArg", "ActualArg",
                            "FormalReturn", "ActualReturn", "ReachableInit", "SrcLoc"]
               ["VarPointsToLoc"]
               andersen


andersenCtxDesc :: ProgramDesc (Either String Int) (ContextSemiring2 Int)
andersenCtxDesc = ProgramDesc ["Alloc", "Move", "Load", "Store", "Call", "VCall", "FormalArg", "ActualArg",
                               "FormalReturn", "ActualReturn", "ReachableInit", "SrcLoc"]
                  ["VarPointsToLoc", "CallGraphLoc", "ReachableLoc", "VarPointsTo",
                   "InterProcAssign"]
                  andersenCtx

-- andersenCtx1Desc :: ProgramDesc (Either String Int) (ContextSemiring2 Int)
andersenCallSiteSensitiveDesc = ProgramDesc ["Alloc", "Move", "Load", "Store", "Call", "VCall", "FormalArg", "ActualArg",
                                             "FormalReturn", "ActualReturn", "ReachableInit", "SrcLoc"]
                                ["VarPointsToLoc", "CallGraphLoc", "ReachableLoc", "VarPointsTo",
                                 "InterProcAssign"]
                                andersenCallSiteSensitive




handleOptions :: Options -> IO ()
handleOptions (Options ind outd s) =
  if s then stepProgramDesc andersenCallSiteSensitiveDesc ind outd
  else runProgramDesc andersenCallSiteSensitiveDesc ind outd

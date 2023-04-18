{-# LANGUAGE TypeOperators, QuasiQuotes #-}
{- quasi quotation
String are enclosed between   [r| and  |]
-}


module Main where

import Data.List
import Text.RawString.QQ
import Data.Maybe
import Data.IORef
import System.IO
import System.Environment -- getArgs
import Control.Monad  
import Text.Printf
import System.CPUTime -- getCPUTime :: IO Integer
import System.Console.GetOpt
import System.Exit
import System.Random
-- import Data.Array.IO




--------
import Language
-- import ProverTypes
import ParserTPTP
import ParserGC 
import ClausifyGeneralClauses
-- import Prover
import Utility
import AspEncoding



-- #######  OPTIONS ######

data Flag
   =  TraceAndLatex                     -- -t
  | Clausify                           -- -c
  | ClaessenRosenClausificationFlag    -- -C
  | ProverDebug                        -- -d
  | ClausDebug                         -- -D
  | GeneralClauses                     -- -g
  | Help                               -- -h
  | StrongClausificationFlag             -- -s
  | WeakClausificationFlag             -- -w
  deriving (Eq,Show)

flags =
       [
         Option ['c'] []       (NoArg Clausify)  "Apply the Clausification procedure to the input formula",
         Option ['R'] []       (NoArg ClaessenRosenClausificationFlag)  "Apply the Clausification procedure to the input formula", 
         Option ['d'] []       (NoArg ProverDebug)  "Debug Prover",
         Option ['D'] []       (NoArg ClausDebug)  "Debug Clausifier",
         Option ['g'] []       (NoArg GeneralClauses)  "GeneralClauses",
         Option ['h'] []       (NoArg Help)    "Print this help message",
         Option ['w'] []       (NoArg WeakClausificationFlag)  "Weak clausification"
       ]



getClausificationType ::  [Flag] -> ClausificationType

getClausificationType args =
  let claessenR =  ClaessenRosenClausificationFlag  `elem` args
      weak = WeakClausificationFlag `elem` args
      strong =  StrongClausificationFlag `elem` args
  in case (claessenR,weak,strong) of
    (True, _, True)  ->  ClaessenRosenClausificationStrong
    (True,_,False)   ->  ClaessenRosenClausification
    (False, True,__) ->  WeakClausification
    (False,False,_)  ->  StrongClausification



--------------------------------------------------------------------------------
-- main
-- ###  MAIN ###

main :: IO ()
main =
  do
    (args, files) <- getArgs >>= parseArgs
    let inputFile = head files
        clausDebug =  ClausDebug `elem` args
        noClausification  = GeneralClauses `elem` args 
        clausificationType = getClausificationType args
    if noClausification then
      withParseFileGC inputFile clausDebug 
    else
      withParseFile  inputFile  clausificationType clausDebug 


------------------------
-- parse arguments

parseArgs :: [String] -> IO ([Flag], [FilePath])
-- return arguments and input files in the command line 
parseArgs argv = case getOpt Permute flags argv of
        (args,inputFiles,[]) ->
          do
           if Help `elem` args then
              do
                hPutStrLn  stderr  help --   (usageInfo header flags)
                exitWith ExitSuccess
           else if  null inputFiles  then -- no input file
              do
                hPutStrLn stderr $ "No input file" ++ help
                exitWith (ExitFailure 1) 
           else if Clausify `elem` args then  -- only clausify
              do
                let clausificationType = getClausificationType args
                    clausDebug = ClausDebug `elem` args
                onlyClausifyInputFormulas (head inputFiles) clausificationType  clausDebug
                exitWith ExitSuccess 
           else return  (args,inputFiles)  
        (_,_,errs)      ->  -- non-empty list of error messages
          do
            hPutStrLn stderr $ ( concat errs ) ++ help 
            exitWith (ExitFailure 1)
      

help :: String
help = [r|
Usage: clausfier [OPTION] FILE

FILE
 text file containing the input formula F (mandatory), see README for the syntax

OPTIONS
 -c     only clausify
 -R     use Claessen&Rosen clausification (as in intuitR), to use strong clausification add option -s
 -g     the input is a general sequent (no clausification is needed)
 -h     print help
 -w     use weak clausification (the deafault is strong clausification)
|]  -- end string



withParseFile ::  FilePath   ->  ClausificationType -> Bool  -> IO ()
withParseFile file  clausificationType clausDebug   =
  do
    putStrLn $ "%%%% +++ Clausifier"
    putStrLn ("%%%% +++ Reading file '" ++ show file ++ "'...")
    mForms <- parseFileTPTP file
    case mForms of
       Left err-> print err  >> fail "parse error"
       Right inputForms   -> processTrace file clausificationType clausDebug  inputForms
       

  

withParseFileGC ::  FilePath    -> Bool  -> IO ()
withParseFileGC file  clausDebug    =
 -- parse general clauses, no clausification
  do
    putStrLn $ "+++ intuitRGC"
    putStrLn ("+++ Reading file (general clauses) " ++ show file ++ "'...")
    mGcSeq <- parseFileGcSeq file
    case mGcSeq of
       Left err -> print err  >> fail "parse error"
       Right gcSeq   -> processTraceGcSeq file  clausDebug  gcSeq
     

      
-- prover with trace
processTrace :: FilePath -> ClausificationType  -> Bool -> [Input (Form Name)] -> IO ()
processTrace file cType  clausDebug  inputForms = 
  do
     putStrLn $ "%%%% +++ Clausification ("  ++ show cType ++   ")..."
     let (alphas, beta) = parseInputForm inputForms  -- inputForms : (/\alphas) => beta
         (cache,gcSeq) =  clausifyForms  cType clausDebug ( (beta  :=>: Atom mainGoalName)  : alphas )
     putStrLn $ "%%%% +++ Created " ++ (show .length . classicalGCs) gcSeq  ++  " classical general clauses, "
                 ++ (show .length . intGCs) gcSeq   ++ " non-classical general clauses, "
                 ++ (show . cacheSize) cache ++ " new atoms\n"   
     hFlush stdout
     putStrLn $ aspEncode gcSeq


       
processTraceGcSeq :: FilePath   -> Bool ->  GenClauseSequent Name -> IO ()
processTraceGcSeq file clausDebug  gcSeq = 
  do
     putStrLn $ printfGCSeq id gcSeq
     putStrLn $ "+++ " ++ (show .length . classicalGCs) gcSeq  ++  " classical clauses, "
                       ++ (show .length . intGCs) gcSeq   ++ " intuitionistic clauses "
     hFlush stdout
     putStrLn $ "** TO DO **"
     -- printProblem  traceLev file clausDebug proverDebug mRandomSeed  gcSeq   emptyCache




-----------------------

-- ### ONLY CLAUSIFICATION ###

onlyClausifyInputFormulas :: FilePath -> ClausificationType -> Bool -> IO ()
onlyClausifyInputFormulas file  clausificationType  clausDebug  =
  do
    putStrLn ("+++ Reading file '" ++ show file ++ "'...")
    mForms <- parseFileTPTP file
    case mForms of
       Left err -> print err  >> fail "parse error"
       Right inputForms ->
        do
          let form = buildInputForm inputForms    
              (cache,gcSeq) = clausifyForms  clausificationType clausDebug [form] 
          putStrLn $  "=== FORMULA ===\n" ++ printfForm id form    
          putStrLn  $ printfGCSeq id gcSeq    
          -- putStrLn  $ "=== INTERNAL MAP ===\n" ++  printCache cache
          -- putStrLn  $ "=== SUBSTITUTION ===\n" ++  printCacheSubst cache


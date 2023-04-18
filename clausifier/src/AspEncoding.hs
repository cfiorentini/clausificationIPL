module AspEncoding
  (  aspEncode   --  GenClauseSequent Name -> String
  )
 where

import Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Control.Monad.State
-- import Debug.Trace

import Language
import Utility


aspEncode :: GenClauseSequent Name -> String
aspEncode gcSeq =
  let cGcs = classicalGCs gcSeq
      iGcs = intGCs gcSeq
      rAtm =  rightAtm gcSeq 
      atms = getGCSeqNames  gcSeq \\ ["$false"]
      numberOfAtms = List.length atms
  in
    "%% index of atoms\n" 
    ++ setIndexOfAtms atms 0  ++ "\n"
    ++ "%% classical clauses (" ++ (show .length) cGcs ++ ")\n"
    ++ printClClauses  cGcs 1
    ++ "%% non-classical clauses (" ++ (show .length) iGcs ++ ")\n"
    ++ printGenClauses iGcs 1  
    ++ printRightAtm rAtm ++ "\n\n"
    ++ "%% world representation\n" 
    ++ defineAtmSets numberOfAtms  


atmEncode :: Name -> String

atmEncode atm |  atm == "$false" = "false"
atmEncode atm | isNewName atm =
                let atm1 = tail atm -- drop the leading $
                in "\"$" ++ atm1 ++ "\""

atmEncode atm = atm

genLitEncode :: GenLit Name -> String
genLitEncode (At atm) = atmEncode atm
genLitEncode ( Not  atm) = "neg(" ++  atmEncode atm ++ ")"
genLitEncode ( atm1 :-/->: atm2  ) =
  "negImp(" ++   atmEncode atm1 ++ ", " ++   atmEncode atm2 ++ ")"


setIndexOfAtm :: Name -> Int -> String
-- setIndexOfAtm  atm k | atm == "$false" = ""
setIndexOfAtm  atm k = "indexOfAtom(" ++ atmEncode atm ++ "," ++ show k ++ ")."


setIndexOfAtms ::  [Name] -> Int  -> String
setIndexOfAtms [] _  = ""
setIndexOfAtms  (atm : atms) k  | atm == "$false" =
  setIndexOfAtms  atms k                
setIndexOfAtms  (atm : atms) k  =
  setIndexOfAtm  atm k  ++ "\n" ++ setIndexOfAtms  atms (k + 1) 

printClClause :: GenClause Name -> Int  -> String 
printClClause cGc k =
  let cGcName = "c" ++ show k
  in "% " ++ cGcName ++ " = " ++ printfGClause id cGc ++ "\n"
     ++ "clClause(" ++  cGcName ++ ")." ++ "\n" 
     ++ printLits cGc cGcName 

printLit :: GenLit Name -> Name ->  String
printLit gLit cGcName =
  "litOf(" ++ genLitEncode gLit ++ ", " ++ cGcName ++ ")."

printLits :: GenClause Name -> Name ->  String
printLits  [] cGcName = ""
printLits  (gLit : gLits) cGcName   =
  printLit gLit cGcName ++ "\n" ++ printLits  gLits cGcName


printClClauses :: [GenClause Name] -> Int  -> String 
printClClauses  [] _  = ""
printClClauses  (cGc : cGcs)  k  = 
  printClClause cGc k ++ "\n" ++  printClClauses cGcs (k + 1)


printGenClause :: GenClause Name -> Int  -> String 
printGenClause gc k =
  let gcName = "gc" ++ show k
  in "% " ++ gcName ++ " = " ++ printfGClause id gc ++ "\n" 
     ++ "genClause(" ++  gcName ++ ")." ++ "\n" 
     ++ printLits gc gcName 

printGenClauses :: [GenClause Name] -> Int -> String 
printGenClauses [] k  = ""
printGenClauses (iGc : iGcs) k  =
  printGenClause iGc k ++ "\n" ++  printGenClauses iGcs (k + 1)

printRightAtm :: Name -> String
printRightAtm atm =
  "%% right atom\n" 
  ++ "rightAtom(" ++ atmEncode atm ++ ")."


defineAtmSets :: Int -> String
defineAtmSets numberOfAtms =
  let range =  [ 0 ..  (numberOfAtms - 1) ]
      xs = List.map (\x -> "0..1") range -- ["0..1", ... ,  "0..1"]
  in
   "atomSet( " ++ printWFromList  xs  ++ " ).\n"   -- atomSet( w(0..1, .... , 0..1) )
   ++ foldl (\s k -> s ++ "\n" ++ printWorldComponent numberOfAtms k) "" range 


printWFromList :: [String] -> String
-- printWFromList [s1 , ... ,sn] =
--  w(s1, ... , sn)
-- We assume that the input list is not empty

printWFromList xs = 
  "w(" ++ foldl1 (\s x -> s ++  ", " ++ x )  xs ++ ")" 


printWorldComponent :: Int -> Int -> String
-- printWorldComponent N K
--  atomSetComponent(K, w(B0, ... ,B(N-1)), BK) :-   atomSet( w(B0, ... ,B(N-1)) ).

printWorldComponent n k =
  let world = printWFromList $ List.map (\k -> "B" ++ show k)  [ 0 .. (n - 1) ]
      bk = "B" ++ show k
  in
    "atomSetComponent( " ++  show k ++ ", " ++ world ++ ", " ++ bk ++ ")" 
     ++ " :- \n"
     ++ "   atomSet( " ++ world ++ " )."

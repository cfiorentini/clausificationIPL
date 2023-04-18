{-# LANGUAGE TypeOperators #-}
module Utility(
  getGCSeqNames,    -- GenClauseSequent Name  -> [Name]
    printfForm,             --  (a -> Name) -> Form a -> String
    printfGClause,          --  (a -> String) -> GenClause a -> String
    printfForms,            --  (a -> Name) -> [Form a] -> String
    printfGCSeq,            --  (a -> Name) -> GenClauseSequent a -> String
  )
 where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array.IO
import System.Random  -- https://hackage.haskell.org/package/mwc-random-0.15.0.2/docs/System-Random-MWC.html
import Control.Monad.State

import Language



--------------------------------------------------------------------------------


getGCSeqNames ::  GenClauseSequent Name  -> [Name]
-- duplication free list of the  the names occurring in the genClauseSequent gcSeq
-- NOTE:  Set.fromList :: Ord a => [a] -> Set    complexity O(n log n)
-- nub ::  Eq a => [a] -> [a]    complexity O(n^2)
getGCSeqNames  gcSeq =
  let ps  =  [ getLitNames c | cs <- classicalGCs gcSeq ,   c <- cs ]
      ips =  [ getLitNames c | cs <- intGCs gcSeq , c <- cs ]
      g = rightAtm gcSeq 
  in Set.toList $ Set.fromList $ (concat ( ps ++ ips ))  ++  [g, false]


isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9' ) 


splitName :: Name -> (String,String)
-- split name and index, both as strings
-- "p11" |-> ("p","11") ,  "p123q14" |-> ("p123","14")  , "pqr" |-> ("pqr", "")  
splitName atm =
  let atmRev = reverse atm
      (kRev, nameRev) = span isDigit atmRev 
  in
  (reverse nameRev, reverse kRev)

splitNameIndex :: Name -> (String, Maybe Int)
-- split name and index, where name is a string and index an int
-- "p11" |-> ("p", Just 11) ,  "p123q14" |-> ("p123", Just 14)  , "pqr" |-> ("pqr", Nothing)  
splitNameIndex atm =
  let (name,indexStr) = splitName atm
      mIndex = if null indexStr then Nothing else  Just (read indexStr :: Int)
  in (name, mIndex)    



-- #### PRINT (for trace)

printfListSep :: String -> (a -> String) ->  [a] -> String
-- first argument: separator between elements
printfListSep sep f []   = "" 
printfListSep sep f [x]   = f x
printfListSep sep f (x:xs)   = f x ++ sep ++ printfListSep sep f  xs

printfList :: (a -> String) -> [a] -> String
printfList  = printfListSep ", " 

printfListNl :: (a -> String) -> [a] -> String
printfListNl = printfListSep "\n" 


printfAtms ::   (a -> Name) ->  [a] -> String
printfAtms  f xs = printfList f  xs


printfAtmsSq ::   (a -> Name) ->  [a] -> String
printfAtmsSq  f xs = "[" ++ printfAtms f xs ++ "]"

printfAtmsBrace ::   (a -> Name) ->  [a] -> String
printfAtmsBrace  f xs = "{" ++ printfAtms f xs ++ "}"

printfAtmsSortedBrace ::   (a -> Name) ->  [a] -> String
printfAtmsSortedBrace  f xs =
  let atmNames = map f xs
      nameMindex_pairs = map splitNameIndex atmNames  -- pairs of the kind ("xyz", Just 5), ("abc", Nothing)
      sorted_nameMindex_pairs = List.sort  nameMindex_pairs
  in printfAtmsBrace id (map nameMindex_toString sorted_nameMindex_pairs )

nameMindex_toString :: (Name, Maybe Int) -> String
--   ("xyz", Just 5) |--> "xyz5" ,  ("abc", Nothing) |--> "abc"
nameMindex_toString (name,Nothing) = name
nameMindex_toString (name, Just k) = name ++ show k

 

-- pretty print of formulas 

betweenParens :: String -> String
betweenParens f = "(" ++ f ++ ")"  

--printfForm :: (a -> Name) -> Form a -> String
printfForm :: Show a => (a -> Name) -> Form a -> String   
printfForm pf (Atom atm)  = pf atm

printfForm pf (f1 :&: f2) =
 let sf1 = printfForm pf f1
     sf2 = printfForm pf f2
     sf1' = if (mainLogicalOp (fmap pf f1)) `elem`[ NoOp,AndOp,NegOp] then sf1 else betweenParens sf1
     sf2' = if (mainLogicalOp (fmap pf f2)) `elem`[ NoOp,AndOp,NegOp] then sf2 else betweenParens sf2
 in sf1'  ++ " & " ++  sf2'

printfForm pf (f1 :|: f2) =
 let sf1 = printfForm pf f1
     sf2 = printfForm pf f2
     sf1' = if (mainLogicalOp (fmap pf f1)) `elem`[ NoOp,OrOp,NegOp] then sf1 else betweenParens sf1
     sf2' = if (mainLogicalOp (fmap pf f2)) `elem`[ NoOp,OrOp,NegOp] then sf2 else betweenParens sf2
 in sf1'  ++ " | " ++  sf2'


printfForm pf (f1 :=>: FALSE )  =
 let sf1 = printfForm pf f1
     sf1' = if (mainLogicalOp (fmap pf f1)) `elem`[ NoOp,NegOp] then sf1 else betweenParens sf1
 in "~" ++  sf1'

printfForm pf (f1 :=>: Atom f2 ) | (pf f2) == false  =
 let sf1 = printfForm pf f1
     sf1' = if (mainLogicalOp (fmap pf f1)) `elem`[ NoOp,NegOp] then sf1 else betweenParens sf1
 in "~" ++  sf1'

printfForm pf (f1 :=>: f2 )  =
 let sf1 = printfForm pf f1
     sf2 = printfForm pf f2
     sf1' = if (mainLogicalOp (fmap pf f1)) `elem`[ NoOp,NegOp] then sf1 else betweenParens sf1
     sf2' = if (mainLogicalOp (fmap pf f2)) `elem`[ NoOp,NegOp] then sf2 else betweenParens sf2
 in sf1'  ++ " => " ++  sf2'

printfForm pf (f1 :<=>: f2 )  =
 let sf1 = printfForm pf f1
     sf2 = printfForm pf f2
     sf1' = if (mainLogicalOp (fmap pf f1)) `elem`[ NoOp,NegOp] then sf1 else betweenParens sf1
     sf2' = if (mainLogicalOp (fmap pf f2)) `elem`[ NoOp,NegOp] then sf2 else betweenParens sf2
 in sf1'  ++ " <=> " ++  sf2'
printfForm pf f   = show f -- TRUE | FALSE
  

-- printfForms :: (a -> Name) -> [Form a] -> String
printfForms :: Show a  => (a -> Name) -> [Form a] -> String -- DELETE !!!
printfForms pf forms =
  printfList (printfForm pf ) forms



printfGLit ::   (a -> String) -> GenLit a -> String
printfGLit f (At x) =   f x
printfGLit f (Not x) =  "~" ++ f x
printfGLit f (x :-/->: y) =   f x ++ "-/->" ++  f y


printfGLits ::   (a -> String) -> [GenLit a] -> String
printfGLits f [] = ""
printfGLits f ( x : xs) =
  let sx = printfGLit f x in 
  if null xs then   sx
   else sx ++ ", " ++ printfGLits f xs



printfGClause ::  (a -> String) -> GenClause a -> String
printfGClause f [] = "Empty Clause (= False)"
printfGClause f gc = printfListSep " | "  (printfGLit f) gc  


printfGClauses ::  (a -> String) -> [GenClause a] -> String
printfGClauses f [] = ""
printfGClauses f  (gc : gcs ) = 
  let sgc = printfGClause f gc in 
  if null gcs then   sgc
   else sgc ++ "\n" ++ printfGClauses f gcs



printfGCSeq  :: (a -> String) -> GenClauseSequent a -> String
printfGCSeq f gcSet =
  let cGCs  = classicalGCs gcSet
      iGCs =  intGCs gcSet
  in "--    CLASSICAL GEN. CLAUSES (" ++ (show .length)  cGCs ++ "):\n" ++  printfGClauses f cGCs ++ "\n"
     ++ "--   INTUITIONISTIC GEN. CLAUSES (" ++ (show .length)  iGCs ++ "):\n"  ++  printfGClauses f iGCs
      ++ "\n" ++ "--  RIGHT ATOM: " ++   f (rightAtm gcSet)   


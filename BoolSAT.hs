-- Solves a boolean satisfiability problem

module BoolSAT (Expr(..),toNNF,Binding,Interpretation,consistent,solve,satisfiable,maxSatisfiable) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List

data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord)

type Binding = (Char, Bool)
type Interpretation = [Binding]

--Pattern match possible expressions
instance Show Expr where
  show (Var ch1) = [ch1]
  show (And (Var ch1) (Var ch2)) = [ch1] ++ " ^ " ++ [ch2]
  show (And (Not expr1) (Not expr2)) = show (Not expr1) ++ " ^ " ++ show (Not expr2)
  show (And (Not expr1) (Var ch2)) = show (Not expr1) ++ " ^ " ++ [ch2]
  show (And (Not expr1) expr2) = show (Not expr1) ++ " ^ (" ++ show expr2 ++ ")"
  show (And (Var ch1) (Not expr1)) = [ch1] ++ " ^ " ++ show (Not expr1)
  show (And expr1 (Not (Var ch1))) = "(" ++ show expr1 ++ ") ^ " ++ [ch1]  
  show (And (Var ch1) expr1) = [ch1] ++ " ^ " ++ "(" ++ show expr1 ++ ")"
  show (And expr1 (Var ch1)) = "(" ++ show expr1 ++ ")" ++ " ^ " ++ [ch1]
  show (And expr1 expr2) = "(" ++ show expr1 ++ ")" ++ " ^ " ++ "(" ++ show expr2 ++ ")"

  show (Or (Var ch1) (Var ch2)) = [ch1] ++ " v " ++ [ch2]
  show (Or (Not expr1) (Not expr2)) = show (Not expr1) ++ " v " ++ show (Not expr2)
  show (Or (Not expr1) (Var ch2)) = show (Not expr1) ++ " v " ++ [ch2]
  show (Or (Not expr1) expr2) = show (Not expr1) ++ " v (" ++ show expr2 ++ ")"
  show (Or (Var ch1) (Not expr1)) = [ch1] ++ " v " ++ show (Not expr1)
  show (Or expr1 (Not expr2)) = "(" ++ show expr1 ++ ") v " ++ show (Not expr2)  
  show (Or (Var ch1) expr1) = [ch1] ++ " v " ++ "(" ++ show expr1 ++ ")"
  show (Or expr1 (Var ch1)) = "(" ++ show expr1 ++ ")" ++ " v " ++ [ch1]
  show (Or expr1 expr2) = "(" ++ show expr1 ++ ") v (" ++ show expr2 ++ ")"

  show (Not (Var ch1)) = "~" ++ [ch1]
  show (Not (Not (Var ch1))) = "~~" ++ [ch1]
  show (Not expr1) = "~(" ++ show expr1 ++ ")"
 
--Move "Not" from expressions to variables
--Change expressions from "And" to "Or" and vice vera as per rules
toNNF :: Expr -> Expr 
toNNF (Not (And expr1 expr2)) = Or (toNNF $ Not expr1) (toNNF $ Not expr2)
toNNF (Not (Or expr1 expr2)) = And (toNNF $ Not expr1) (toNNF $ Not expr2)
toNNF (Not (Not a)) = a
toNNF a = a

--Check if interpretation is consistent by comparing truth assignments on equal variables
--Alternatively: f (x:xs) = any (\el -> fst x == fst el && snd x /= snd el) xs && f xs
consistent :: Interpretation -> Bool
consistent (x:xs) = length [ y | y <- xs, fst x == fst y, snd x /= snd y] == 0 && consistent xs
consistent [] = True

--Main solve function for any expression
--1. Transform expression to NNF
--2. Get the interpretations up to all "Or x y" expressions or up to the end if no "Or"s are met
--3 Pass as tuple which saves all the "Or" exprs met so far, and the interpretation of all possible variables before that
--4.1 Then for each "Or" expr in the list, calculate its interpretations further by combining in lists the following:
--4.2 [(Interpretations returned thus far) ++ (Interpretations in expr1 from "Or expr1 expr2)] ++ [same but it's interps in expr2]
--4.3.1 This is essentially the exponential part of the problem: For each "Or x y", copy every list so far,
--4.3.2 and add the interp of expr1 in one half, and interp of expr2 in other half. (Imagine a tree)
--5 Remove all incosistent interpretations and those that are subsets of others in the list
solve :: Expr -> [Interpretation]
solve = trimSort . solveHelp . solve' . toNNF
  where
    solveHelp :: ([Expr], Interpretation) -> [Interpretation]
    solveHelp (((Or expr1 expr2):[]), acc) = solveHelp (fst (solve' expr1), acc ++ snd(solve' expr1)) ++ solveHelp (fst (solve' expr2), acc ++ snd (solve' expr2)) 
    solveHelp (((Or expr1 expr2):xs), acc) = solveHelp (xs,acc ++ snd (solve' expr1)) ++ solveHelp (xs, acc ++ snd (solve' expr2))
    solveHelp ([], acc) = [acc]

    trimSort :: [Interpretation] -> [Interpretation]
    trimSort interps = [ nub interp | interp <- interps, consistent interp, (not) $ isSubset interp interps ]

--Check if some list of items is a subset of any list from a list of lists
--e.g. [1,2] is a subset of [1,2,3] in [[1,3],[1,4],[1,2,3]]
--Done by filtering all lists of size greater than our list (as it is a subset)
--And for each list, check if it contains every element of our list (in helper func)
isSubset :: Eq a => [a] -> [[a]] -> Bool
isSubset xs xss = any (\el -> isSubset' xs el) $ filter (\el -> length el >= length xs && el /= xs) xss

--Similar to the above, not needed eventually
--isSuperset :: Eq a => [a] -> [[a]] -> Bool
--isSuperset xs xss = any (\el -> isSubset' el xs) $ filter (\el -> length el <= length xs && el /= xs) xss

--Check if each element in our list is contained in the other
isSubset' :: Eq a => [a] -> [a] -> Bool
isSubset' [] _ = True
isSubset' (x:xs) ys = any (x==) ys && isSubset' xs ys

--Get interpretations up to the next "Or" or up to the end
solve' :: Expr -> ([Expr], Interpretation)
solve' (Var ch) = ([], [(ch, True)])
solve' (Not (Var ch)) = ([], [(ch, False)])
solve' (And expr1 expr2) = (fst (solve' expr1) ++ fst (solve' expr2), snd (solve' expr1) ++ snd (solve' expr2)) 
solve' (Or expr1 expr2) = ([Or expr1 expr2], [])

--Check if a list of expressions has common truth assignments that make all of them true
--Explanation in the helper function
satisfiable :: [Expr] -> Bool
satisfiable [] = True
satisfiable (x:xs) = length (satis xs (solve x)) > 0

--The idea here is that we get the interpretations from the previous expressions
--And combine each of them with the interpretations produced from the next expression (the x in (x:xs)),
--as long as the next interpretation is not a superset of the one we combine it with,
--then we check which ones are consistent and take those ones only
satis :: [Expr] -> [Interpretation] -> [Interpretation]
satis _ [] = []
satis [] ls = ls
satis (x:xs) ls = satis xs [ concat $ filter (consistent) $ map (l ++) (solve x) | l <- ls, any (consistent) (map (l ++) (solve x)) && not (isSubset l (solve x))]

--This one produces all subsets of expressions that are satisfiable together
--It filters out any subsets that are subsets of other subsets
maxSatisfiable :: [Expr] -> [[Expr]]
maxSatisfiable [] = [[]]
maxSatisfiable exprs = let subsets = filter (satisfiable) (comb (length exprs) exprs) in filter (\el -> not $ isSubset el subsets) subsets

--Produce all possible combinations of a list with itself
--e.g. [1,2,3] = [[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]
comb :: Int -> [Expr] -> [[Expr]]
comb 0 _ = [[]]
comb _ [] = []
comb n (x:xs) = nub $ map (x:) (comb (n-1) xs) ++ comb n xs ++ comb (n-1) (x:xs)

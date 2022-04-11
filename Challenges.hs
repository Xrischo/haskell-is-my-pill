--Challenge 1 and 2 - Solve the Black Box Game
--Challenge 3 - Pretty print Let expressions while taking into consideration Scott numerals
--Challenge 4 - Monadic Parser for Let expressions
--Challenge 5 - Transform Lambda expressions into Combinatory Logic

{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Hristiyan Georgiev

module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),LetExpr(..),CLExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseLet,
                   clTransform,
                   innerRedn1,outerRedn1,innerCLRedn1,outerCLRedn1,compareInnerOuter
                   )
where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

--instance NFData CLExpr
--instance NFData LetExpr
--instance NFData LamExpr 
--instance NFData Marking
--instance NFData Side

type Pos = ( Int, Int )

type EdgePos = ( Side, Int )
data Side = North | East | South | West deriving (Show, Eq, Ord)

type Atoms = [ Pos ]

type Interactions = [ (EdgePos, Marking) ]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)

calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions grid atoms = [ findReflect ((y, x), calcRay (rayDir y) (rayStart x grid y) grid atoms) | y <- [North, East, South, West], x <- [1..grid] ]

--Fix path of rays that were juggled around and eventually reflected
findReflect :: (EdgePos, Marking) -> (EdgePos, Marking)
findReflect ((side, x), Path (side2, y)) | side == side2 && x == y  = ((side, x), Reflect)
                                         | otherwise                = ((side, x), Path (side2, y))
findReflect a = a

--Direction of ray depending on where we start - north/south change Y axis, east/west change X axis
rayDir :: Side -> Pos
rayDir North = (0, 1)
rayDir East  = (-1, 0)
rayDir South = (0, -1)
rayDir West  = (1, 0)

--Starting position of ray
rayStart :: Int -> Int -> Side -> Pos
rayStart x size North = (x, 1)
rayStart x size East  = (size, x)
rayStart x size South = (x, size)
rayStart x size West  = (1, x)

--Calculate the ray path according to where it starts, its direction, the grid size and atoms positions
--Goes forward each step and checks if it runs into an atom or goes out of bounds
calcRay :: Pos -> Pos -> Int -> Atoms -> Marking
calcRay (0, d) (x, y) grid atoms | x > grid || x < 1      = Reflect
                                 | y > grid               = Path (South, x)
                                 | y < 1                  = Path (North, x)
                                 | (x, y) `elem` atoms    = Absorb
                                 | (x-1, y) `elem` atoms && (x+1, y) `elem` atoms = calcRay (0, -d) (x, y-d) grid atoms
                                 | (x-1, y) `elem` atoms  = calcRay (1, 0) (x+2, y-d) grid atoms
                                 | (x+1, y) `elem` atoms  = calcRay (-1, 0) (x-2, y-d) grid atoms
                                 | otherwise              = calcRay (0, d) (x, y+d) grid atoms

calcRay (d, 0) (x, y) grid atoms | y > grid || y < 1      = Reflect
                                 | x > grid               = Path (East, y)
                                 | x < 1                  = Path (West, y)
                                 | (x, y) `elem` atoms    = Absorb
                                 | (x, y-1) `elem` atoms && (x, y+1) `elem` atoms = calcRay (-d, 0) (x-d, y) grid atoms
                                 | (x, y-1) `elem` atoms  = calcRay (0, 1) (x-d, y+2) grid atoms
                                 | (x, y+1) `elem` atoms  = calcRay (0, -1) (x-d, y-2) grid atoms
                                 | otherwise              = calcRay (d, 0) (x+d, y) grid atoms

--List of combinations of size N over a list
comb :: Int -> Atoms -> [Atoms]
comb 0 _ = [[]]
comb _ [] = []
comb n (x:xs) = map (x:) (comb (n-1) xs) ++ comb n xs

--Combine 2 lists in one list of size N by replacing M number of items from the first list
--Increase M sequentially until it reaches N, since this is effectively 'comb n ys' on the second list
combLists :: Int -> Int -> Atoms -> Atoms -> [Atoms]
combLists _  _ [] a = [a]
combLists _  _ a [] = [a]
combLists m n xs ys | m >= n    = []
                    | otherwise = [ x ++ y | x <- comb (n - m) xs, y <- comb m ys] ++ combLists (m+1) n xs ys

solveBB :: Int -> Interactions -> Atoms
solveBB x inters = getFirst gridSize inters $ analyseRays gridSize x inters (getAbsorbLines inters gridSize) (getAtoms [North, West, East, South] gridSize inters)
   where
     gridSize = length (filter (\el -> fst (fst el) == North) inters)

--Get the first solution found
getFirst :: Int -> Interactions -> [Atoms] -> Atoms
getFirst _ _ [] = []
getFirst gridSize inters (solution:solutions) | inters == calcBBInteractions gridSize solution = solution
                                              | otherwise    = getFirst gridSize inters solutions

--Construct lists of possible solutions of size N based on atoms that we have found, and cells that we suspect are likely to have atoms
--Solutions are constructed in decreasing likelihood of satisfiability in the second argument of the map functions.
--Combine cells that we have found have atoms in, with list of cells that lie on 2 adjacent sides that rays were absorbed on,
--then combine with cells that lie on 1 absorbed side, then try everything else.
analyseRays :: Int -> Int -> Interactions -> Atoms -> (Atoms, Atoms) -> [Atoms]
analyseRays gridSize x inters absorbLines (exist, notExist) 
    | x == 0           = []
    | x < length exist = []
    | otherwise        = map (++ exist) 
                             (combLists 0 (remainingAtoms) doubleAbsorbs (filter (`notElem` doubleAbsorbs) absorbs)) ++ map (++ exist) 
                             (comb (remainingAtoms) allPossible)
   where
     doubleAbsorbs = nub [ (snd (fst x),snd (fst y)) | 
                               sideX <- [North, South], x <- filter (\((side1,_),mark1) -> side1 == sideX && mark1 == Absorb) inters, 
                               sideY <- [West, East], y <- filter (\((side2,_),mark2) -> side2 == sideY && mark2 == Absorb) inters, 
                               (snd (fst x),snd (fst y)) `notElem` exist ]

     absorbs = filter (\el -> notElem el notExist && notElem el exist) (nub absorbLines)
     allPossible = [ (x,y) | x <- [1..gridSize], y <- [1..gridSize], (x,y) `notElem` notExist, (x,y) `notElem` exist ]
     remainingAtoms = x - length exist

--Get a tuple of cells, the first part is cells that certainly have atoms, the second part is empty cells
--Func :: List of all sides of the blackBox -> Grid Size -> Interactions of the game -> Tuple of cells
getAtoms :: [Side] -> Int -> Interactions -> (Atoms, Atoms)
getAtoms [] gridSize inters = calcEdgeCells inters gridSize
getAtoms (side:sides) gridSize inters = (nub (concatMap fst calculateAtoms ++ fst (getAtoms sides gridSize inters)),
                                         nub (concatMap snd calculateAtoms ++ snd (getAtoms sides gridSize inters)))
   where
     calculateAtoms = [ narrowGrid (sortBy (sortRays dir) (filter (\((side2,_),_) -> side == side2) inters)) gridSize dir [] | dir <- [1,-1]]

--Sort interactions in increasing or decreasing order based on direction, to begin analysis from a corner
--Order : 1 if we go forward, increasing order; -1 if we go back, decreasing order
sortRays :: Int -> (EdgePos, Marking) -> (EdgePos, Marking) -> Ordering
sortRays order ((_,num),_) ((_,num2),_)  | order == 1 && num < num2 = LT
                                         | order == 1  = GT
                                         | order == -1 && num < num2 = GT
                                         | order == -1 = LT

--Find all non-empty and empty cells around the edges based on whether rays are reflected and have been absorbed a step back
calcEdgeCells :: Interactions -> Int -> (Atoms, Atoms)
calcEdgeCells inters gridSize = (\el -> (filterOutOfBounds (concat (fst el)), 
                                         filterOutOfBounds (concat (snd el)))) $ unzip [ calcForwardBackwards x | x <- [1,-1] ]
   where
     --Calculate from West to East (North to South), but also vice versa to gain as much information as possible
     calcForwardBackwards direction = calcEdgeCells' sortInteractions gridSize direction False [] []
        where
          sortInteractions = concat [ sortBy (sortRays direction) (filter (\el -> fst (fst el) == side) inters) | side <- [North, West, East, South] ]

     --Calculate what cells are empty and what cells have atoms around the edges by analysing from each corner deterministically
     --F :: Game Interactions -> Size of grid -> Direction of analysis -> Absorbed previously -> List of atoms -> List of empty cells
     calcEdgeCells' :: Interactions -> Int -> Int -> Bool -> Atoms -> Atoms -> (Atoms, Atoms)
     calcEdgeCells' [] _ _ _ nonEmpty empty = (nonEmpty, empty)
     calcEdgeCells' (((North,num),Reflect):xs) gridSize dir False nonEmpty empty = calcEdgeCells' xs gridSize dir True ((num+dir,1):nonEmpty) empty
     calcEdgeCells' (((West,num),Reflect):xs) gridSize dir False nonEmpty empty = calcEdgeCells' xs gridSize dir True ((1,num+dir):nonEmpty) empty
     calcEdgeCells' (((East,num),Reflect):xs) gridSize dir False nonEmpty empty = calcEdgeCells' xs gridSize dir True ((gridSize,num+dir):nonEmpty) empty
     calcEdgeCells' (((South,num),Reflect):xs) gridSize dir False nonEmpty empty = calcEdgeCells' xs gridSize dir True ((num+dir,gridSize):nonEmpty) empty

     calcEdgeCells' ((_,Reflect):xs) gridSize dir True nonEmpty empty = calcEdgeCells' xs gridSize dir True nonEmpty empty
     calcEdgeCells' ((_,Absorb):xs) gridSize dir _ nonEmpty empty = calcEdgeCells' xs gridSize dir True nonEmpty empty

     calcEdgeCells' (((North,num),_):xs) gridSize dir _ nonEmpty empty = calcEdgeCells' xs gridSize dir False nonEmpty ((num-1,1):(num,1):(num+1,1):empty)
     calcEdgeCells' (((West,num),_):xs) gridSize dir _ nonEmpty empty = calcEdgeCells' xs gridSize dir False nonEmpty ((1,num-1):(1,num):(1,num+1):empty)
     calcEdgeCells' (((East,num),_):xs) gridSize dir _ nonEmpty empty = calcEdgeCells' xs gridSize dir False nonEmpty ((gridSize,num-1):(gridSize,num):(gridSize,num+1):empty)
     calcEdgeCells' (((South,num),_):xs) gridSize dir _ nonEmpty empty = calcEdgeCells' xs gridSize dir False nonEmpty ((num-1,gridSize):(num,gridSize):(num+1,gridSize):empty)

     filterOutOfBounds :: Atoms -> Atoms
     filterOutOfBounds = nub . filter (\el -> fst el > 0 && fst el <= gridSize && snd el > 0 && snd el <= gridSize)

--Get the cells of the row/column in which a ray has been absorbed
getAbsorbLines :: Interactions -> Int -> Atoms
getAbsorbLines [] gridSize = []
getAbsorbLines (((side,num),Absorb):xs) gridSize | side == North || side == South = [ (num,y) | y <- [1..gridSize] ] ++ getAbsorbLines xs gridSize
                                                 | otherwise                      = [ (x,num) | x <- [1..gridSize] ] ++ getAbsorbLines xs gridSize
getAbsorbLines (_:xs) gridSize = getAbsorbLines xs gridSize

--Get atoms and empty cells by firing rays from one corner to another, greedily collecting all empty cells and atoms possible
--Func :: Interactions -> Grid Size -> Direction (+1 or -1 for forward or backwards) -> Empty Cells
narrowGrid :: Interactions -> Int -> Int -> Atoms -> (Atoms, Atoms)
narrowGrid ((_,Absorb):xs) _ _ _ = ([],[])
narrowGrid (((side,num),Reflect):xs) gridSize _ _ = (getAtomFromReflect side num gridSize, [])
narrowGrid (((side,num),Path (side2,num2)):xs) gridSize dir emptyCells 
            | side == oppositeSide side2 = narrowGrid xs gridSize dir (emptyCells ++ getEmptyCells side dir gridSize num)
            | otherwise  = (getAtomFromPath side side2 num num2, emptyCells ++ getEmptyCellsUpTo side dir gridSize num num2)
narrowGrid [] _ _ _ = ([],[])

--Grabs the atom that caused a deflection when narrowing the grid
--Func :: Ray Fired From -> Ray Hit -> Fire Pos -> Hit Pos
getAtomFromPath :: Side -> Side -> Int -> Int -> Atoms
getAtomFromPath North West rayStart rayHit = [(rayStart+1, rayHit+1)]
getAtomFromPath South West rayStart rayHit = [(rayStart+1, rayHit-1)]
getAtomFromPath North East rayStart rayHit = [(rayStart-1, rayHit+1)]
getAtomFromPath South East rayStart rayHit = [(rayStart-1, rayHit-1)]
getAtomFromPath East North rayStart rayHit = [(rayHit-1, rayStart+1)]
getAtomFromPath West North rayStart rayHit = [(rayHit+1, rayStart+1)]
getAtomFromPath East South rayStart rayHit = [(rayHit-1, rayStart-1)]
getAtomFromPath West South rayStart rayHit = [(rayHit+1, rayStart-1)]

--Take the position of the atom that caused a deflection in the corner
--Func :: Side of ray -> Corner on the side -> Size of Grid
getAtomFromReflect :: Side -> Int -> Int -> Atoms
getAtomFromReflect West 1 _ = [(1,2)]
getAtomFromReflect West num _ = [(1,num-1)]
getAtomFromReflect East 1 gridSize = [(gridSize,2)]
getAtomFromReflect East num gridSize = [(gridSize, num-1)]
getAtomFromReflect North 1 _ = [(2,1)]
getAtomFromReflect North num _ = [(num-1,1)]
getAtomFromReflect South 1 gridSize = [(2, gridSize)]
getAtomFromReflect South num gridSize = [(num-1, gridSize)]

--Take the empty cells for a whole line
--Func :: Side of ray -> Size of grid -> Position of ray
getEmptyCells :: Side -> Int -> Int -> Int -> Atoms
getEmptyCells West dir gridSize rayPos = [ (x,y) | x <- [1..gridSize], y <- [rayPos, rayPos+dir] ]
getEmptyCells East dir gridSize rayPos = [ (x,y) | x <- reverse [1..gridSize], y <- [rayPos, rayPos+dir] ]
getEmptyCells North dir gridSize rayPos = [ (x,y) | x <- [rayPos, rayPos+dir], y <- [1..gridSize] ]
getEmptyCells South dir gridSize rayPos = [ (x,y) | x <- [rayPos, rayPos+dir], y <- reverse [1..gridSize] ]

--Take the empty cells up to a point
getEmptyCellsUpTo :: Side -> Int -> Int -> Int -> Int -> Atoms
getEmptyCellsUpTo West dir gridSize rayPos takeTo = [ (x,y) | x <- [1..takeTo], y <- [rayPos, rayPos+dir] ]
getEmptyCellsUpTo East dir gridSize rayPos takeTo = [ (x,y) | x <- reverse [takeTo..gridSize], y <- [rayPos, rayPos+dir] ]
getEmptyCellsUpTo North dir gridSize rayPos takeTo = [ (x,y) | x <- [rayPos, rayPos+dir], y <- [1..takeTo] ]
getEmptyCellsUpTo South dir gridSize rayPos takeTo = [ (x,y) | x <- [rayPos, rayPos+dir], y <- reverse [takeTo..gridSize] ]

--Get the side across
oppositeSide :: Side -> Side
oppositeSide North = South
oppositeSide South = North
oppositeSide West = East
oppositeSide East = West

--Challenge 3
---------------------------------------------------------------------------------

data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

--Pattern match on the whole expression
prettyPrint :: LamExpr -> String
prettyPrint (LamVar n) = "x" ++ show n
prettyPrint (LamAbs n1 (LamAbs n2 (LamApp (LamVar n3) expr))) | n2 == n3 && all isDigit (prettyPrint expr) = show $ addOne $ prettyPrint expr
                                                              | otherwise = "\\x" ++ show n1 ++ " -> " ++ 
                                                                            "\\x" ++ show n2 ++ " -> " ++ 
                                                                            "x" ++ show n3 ++ " " ++ prettyPrint expr
prettyPrint (LamAbs n1 (LamAbs n2 expr)) | checkEqualVars n1 (prettyPrint expr) = "0"
                                         | otherwise = "\\x" ++ show n1 ++ " -> " ++ "\\x" ++ show n2 ++ " -> " ++ prettyPrint expr
prettyPrint (LamAbs n1 expr) = "\\x" ++ show n1 ++ " -> " ++ prettyPrint expr
prettyPrint (LamApp (LamVar n1) expr) = "x" ++ show n1 ++ " " ++ prettyPrint expr
prettyPrint (LamApp expr1 expr2) = "(" ++ prettyPrint expr1 ++ ") " ++ prettyPrint expr2

--Increment for scott numerals
addOne :: String -> Int
addOne [] = 1
addOne (x:xs) = digitToInt x * 10 ^ (length xs + 1) + addOne xs

checkEqualVars :: Int -> String -> Bool
checkEqualVars n1 n2 = show n1 == drop 1 n2

--Challenge 4
------------------------------------------------------------------------------

data LetExpr = LetApp LetExpr LetExpr | LetDef [([Int], LetExpr)] LetExpr | LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)

--Return Nothing on a failed parse
parseLet :: String -> Maybe LetExpr
parseLet str | (parse expr str) == [] = Nothing
             | otherwise = Just (fst (head (parse expr str)))

--Entry point to the parser
expr :: Parser LetExpr
expr = num <|> var <|> fun <|> letIn <|> bracketExpr

--"expr expr", accepts only if abc123 is a number
num :: Parser LetExpr
num = do n <- digits
         char ' '
         e <- expr
         return (LetApp (LetNum n) e)
         <|> do n <- digits
                return (LetNum n)

--"let expr; expr in expr"
letIn :: Parser LetExpr
letIn = do string "let "
           eq <- eqnList
           string " in "
           exp <- expr
           return (LetDef eq exp) 

--"(expr)"
bracketExpr :: Parser LetExpr
bracketExpr = do char '('
                 exp2 <- expr
                 char ')'
                 return exp2

--"expr; expr"
eqnList :: Parser [([Int], LetExpr)]
eqnList = do eq <- eqn
             eq1 <- eqnList
             string "; "
             eq2 <- eqn
             return (eq ++ eq2)
             <|> do eq <- eqn
                    string "; "
                    eq2 <- eqn
                    return (eq ++ eq2)
                    <|> eqn
--"functions variables = expr"
eqn :: Parser [([Int], LetExpr)]
eqn = do f <- funInt
         char ' '
         v <- varList
         string " = "
         exp <- expr
         return ([(f:v, exp)])

--"f123 "
fun :: Parser LetExpr
fun = do char 'f'
         d <- digits
         char ' '
         e <- expr
         return (LetApp (LetFun d) e)
         <|> do char 'f'
                d <- digits
                return (LetFun d)

--Grab the digit/number of the function
funInt :: Parser Int
funInt = do char 'f'
            d <- digits
            return d

--"x123 "
var :: Parser LetExpr
var = do char 'x'
         d <- digits
         char ' '
         e <- expr
         return (LetApp (LetVar d) e)
         <|> do char 'x'
                d <- digits
                return (LetVar d)

--Grab the digit/number of the variable
varInt :: Parser [Int]
varInt = do char 'x'
            d <- digits
            return [d]

--Grab all digits of all variables in the list
varList :: Parser [Int]
varList = varInt <|> do v1 <- varInt
                        v2 <- varList
                        return (v1 ++ v2)

--Get the digits as integers
digits :: Parser Int
digits = do d <- digits'
            return (read d)

--Get the digits as Strings 
digits' :: Parser String
digits' = do d1 <- some digit
            -- char ' '
             d2 <- digits'
             return (d1 ++ d2)
             <|> do d <- some digit
                    return d


-- Challenge 5
---------------------------------------------------------------------------------------------

data CLExpr = S | K  | I | CLVar Int | CLApp CLExpr CLExpr deriving (Show,Read,Eq)

--Pattern match on the lambda expression. Since the transformation is not strongly defined for lambda abstractions
--(i.e. we end up having both lambda expr and combinatory logic expr at the same time), the function is looking "in the future"
--to determine what the lambda abstractions will end up like.
clTransform :: LamExpr -> CLExpr
clTransform (LamVar n) = CLVar n
clTransform (LamApp lamexp1 lamexp2) = CLApp (clTransform lamexp1) (clTransform lamexp2)
clTransform (LamAbs n1 (LamVar n2)) | n1 == n2  = I
                                    | otherwise = CLApp K (CLVar n2)
clTransform (LamAbs n1 (LamAbs n2 lamexp)) | freeVar n1 lamexp && freeVarCL n1 absTransf && isCLApp absTransf = 
                                                     CLApp (CLApp S (clTransformFree n1 (fst clExpr))) (clTransformFree n1 (snd clExpr))
                                           | freeVar n1 lamexp && freeVarCL n1 absTransf && (absTransf == CLVar n1) = I
                                           | otherwise = CLApp K absTransf
  where
    absTransf = clTransform (LamAbs n2 lamexp)
    clExpr = clAppExpr absTransf

clTransform (LamAbs n1 (LamApp lamexpr1 lamexpr2)) | freeVar n1 lamexpr1 || freeVar n1 lamexpr2 = CLApp (CLApp S (clTransform (LamAbs n1 lamexpr1))) (clTransform (LamAbs n1 lamexpr2))
                                                   | otherwise = CLApp K (CLApp (clTransform lamexpr1) (clTransform lamexpr2))

--Transform a combinatory logic expression based on whether the variable bound to it is free
clTransformFree :: Int -> CLExpr -> CLExpr
clTransformFree x e@(CLApp expr1 expr2) | freeVarCL x e = CLApp (CLApp S (clTransformFree x expr1)) (clTransformFree x expr2)
                                        | otherwise     = CLApp K e
clTransformFree x (CLVar y) | x == y    = I
                            | otherwise = CLApp K (CLVar y)
clTransformFree x a = CLApp K a 

--Check if a variable is free within a lambda expression
freeVar :: Int -> LamExpr -> Bool
freeVar x (LamVar y) = x == y
freeVar x (LamAbs y lamexp) | x == y = False
                            | otherwise = freeVar x lamexp
freeVar x (LamApp lamexp1 lamexp2) = freeVar x lamexp1 || freeVar x lamexp2

--Check if a variable is free within a combinatory logic expression
freeVarCL :: Int -> CLExpr -> Bool
freeVarCL x (CLVar y) = x == y
freeVarCL x (CLApp clexpr1 clexpr2) = freeVarCL x clexpr1 || freeVarCL x clexpr2
freeVarCL x _ = False

--Check if the given expression is a combinatory logic application
isCLApp :: CLExpr -> Bool
isCLApp (CLApp _ _) = True
isCLApp _ = False

--Extract both expressions from a combinatory logic application
clAppExpr :: CLExpr -> (CLExpr, CLExpr)
clAppExpr (CLApp expr1 expr2) = (expr1, expr2)
clAppExpr a = (a,a)

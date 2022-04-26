-- WRITTEN BY HRISTIYAN GEORGIEV


import Tokens
import Grammar
import Read
import System.Environment
import System.IO
import Control.Exception
import Data.List
import Data.Char


main :: IO ()
main = catch main' errCatch

main' = do
  (fileName : _) <- getArgs
  fileText <- readFile fileName
  fileContent <- unwrapAST $ parseTurtle $ alexScanTokens fileText
  putStrLn fileContent

errCatch :: ErrorCall -> IO ()
errCatch e = do
  let err =  show e
  hPutStr stderr ("ERR: " ++ err)
  return ()

unwrapAST :: Exp -> IO String
unwrapAST (Read files) = do
  fileContent <- readTurtle files
  return $ unzipTriples $ nub $ sortBy sortTriples $ concatMap (fromFileToTriples . snd) fileContent

unwrapAST (ReadOp files op) = do
  fileContent <- readTurtle files
  return $ unzipTriples $ nub $ sortBy sortTriples $ concatMap snd $ applyOp (map (\x -> (fst x, fromFileToTriples (snd x))) fileContent) op

unwrapAST (SeqExps a b) = do
  expEval1 <- unwrapAST a
  expEval2 <- unwrapAST b
  return $ expEval1 ++ expEval2

readTurtle :: FileList -> IO [(String, String)]
readTurtle (File fileName) = do
  contents <- readFile $ fileName ++ ".ttl"
  return [(fileName, contents)]

readTurtle (Files fileList1 fileList2) = do
  contents1 <- readTurtle fileList1
  contents2 <- readTurtle fileList2
  return $ contents1 ++ contents2

--Takes the file content and an operation to apply to
applyOp :: [(String,[(String, String, String)])] -> Operation -> [(String, [(String, String, String)])]
applyOp triples (Where cond) = solveCond triples cond
applyOp triples (WhereIf (If cond act)) = solveIf triples (solveCond triples cond) act
applyOp triples (WhereIf (IfElse cond act1 act2)) = solveIfElse triples (solveCond triples cond) act1 act2
applyOp triples (WhereIf (SeqIfs if1 if2)) = applyOp (applyOp triples (WhereIf if1)) (WhereIf if2)
applyOp triples (WhereSeq cond (If condIf act)) = solveIf condTriples (solveCond condTriples condIf) act
   where condTriples = solveCond triples cond
applyOp triples (WhereSeq cond (IfElse condIf act1 act2)) = solveIfElse condTriples (solveCond condTriples condIf) act1 act2
   where condTriples = solveCond triples cond
applyOp triples (WhereSeq cond (SeqIfs if1 if2)) = applyOp (applyOp condTriples (WhereIf if1)) (WhereIf if2)
   where condTriples = solveCond triples cond

--Gets the value of the operand within the context
evalOperand :: [(String,[(String, String, String)])] -> Operand -> (String, [String])
evalOperand triples (Subject str) = (str, map fst3 (snd $ head $ filter (\x -> fst x == str) triples))
evalOperand triples (Predicate str) = (str, map snd3 (snd $ head $ filter (\x -> fst x == str) triples))
evalOperand triples (String str) = ("", replicate (biggestTriple triples 0) (removeQuotes str))
evalOperand triples (Bool True) = ("", replicate (biggestTriple triples 0) "true")
evalOperand triples (Bool False) = ("", replicate (biggestTriple triples 0) "false")
evalOperand triples (Eval op) = evalOperandRec triples op

-- Evaluates operands that are in itself operations, e.g. foo.OBJ+4
evalOperandRec :: [(String,[(String, String, String)])] -> OperandEval -> (String, [String])
evalOperandRec triples (Object str) = (str, map (removeQuotes . trd3) (snd $ head $ filter (\x -> fst x == str) triples))
evalOperandRec triples (Int n) = ("", replicate (biggestTriple triples 0) (show n))
evalOperandRec triples (Negate n) = ("", replicate (biggestTriple triples 0) ("-" ++ show n))
evalOperandRec triples (Add opRec1 opRec2) = (getContext opRec1 opRec2,
  operStrings (snd $ evalOperandRec triples opRec1) (snd $ evalOperandRec triples opRec2) (+))
evalOperandRec triples (Subtract opRec1 opRec2) = (getContext opRec1 opRec2,
  operStrings (snd $ evalOperandRec triples opRec1) (snd $ evalOperandRec triples opRec2) (-))

--Returns the context of an operation, e.g. foo.OBJ+3 is in the context of foo, whereas foo.OBJ+3-foo.OBJ is ambiguous
getContext :: OperandEval -> OperandEval -> String
getContext op1 op2 = getContext' (getContextRec op1 op2)

getContext' :: OperandEval -> String
getContext' (Object str) = str
getContext' _ = []

getContextRec :: OperandEval -> OperandEval -> OperandEval
getContextRec Object {} Object {} = error "Ambiguous condition for query"
getContextRec (Add a1 a2) (Add a3 a4) = getContextRec (getContextRec a1 a2) (getContextRec a3 a4)
getContextRec (Add a1 a2) (Subtract s1 s2) = getContextRec (getContextRec a1 a2) (getContextRec s1 s2)
getContextRec (Subtract s1 s2) (Add a1 a2) = getContextRec (getContextRec s1 s2) (getContextRec a1 a2)
getContextRec (Subtract s1 s2) (Subtract s3 s4) = getContextRec (getContextRec s1 s2) (getContextRec s3 s4)
getContextRec (Add a1 a2) b = getContextRec (getContextRec a1 a2) b
getContextRec b (Add a1 a2) = getContextRec b (getContextRec a1 a2)
getContextRec (Subtract s1 s2) b = getContextRec (getContextRec s1 s2) b
getContextRec b (Subtract s1 s2) = getContextRec b (getContextRec s1 s2)
getContextRec a@(Object str) b = a
getContextRec a b@(Object str) = b
getContextRec a b = a

--Returns all triples that the condition applies to
solveCond :: [(String, [(String, String, String)])] -> Cond -> [(String, [(String, String, String)])]
solveCond triples (Gt a b) = solveCond' (filterTriples triples (fst (evalOp a))) (snd $ evalOp a) (snd $ evalOp b) gtFunc
  where evalOp = evalOperandRec triples
solveCond triples (Gte a b) = solveCond' (filterTriples triples (fst (evalOp a))) (snd $ evalOp a) (snd $ evalOp b) gteFunc
  where evalOp = evalOperandRec triples
solveCond triples (Lt a b) = solveCond' (filterTriples triples (fst (evalOp a))) (snd $ evalOp a) (snd $ evalOp b) ltFunc
  where evalOp = evalOperandRec triples
solveCond triples (Lte a b) = solveCond' (filterTriples triples (fst (evalOp a))) (snd $ evalOp a) (snd $ evalOp b) lteFunc
  where evalOp = evalOperandRec triples
solveCond triples (Eql a b) = solveCond' (filterTriples triples (fst (evalOp a))) (snd $ evalOp a) (snd $ evalOp b) eqlFunc
  where evalOp = evalOperand triples
solveCond triples (And c1 c2) = intersectTriples (applyOp triples (Where c1)) $ applyOp triples (Where c2)
solveCond triples (Or c1 c2) = unionTriples (applyOp triples (Where c1)) $ applyOp triples (Where c2)

solveCond' :: [(String, [(String, String, String)])] -> [String] -> [String] -> (String -> [String] -> Bool) -> [(String, [(String, String, String)])]
solveCond' [] _ _ _ = []
solveCond' _ [] _ _ = []
solveCond' relevantTriples (eq1:eq1s) eq2s condFunc | condFunc eq1 eq2s =
  joinTriples (getFirstTriples relevantTriples) $ solveCond' (map (\(a,b) -> (a, safeTail b)) relevantTriples) eq1s eq2s condFunc
                                                           | otherwise =
  solveCond' (map (\(a,b) -> (a, safeTail b)) relevantTriples) eq1s eq2s condFunc

intersectTriples :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> [(String, [(String, String, String)])]
intersectTriples [] _ = []
intersectTriples (tr:trps) trps2 | fst tr `elem` map fst trps2 =
 (fst tr, intersect (snd tr) $ snd $ head $ filter (\x -> fst tr == fst x) trps2) : intersectTriples trps trps2
                         | otherwise = intersectTriples trps trps2

--Gets the elements from the first list that are not in the second
leftOuterTriples :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> [(String, [(String, String, String)])]
leftOuterTriples [] _ = []
leftOuterTriples trps [] = trps
leftOuterTriples (tr:trps) trps2 | fst tr `elem` map fst trps2 =
  (fst tr, [ triple | triple <- snd tr, triple `notElem` contextTr ]) : leftOuterTriples trps trps2
                                 | otherwise = leftOuterTriples trps trps2
    where contextTr = snd $ head $ filter (\x -> fst tr == fst x) trps2

--Unions the results of two conditions
--If the next element from the first list of triples has the same context as in the second list, merge their triples and remove
--them from the second list of triples. At the end of the recursion, return the second list (whose contexts are now not in the first)
unionTriples :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> [(String, [(String, String, String)])]
unionTriples [] trps2 = trps2
unionTriples (tr:trps) trps2 | fst tr `elem` map fst trps2 =
  (fst tr, nub $ snd tr ++ snd (head $ filter (\x -> fst tr == fst x) trps2)) : unionTriples trps (filter (\x -> fst tr /= fst x) trps2)
                        | otherwise = tr : unionTriples trps trps2

--Same as unionTriples but doesn't remove same elements
unionTriplesRep :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> [(String, [(String, String, String)])]
unionTriplesRep [] trps2 = trps2
unionTriplesRep (tr:trps) trps2 | fst tr `elem` map fst trps2 =
  (fst tr, snd tr ++ snd (head $ filter (\x -> fst tr == fst x) trps2)) : unionTriplesRep trps (filter (\x -> fst tr /= fst x) trps2)
                        | otherwise = tr : unionTriplesRep trps trps2

--Solve an IF query
solveIf :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> Action -> [(String, [(String, String, String)])]
solveIf [] _ _ = []
solveIf allTriples condTriples act = unionTriplesRep (leftOuterTriples allTriples condTriples) (doAction condTriples act)

solveIfElse :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> Action -> Action -> [(String, [(String, String, String)])]
solveIfElse allTriples condTriples act1 act2 =
  unionTriplesRep (doAction condTriples act1) (doAction (leftOuterTriples allTriples condTriples) act2)

--Performs a certain action on the relevant triples
doAction :: [(String, [(String, String, String)])] -> Action -> [(String, [(String, String, String)])]
doAction [] _ = []
doAction triples Drop = []
doAction triples (NestIf (NIf cond act)) = applyOp triples (WhereIf (If cond act))
doAction triples (NestIf (NIfElse cond act1 act2)) = applyOp triples (WhereIf (IfElse cond act1 act2))
doAction triples act = unionTriplesRep (getChangedTriples triples act) (getInsertTriples triples act)
--doAction triples act = getVal triples act

--Get a list of all triples that are inserted within an action
getInsertTriples :: [(String, [(String, String, String)])] -> Action -> [(String, [(String, String, String)])]
getInsertTriples triples act@(Insert subj pred obj) = getVal triples act
getInsertTriples triples act@(SeqActs a1 a2) = getInsertTriples triples a1 ++ getInsertTriples triples a2
getInsertTriples _ _ = []

--Get a list of all changed triples within an action.
--Actions are done sequentially: CHANGE a AND CHANGE b will change to a then b, and return b.
--CHANGE a AND IF ... will change to a, then pass a to IF and perform actions there.
--IF ... AND CHANGE a will change the output of IF to a
getChangedTriples :: [(String, [(String, String, String)])] -> Action -> [(String, [(String, String, String)])]
getChangedTriples triples Drop = []
getChangedTriples triples (NestIf (NIf cond act)) = applyOp triples (WhereIf (If cond act))
getChangedTriples triples (NestIf (NIfElse cond act1 act2)) = applyOp triples (WhereIf (IfElse cond act1 act2))
getChangedTriples triples act@(Change subj pred obj) = getVal triples act
getChangedTriples triples (SeqActs a1 a2) = getChangedTriples (getChangedTriples triples a1) a2
getChangedTriples triples _ = triples

--Perform an action on triples and get the resulting values
getVal :: [(String, [(String, String, String)])] -> Action -> [(String, [(String, String, String)])]
getVal triples act@(Change subj pred obj)
  | length triples > 1 && isNonContext subj = error $ "Ambiguous CHANGE SUBJECT query, number of contexts: " ++ show (length triples)
  | length triples > 1 && isNonContext pred = error $ "Ambiguous CHANGE PREDICATE query, number of contexts: " ++ show (length triples)
  | length triples > 1 && isNonContext obj = error $ "Ambiguous CHANGE OBJECT query, number of contexts: " ++ show (length triples)
  | declNotURI subj || declNotURI pred = error "Bad CHANGE query: SUBJECT and PREDICATE declarations must be of URI format"
  | otherwise = getVal' triples act
getVal triples act@(Insert subj pred obj)
  | length triples > 1 && isNonContext subj = error $ "Ambiguous INSERT SUBJECT query, number of contexts: " ++ show (length triples)
  | length triples > 1 && isNonContext pred = error $ "Ambiguous INSERT PREDICATE query, number of contexts: " ++ show (length triples)
  | length triples > 1 && isNonContext obj = error $ "Ambiguous INSERT OBJECT query, number of contexts: " ++ show (length triples)
  | declNotURI subj || declNotURI pred = error "Bad INSERT query: SUBJECT and PREDICATE declarations must be of URI format"
  | otherwise = getVal' triples act
getVal triples _ = triples -- Can't reach this state

getVal' :: [(String, [(String, String, String)])] -> Action -> [(String, [(String, String, String)])]
getVal' [] _ = []
getVal' t@((ctxt,trs):triples) act@(Change subj pred obj) =
  (ctxt, map (\trip -> (getValueOf trip subj, getValueOf trip pred, getValueOf trip obj)) trs) : getVal' triples act
getVal' t@((ctxt,trs):triples) act@(Insert subj pred obj) =
  (ctxt, map (\trip -> (getValueOf trip subj, getValueOf trip pred, getValueOf trip obj)) trs) : getVal' triples act
getVal' t _ = t -- Can't reach this state

--Get the value of a single operand
getValueOf :: (String, String, String) -> OperandAct -> String
getValueOf (subj, _, _) Subj = subj
getValueOf (_, pred, _) Pred = pred
getValueOf (_, _, obj) Obj = removeQuotes obj
getValueOf t (Oper operand) = getValueOf' t operand

getValueOf' :: (String, String, String) -> Operand -> String
getValueOf' t@(subj, pred, obj) (Eval ev) = getValueOfRec t ev
getValueOf' (subj, pred, obj) (Subject str) = subj
getValueOf' (subj, pred, obj) (Predicate str) = pred
getValueOf' _ (String str) = removeQuotes str
getValueOf' _ (Bool True) = "true"
getValueOf' _ (Bool False) = "false"

getValueOfRec :: (String, String, String) -> OperandEval -> String
getValueOfRec t@(subj, pred, obj) (Add op1 op2) = show $ read (getValueOfRec t op1) + read (getValueOfRec t op2)
getValueOfRec t@(subj, pred, obj) (Subtract op1 op2) = show $ read (getValueOfRec t op1) - read (getValueOfRec t op2)
getValueOfRec (subj, pred, obj) (Object str) = removeQuotes obj
getValueOfRec _ (Negate n) = '-':show n
getValueOfRec _ (Int n) = show n

------------------------ HELPER FUNCTIONS

--Checks if an operand is free of context (e.g. SUBJ instead of foo.SUBJ)
isNonContext :: OperandAct -> Bool
isNonContext Oper {} = False
isNonContext _ = True

-- LESS THAN, GREATER THAN, EQUALS
lteFunc :: String -> [String] -> Bool
lteFunc n xs | isInt n   = all (read n <=) $ readIntList xs
             | otherwise = False

ltFunc :: String -> [String] -> Bool
ltFunc n xs | isInt n   = all (read n <) $ readIntList xs
            | otherwise = False

gteFunc :: String -> [String] -> Bool
gteFunc n xs | isInt n   = all (read n >=) $ readIntList xs
             | otherwise = False

gtFunc :: String -> [String] -> Bool
gtFunc n xs | isInt n   = all (read n >) $ readIntList xs
            | otherwise = False

eqlFunc :: String -> [String] -> Bool
eqlFunc n xs = n `elem` xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail a = tail a

isInt :: String -> Bool
isInt [] = False
isInt n | all isDigit n || head n == '-' && all isDigit (tail n)  = True
        | otherwise     = False

isBool :: String -> Bool
isBool "true" = True
isBool "false" = True
isBool _ = False

isURI :: String -> Bool
isURI str | take 8 str == "<http://" && last str == '>' = True
          | otherwise                                   = False

declNotURI :: OperandAct -> Bool
declNotURI (Oper (String str)) = not $ isURI $ removeQuotes str
declNotURI _ = False

isLiteral :: String -> Bool
isLiteral str = not (isInt str) && not (isBool str) && not (isURI str)

readInt :: String -> Int
readInt = read

--Read only those strings that are ints from a list of strings
readIntList :: [String] -> [Int]
readIntList [] = []
readIntList (x:xs) | all isDigit x || head x == '-' && all isDigit (tail x) = read x : readIntList xs
                   | otherwise     = readIntList xs

--Checks if all strings in a list are integers
checkAllInts :: [String] -> Bool
checkAllInts [] = True
checkAllInts ([]:xs) = False
checkAllInts (x:xs) | all isDigit x || head x == '-' && all isDigit (tail x) = checkAllInts xs
                    | otherwise = False

--Add two lists of strings if each head is an int
operStrings :: [String] -> [String] -> (Int -> Int -> Int) -> [String]
operStrings [] _ _ = []
operStrings _ [] _ = []
operStrings (x:xs) (y:ys) oper | isInt x && isInt y = show (oper (read x) (read y)) : operStrings xs ys oper
                               | otherwise          = "nonINT" : operStrings xs ys oper

--returns the length of the biggest list of triples
biggestTriple :: [(String, [(String, String, String)])] -> Int -> Int
biggestTriple [] leng = leng
biggestTriple (x:xs) leng | length (snd x) > leng = biggestTriple xs $ length (snd x)
                          | otherwise             = biggestTriple xs leng

-- Custom filter behaviour, when given an empty string as a filter, it returns everything
filterTriples :: [(String, [(String, String, String)])] -> String -> [(String, [(String, String, String)])]
filterTriples a [] = a
filterTriples a str = filter (\x -> str == fst x) a

joinTriples :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])] -> [(String, [(String, String, String)])]
joinTriples [] _ = []
joinTriples a [] = a -- The first time we call this function, second list is empty
joinTriples ((str,tripleTo):joinTo) ((_,triplesFrom):joinFrom) = (str,tripleTo ++ triplesFrom) : joinTriples joinTo joinFrom

-- Takes the first element from each list of triples
getFirstTriples :: [(String, [(String, String, String)])] -> [(String, [(String, String, String)])]
getFirstTriples [] = []
getFirstTriples ((str,[]) : triples) = (str,[]) : getFirstTriples triples
getFirstTriples ((str,trpls) : triples) = (str, [head trpls]) : getFirstTriples triples

--Takes all triples and puts them inside a single string, with a new line as a delimiter
unzipTriples :: [(String, String, String)] -> String
unzipTriples [] = ""
unzipTriples ((a, b, c):xs) = a ++ " " ++ b ++ " " ++ c ++ " .\n" ++ unzipTriples xs

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

--Removes quotes from the beginning and end of a string
removeQuotes :: String -> String
removeQuotes [] = []
removeQuotes [x] = [x]
removeQuotes str | head str == '\"' && last str == '\"' = tail $ init str
                 | otherwise                            = str

sortTriples :: (String, String, String) -> (String, String, String) -> Ordering
sortTriples t1@(s1, p1, o1) t2@(s2, p2, o2) | s1 == s2 && p1 == p2 = sortObj o1 o2
                                            | otherwise            = compare t1 t2

sortObj :: String -> String -> Ordering
sortObj o1 o2 | isURI o1 && isURI o2 = compare o1 o2
              | isURI o1             = LT
              | isURI o2             = GT
              | isLiteral o1 && isLiteral o2 = compare o1 o2
              | isLiteral o1                 = GT
              | isLiteral o2                 = LT
              | otherwise                    = compare o1 o2

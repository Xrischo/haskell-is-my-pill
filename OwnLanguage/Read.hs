-- MOST OF THIS FILE IS WRITTEN BY Edward Clewer
--
-- Haskell file that reformats simple Turtle documents
-- NOTE: We treat ':', ';' and ',' as special characters
--          -> A line cannot have a ':', unless within the 'http://' or for prefix substitution
--          -> A line cannot have a ';', unless for separating a predicate list
--          -> A line cannot have a ',', unless for separating an object list

module Read where
import Data.List
import Data.Char
import TTLParser

-- func :: [line, line, ...] -> [("base", line), ("prefix", line), ("triple", line), ...]
-- Takes each line from the list and categorises it as a triple, prefix or a base in tuples, and adds them in a list
parseLines :: [String] -> [(String, String)]
parseLines parsedSourceText = [parseLine p | p <- parsedSourceText]

parseLine :: String -> (String, String)
parseLine ('@':'b':'a':'s':'e':line) = ("base", line)
parseLine ('@':'p':'r':'e':'f':'i':'x':line) = ("prefix", line)
parseLine line = (categoriseTriple line, line) -- objectList, predicateList or triple

categoriseTriple :: String -> String
categoriseTriple str
  | elem ';' str = "predicateList"
  | elem ',' str = "objectList"
  | otherwise = "triple"

-- func :: "<aa> <bb> <cc>" -> Accumulator -> ["<aa>", "<bb>", "<cc>"]
-- Takes one line of triples and adds each element of it inside a list
parseTriple :: String -> String -> [String]
parseTriple "" acc = [acc]
parseTriple (' ':xs) acc = parseTriple xs acc
parseTriple ('>':xs) acc = (acc ++ ">") : (parseTriple xs "")
parseTriple (x:xs) acc = parseTriple xs (acc ++ [x])

-- func :: [("base", baseLine), ("prefix", line), ("triple", line), ...] -> baseLine
-- Finds the tuple categorised as "base" and returns its content
getBase :: [(String, String)] -> String
getBase lines = head $ map snd $ filter (\x -> fst x == "base") lines

-- Takes all prefixes and puts them into a list of tuples that hold the prefix name and its content
getPrefixes :: [(String, String)] -> [(String, String)]
getPrefixes [] = []
getPrefixes (("prefix",line):lines) = parsePrefix line "":getPrefixes lines
getPrefixes (_:lines) = getPrefixes lines

-- Takes a prefix and returns a tuple of the prefix name and its content
parsePrefix :: String -> String -> (String, String)
parsePrefix [] acc = (acc, [])
parsePrefix (':':xs) acc = (acc, xs)
parsePrefix (x:xs) acc = parsePrefix xs (acc ++ [x])

--classify the lines into triple, objectList or predicateList, then make tuples containing (classification, line)
getTriples :: [(String, String)] -> [String]
getTriples lines = (map snd $ filter (\x -> fst x == "triple") lines)
                   ++ (concat [expandObjectLists l | l <- (map snd $ filter (\x -> fst x == "objectList") lines)])
                   ++ (concat [expandPredicateLists p | p <- map snd $ filter (\x -> fst x == "predicateList") lines])

--takes an objectList and expands it into a list of the full triples that it describes
expandObjectLists :: String -> [String]
expandObjectLists string = [subject ++ predicate ++ object | object <- objects]
  where subject = (takeWhile (\x -> x /= '>') string) ++ ">"
        predicate = (takeWhile (\x -> x /= '>') (drop (length subject) string)) ++ ">"
        splitList = splitOnChar ',' string ""
        objects = drop ((length subject) + (length predicate)) splitList

--takes a predicateList and expands it into a list of the full triples that it describes
expandPredicateLists :: String -> [String]
expandPredicateLists string = [subject ++ predicateAndObject | predicateAndObject <- predicatesAndObjects]
  where subject = (takeWhile (\x -> x /= '>') string) ++ ">"
        splitList = splitOnChar ';' string ""
        predicatesAndObjects = drop ((length subject)) splitList

--Splits a string into a list of strings on a given char delimiter
splitOnChar :: Char -> String -> String -> [String]
splitOnChar c [] acc = [acc]
splitOnChar c (x:xs) acc | c == x = (acc ++ [x]) : splitOnChar c xs []
                         | otherwise = splitOnChar c xs (acc ++ [x])

-- func :: "http://baseText/" -> [("foo", "<blabla/"), ("bar", "ewewew/"), ...] -> "<aa> <bb> <cc>" -> "<http://..> <http://..> <http://.."
-- Takes the base link, a list of prefixes categorised, and a triple, and returns the fully expanded triple
updateTriple :: String -> [(String, String)] -> String -> String
updateTriple base prefixes triple = updateBase base prefixes (pt !! 0)
                                    ++ updateBase base prefixes (pt !! 1)
                                    ++ updateBase base prefixes (pt !! 2)
  where pt = parseTriple triple []

prefixSwap :: [(String, String)] -> String -> String
prefixSwap [] string = string
prefixSwap prefixes string | take (length (fst (head prefixes))) (tail string) == fst (head prefixes) = init (snd (head prefixes)) ++ drop (length (fst (head prefixes)) + 1) (tail string)
                           | otherwise = prefixSwap (tail prefixes) string

removePrecedingSpace :: String -> String
removePrecedingSpace (' ':xs) = xs
removePrecedingSpace xs = xs

-- if its a link then no change, if a prefix then substitute, otherwise substitute the base
updateBase :: String -> [(String, String)] -> String -> String
updateBase base prefixes string | take 8 (removePrecedingSpace string) == "<http://" = string
                                | elem ':' string = prefixSwap prefixes string
                                | take 1 string == "<" = init base ++ tail (string)
                                | otherwise = string

addBaseToPrefix :: String -> String -> String
addBaseToPrefix base link@('<':'h':'t':'t':'p':':':'/':'/':xs) = link
addBaseToPrefix base xs = init base ++ tail xs

-- Functions below are all for readability

getTupleTriples sortedTriples = [((splitOnChar '>' triple "") !! 0, (splitOnChar '>' triple "") !! 1,
                                       (splitOnChar '>' triple "") !! 2) | triple <- sortedTriples]

prettyPrintTupleTriples :: [(String, String, String)] -> String
prettyPrintTupleTriples [] = ""
prettyPrintTupleTriples (x:xs) = show (x) ++ " \n" ++ (prettyPrintTupleTriples xs)

fromFileToTriples :: String -> [(String, String, String)]
fromFileToTriples sourceText = getTupleTriples (sort [updateTriple base prefixes t | t <- (getTriples parsedLines)])
  where parsedLines = parseLines (parseFile sourceText)
        base = getBase parsedLines
        prefixes = [(removePrecedingSpace a, addBaseToPrefix base (removePrecedingSpace b)) | (a,b) <- getPrefixes parsedLines]

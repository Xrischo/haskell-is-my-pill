-- WRITTEN BY Edward Clewer and Thomas Cutts
-- Using Graham Hutton's functional parsing library

module TTLParser where
import Parsing

parseFile :: String -> [String]
parseFile input
  | snd (parserText !! 0) /= "" = error "Invalid turtle file"
  | otherwise = fst (parserText !! 0)
  where parserText = parse file input

file :: Parser [String]
file = many line

line :: Parser String
line = base <|> prefix <|> triple

base :: Parser String
base = do space
          b <- symbol "@base"
          space
          char '<'
          space
          s <- validString
          space
          char '>'
          space
          char '.'
          space
          return ("@base<" ++ s ++ ">")

prefix :: Parser String
prefix = do space
            b <- symbol "@prefix"
            space
            name <- validPredicateNameString
            space
            char ':'
            space
            char '<'
            space
            s <- validString
            space
            char '>'
            space
            char '.'
            space
            return ("@prefix " ++ name ++ ": <" ++ s ++ ">")

triple :: Parser String
triple = do space
            ol <- objectList
            space
            return ol
          <|> do space
                 pl <- predicateList
                 space
                 return pl
          <|> do space
                 bt <- basicTriple
                 space
                 char '.'
                 space
                 return bt

subject :: Parser String
subject = hasBase <|> hasPrefix <|> noBase

predicate :: Parser String
predicate = hasBase <|> hasPrefix <|> noBase

object :: Parser String
object = hasBase <|> hasPrefix <|> noBase <|> literal

hasBase :: Parser String
hasBase = do space
             s <- symbol "<http://"
             xs <- validString
             char '>'
             space
             return (s ++ xs ++ ">")

hasPrefix :: Parser String
hasPrefix = do space
               s1 <- validPredicateNameString
               char ':'
               s2 <- validString
               space
               return ("<" ++ s1 ++ ":" ++ s2 ++ ">")

noBase :: Parser String
noBase = do space
            char '<'
            s <- validString
            char '>'
            space
            return ("<" ++ s ++ ">")

literal :: Parser String
literal = do space
             s <- symbol "true"
             space
             return s
          <|> do space
                 s <- symbol "false"
                 space
                 return s
          <|> do space
                 s <- char '+'
                 space
                 s2 <- int
                 space
                 return (show (s2))
          <|> do space
                 s <- char '-'
                 space
                 s2 <- int
                 space
                 return (s : (show (s2)))
          <|> do space
                 s <- int
                 space
                 return (show s)
          <|> do space
                 many (char '\"')
                 s <- validString
                 many (char '\"')
                 space
                 return (['\"'] ++ s ++ ['\"'])

basicTriple :: Parser String
basicTriple = do space
                 s <- subject
                 space
                 p <- predicate
                 space
                 o <- object
                 space
                 return (s ++ p ++ o)

predicateList :: Parser String
predicateList = do space
                   s <- subject
                   space
                   items <- predicateListItems
                   return (s ++ items)

predicateListItems :: Parser String
predicateListItems = do space
                        p <- predicate
                        space
                        o <- object
                        space
                        char ';'
                        space
                        ps <- predicateListItems
                        return (p ++ o ++ " ; " ++ ps)
                     <|> do space
                            p <- predicate
                            space
                            o <- object
                            space
                            char '.'
                            space
                            return (p ++ o)
                     <|> return ""

objectList :: Parser String
objectList = do space
                s <- subject
                space
                p <- predicate
                space
                items <- objectListItems
                space
                return (s ++ p ++ items)

objectListItems :: Parser String
objectListItems = do space
                     o <- object
                     space
                     char ','
                     space
                     os <- objectListItems
                     return (o ++ " , " ++ os)
                  <|> do space
                         o <- object
                         space
                         char '.'
                         space
                         return (o)

validString :: Parser String
validString = do x <- (sat allowedChar)
                 xs <- validString
                 return (x:xs)
                 <|> return ""

allowedChar :: Char -> Bool
allowedChar x = elem x (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '-', '#', '/', ':', '+'])

validPredicateNameString :: Parser String
validPredicateNameString = do x <- (sat allowedCharsForPredicateNaming)
                              xs <- validPredicateNameString
                              return (x:xs)
                              <|> return []

allowedCharsForPredicateNaming :: Char -> Bool
allowedCharsForPredicateNaming x = elem x (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '-', '#', '/', '+'])

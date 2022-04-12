{ 
module Grammar where 
import Tokens
import System.Environment
}

%name parseTurtle
%tokentype { Token } 
%error { parseError }
%token 
  READ     { TokenRead _ }
  WHERE    { TokenWhere _ }
  SUBJ     { TokenSubj _ }
  PRED     { TokenPred _ }
  OBJ      { TokenObj _ }
  AND      { TokenAnd _ }
  OR       { TokenOr _ }
  IF       { TokenIf _ }
  THEN     { TokenThen _ }
  ELSE     { TokenElse _ }
  END      { TokenEnd _ }
  DROP     { TokenDrop _ }
  CHANGE   { TokenChange _ }
  INSERT   { TokenInsert _ }
  '.'      { TokenDot _ }
  '<'      { TokenLBrack _ }
  '>'      { TokenRBrack _ }
  '='      { TokenEq _ }
  '('      { TokenLParen _ }
  ')'      { TokenRParen _ }
  '+'      { TokenPlus _ }
  '-'      { TokenMinus _ }
  file     { TokenFile _ $$ }
  string   { TokenString _ $$ }
  bool     { TokenBool _ $$ }
  int      { TokenInt _ $$ }

%right AND OR WHERE
%left '+' '-'
%left NEG
%%

Exp : READ FileList Operation                            { ReadOp $2 $3 }
    | READ FileList                                      { Read $2 }
    | Exp AND Exp                                        { SeqExps $1 $3 }

FileList : file FileList                                 { Files (File $1) $2 }
         | file                                          { File $1 }

Operation : WHERE Cond AND IfStmt                        { WhereSeq $2 $4 }
          | WHERE Cond                                   { Where $2 }
          | WHERE IfStmt                                 { WhereIf $2 }

Cond : OperandEval '>' OperandEval                       { Gt $1 $3 }
     | OperandEval '>' '=' OperandEval                   { Gte $1 $4 }
     | OperandEval '<' OperandEval                       { Lt $1 $3 }
     | OperandEval '<' '=' OperandEval                   { Lte $1 $4 }
     | Operand '=' Operand                               { Eql $1 $3 }
     | Cond AND Cond                                     { And $1 $3 }
     | Cond OR Cond                                      { Or $1 $3 }
	
IfStmt : IF '(' Cond ')' THEN Action ELSE Action END     { IfElse $3 $6 $8 }
       | IF '(' Cond ')' THEN Action END                 { If $3 $6 }
       | IfStmt AND IfStmt                               { SeqIfs $1 $3 }

	
NestedIf : IF '(' Cond ')' THEN Action ELSE Action END   { NIfElse $3 $6 $8 }
         | IF '(' Cond ')' THEN Action END               { NIf $3 $6 }
             
Action : CHANGE OperandAct OperandAct OperandAct         { Change $2 $3 $4 }
       | INSERT OperandAct OperandAct OperandAct         { Insert $2 $3 $4 }
       | DROP                                            { Drop }
       | NestedIf                                        { NestIf $1}
       | Action AND Action                               { SeqActs $1 $3 }

Operand : file '.' SUBJ                                  { Subject $1 }
        | file '.' PRED                                  { Predicate $1 }
	| string                                         { String $1 }
	| bool                                           { Bool $1 }
	| OperandEval                                    { Eval $1 }


OperandEval : file '.' OBJ                               { Object $1 }
            | int                                        { Int $1 }
            | OperandEval '+' OperandEval                { Add $1 $3 }
            | OperandEval '-' OperandEval                { Subtract $1 $3 }
	    | '(' '-' int ')'                            { Negate $3 }			
			
OperandAct : SUBJ                                        { Subj }
           | PRED                                        { Pred }
	   | OBJ                                         { Obj }
	   | Operand                                     { Oper $1 }
{ 

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t@(TokenRead {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token READ \n\n")
parseError (t@(TokenWhere {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token WHERE\n\n")
parseError (t@(TokenSubj {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token SUBJ \n\n")
parseError (t@(TokenPred {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token PRED\n\n")
parseError (t@(TokenObj {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token OBJ\n\n")
parseError (t@(TokenAnd {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token AND\n\n")
parseError (t@(TokenOr {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token OR\n\n")
parseError (t@(TokenIf {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token IF\n\n")
parseError (t@(TokenThen {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token THEN\n\n")
parseError (t@(TokenElse {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token ELSE\n\n")
parseError (t@(TokenEnd {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token END\n\n")
parseError (t@(TokenDrop {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token DROP\n\n")
parseError (t@(TokenChange {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token CHANGE\n\n")
parseError (t@(TokenInsert {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token INSERT\n\n")
parseError (t@(TokenDot {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced dot '.'\n\n")
parseError (t@(TokenLBrack {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced bracket '<'\n\n")
parseError (t@(TokenRBrack {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced bracket '>'\n\n")
parseError (t@(TokenEq {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token '='\n\n")
parseError (t@(TokenLParen {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced left parenthesis '('\n\n")
parseError (t@(TokenRParen {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced right parenthesis ')'\n\n")
parseError (t@(TokenPlus {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token '+'\n\n")
parseError (t@(TokenMinus {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced token '-'\n\n")
parseError (t@(TokenFile {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced files\n\n")
parseError (t@(TokenString {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced string\n\n")
parseError (t@(TokenBool {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced bool\n\n")
parseError (t@(TokenInt {}):ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " -> Misplaced int\n\n")


data Exp = ReadOp FileList Operation
         | Read FileList
         | SeqExps Exp Exp
         deriving Show

data FileList = Files FileList FileList
              | File String
              deriving Show

data Operation = Where Cond
               | WhereIf IfStmt
               | WhereSeq Cond IfStmt
               deriving Show

data Cond = Gt OperandEval OperandEval
          | Gte OperandEval OperandEval
          | Lt OperandEval OperandEval
          | Lte OperandEval OperandEval
          | Eql Operand Operand
          | And Cond Cond
          | Or Cond Cond
          deriving Show

data IfStmt = If Cond Action
            | IfElse Cond Action Action
            | SeqIfs IfStmt IfStmt
            deriving Show
			
data NestedIf = NIf Cond Action
              | NIfElse Cond Action Action
              deriving Show

data Action = Change OperandAct OperandAct OperandAct
            | Insert OperandAct OperandAct OperandAct
            | Drop
	    | NestIf NestedIf
            | SeqActs Action Action
            deriving Show

data Operand = Subject String
             | Predicate String
             | String String
             | Bool Bool
             | Eval OperandEval
             deriving Show

data OperandEval = Object String
                 | Int Int
		 | Negate Int
		 | Add OperandEval OperandEval
		 | Subtract OperandEval OperandEval
		 deriving Show

data OperandAct = Subj
                | Pred
		| Obj
		| Oper Operand
		deriving Show

} 

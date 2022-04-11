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
             
Action : CHANGE Operand Operand Operand                  { Change $2 $3 $4 }
       | INSERT Operand Operand Operand                  { Insert $2 $3 $4 }
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
			
{ 

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

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

data Action = Change Operand Operand Operand
            | Insert Operand Operand Operand
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

} 

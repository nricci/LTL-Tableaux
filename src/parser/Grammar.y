{
module Parser where
import Lexer
import LTL

}

%name parseSpecFromTokenStream
%tokentype { Token }
%error { parseError }

%token
    'true' { TokenTrue }
    'false' { TokenFalse }
    '!' { TokenNot }
    '&&' { TokenAnd }
    '||' { TokenOr }
    '->' { TokenImplies }
    '==' { TokenIff }
    '!=' { TokenXor }
    'X' { TokenNext }
    'F' { TokenFuture }
    'G' { TokenGlobally }
    'U' { TokenUntil }
    'W' { TokenWUntil }
    '(' { TokenLParen }
    ')' { TokenRParen }
    ';' { TokenSemi }
    'var' { TokenVar $$ }

%left ';'
%left 'U' 'W'
%left '->' '==' '!='
%left '&&' '||'
%left 'G' 'F' 'X'
%left '!'

%%


Formulae    :   Formula ';' Formulae    { $1 : $3 }
            |    Formula ';'             { [$1] }
            |   Formula                 { [$1] }


Formula : Formula '&&' Formula { And $1 $3 }
        | Formula '||' Formula { Or $1 $3 }
        | Formula '->' Formula { If $1 $3 }
        | Formula '==' Formula { Iff $1 $3 }
        | Formula '!=' Formula { Xor $1 $3 }
        | '!' Formula { Not $2}
        | '(' Formula ')' { $2 }

        |  Formula 'U' Formula { U $1 $3 }
        |  Formula 'W' Formula { W $1 $3 }
        |  'X' Formula { X $2 }
        |  'G' Formula { G $2 }
        |  'F' Formula { F $2 }

        |   'var'   { Prop $1 } 
        |   'true'  { TrueConst }
        |   'false' { FalseConst }            

{

parseSpecification :: String -> [Formula]
parseSpecification s = parseSpecFromTokenStream $ scanTokens s

parseError :: [Token] -> a
parseError ts = error ("Parse error" ++ show ts)

}

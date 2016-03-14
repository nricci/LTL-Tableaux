{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]


tokens :-

  $white+                       ;
  "--".*                        ;

  "False"                             { \s -> TokenFalse }
  "True"                             { \s -> TokenTrue }

  "&&"                             { \s -> TokenAnd }
  "||"                             { \s -> TokenOr }
  "->"                             { \s -> TokenImplies }
  "=="                              { \s -> TokenIff }
  "!="                             { \s -> TokenXor }
  "!"                              { \s -> TokenNot }


  "X"                            { \s -> TokenNext }
  "F"                           { \s -> TokenFuture }
  "G"                            { \s -> TokenGlobally }
  "U"                            { \s -> TokenUntil }
  "W"                            { \s -> TokenWUntil }

  "("                            { \s -> TokenLParen }
  ")"                            { \s -> TokenRParen }

  ";"                           { \s -> TokenSemi }

  $alpha [$alpha $digit \_ \']* { \s -> TokenVar s }

{

-- The token type:
data Token = TokenFalse
  | TokenTrue
  | TokenNot
  | TokenAnd
  | TokenOr
  | TokenImplies
  | TokenIff
  | TokenXor
  | TokenNext
  | TokenFuture
  | TokenGlobally
  | TokenUntil
  | TokenWUntil
  | TokenLParen
  | TokenRParen
  | TokenSemi
  | TokenVar String
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

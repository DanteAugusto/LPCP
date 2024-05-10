{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$upperAlpha = [A-Z]  -- alphabetic characters
$lowerAlpha = [a-z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "--".*.                         ;
  
  typeDeclarations                { \p s -> TypeDeclarations p }
  endTypeDeclarations             { \p s -> EndTypeDeclarations p }

  subprograms                     { \p s -> Subprograms p }
  endSubprograms                  { \p s -> EndSubprograms p }

  program                         { \p s -> Program p }
  end                             { \p s -> End p }

  fun                             { \p s -> Fun p }
  endFun                          { \p s -> EndFun p }
  return                          { \p s -> Return p }

  ";"                             { \p s -> Semicolon p }
  "->"                            { \p s -> Arrow p }
  "+"                             { \p s -> Plus p }
  "="                             { \p s -> Assign p }

  int                             { \p s -> Int p }
  
  $digit+	                            { \p s -> IntLit p (read s) }
  $lowerAlpha [$alpha $digit \_]*	    { \p s -> Id p s }
  $upperAlpha [$alpha $digit \_]*	    { \p s -> TypeId p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeDeclarations     AlexPosn        |
  EndTypeDeclarations  AlexPosn        |
  Subprograms          AlexPosn        |
  EndSubprograms       AlexPosn        |
  Program              AlexPosn        |
  End                  AlexPosn        |
  Fun                  AlexPosn        |
  EndFun               AlexPosn        |
  Return               AlexPosn        |
  Semicolon            AlexPosn        |
  Arrow                AlexPosn        |
  Plus                 AlexPosn        |
  Assign               AlexPosn        |
  Int                  AlexPosn        |
  Sym                  AlexPosn Char   |
  Var                  AlexPosn String |
  Int                  AlexPosn Int
  deriving (Eq,Show)

token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}
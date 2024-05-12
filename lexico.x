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
  type                            { \p s -> Type p }
  endType                         { \p s -> EndType p }

  globalVariables                 { \p s -> GlobalVariables p }
  endGlobalVariables              { \p s -> EndGlobalVariables p }

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
  "("                             { \p s -> OpenParent p }
  ")"                             { \p s -> CloseParent p }
  ","                             { \p s -> Comma p }

  int                             { \p s -> Int p }
  double                          { \p s -> Double p }
  
  $digit+	                            { \p s -> IntLit p (read s) }
  $digit+\.$digit+	                  { \p s -> DoubleLit p (read s) }
  $lowerAlpha [$alpha $digit \_]*	    { \p s -> Id p s }
  $upperAlpha [$alpha $digit \_]*	    { \p s -> TypeId p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeDeclarations     AlexPosn        |
  EndTypeDeclarations  AlexPosn        |
  Type                 AlexPosn        |                 
  EndType              AlexPosn        |
  GlobalVariables      AlexPosn        |
  EndGlobalVariables   AlexPosn        |
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
  OpenParent           AlexPosn        |
  CloseParent          AlexPosn        |
  Comma                AlexPosn        |
  Int                  AlexPosn        |
  Double               AlexPosn        |
  IntLit               AlexPosn Int    |
  DoubleLit            AlexPosn Double |
  Id                   AlexPosn String |
  TypeId               AlexPosn String 
  deriving (Eq,Show)

token_posn (Double p) = p
-- token_posn (In p) = p
-- token_posn (Sym p _) = p
-- token_posn (Var p _) = p
-- token_posn (Int p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}
{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$upperAlpha = [A-Z]  -- alphabetic characters
$lowerAlpha = [a-z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "--".*.                         ;
  
  typeDeclarations                { \p s -> TypeDeclarations (getLC p) }
  endTypeDeclarations             { \p s -> EndTypeDeclarations (getLC p) }
  type                            { \p s -> Type (getLC p) }
  endType                         { \p s -> EndType (getLC p) }

  globalVariables                 { \p s -> GlobalVariables (getLC p) }
  endGlobalVariables              { \p s -> EndGlobalVariables (getLC p) }

  subprograms                     { \p s -> Subprograms (getLC p) }
  endSubprograms                  { \p s -> EndSubprograms (getLC p) }

  program                         { \p s -> Program (getLC p) }
  end                             { \p s -> End (getLC p) }

  fun                             { \p s -> Fun (getLC p) }
  endFun                          { \p s -> EndFun (getLC p) }
  return                          { \p s -> Return (getLC p) }

  ";"                             { \p s -> Semicolon (getLC p) }
  "->"                            { \p s -> Arrow (getLC p) }
  "+"                             { \p s -> Plus (getLC p) }
  "**"                            { \p s -> Expo (getLC p) }
  "="                             { \p s -> Assign (getLC p) }
  "("                             { \p s -> OpenParent (getLC p) }
  ")"                             { \p s -> CloseParent (getLC p) }
  ","                             { \p s -> Comma (getLC p) }

  int                             { \p s -> Int (getLC p) }
  double                          { \p s -> Double (getLC p) }
  
  $digit+	                            { \p s -> IntLit (read s) (getLC p) }
  $digit+\.$digit+	                  { \p s -> DoubleLit (read s) (getLC p) }
  $lowerAlpha [$alpha $digit \_]*	    { \p s -> Id s (getLC p)}
  $upperAlpha [$alpha $digit \_]*	    { \p s -> TypeId s (getLC p)}

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeDeclarations     (Int, Int)        |
  EndTypeDeclarations  (Int, Int)        |
  Type                 (Int, Int)        |                 
  EndType              (Int, Int)        |
  GlobalVariables      (Int, Int)        |
  EndGlobalVariables   (Int, Int)        |
  Subprograms          (Int, Int)        |
  EndSubprograms       (Int, Int)        |
  Program              (Int, Int)        |
  End                  (Int, Int)        |
  Fun                  (Int, Int)        |
  EndFun               (Int, Int)        |
  Return               (Int, Int)        |
  Semicolon            (Int, Int)        |
  Arrow                (Int, Int)        |
  Plus                 (Int, Int)        |
  Expo                 (Int, Int)        |
  Assign               (Int, Int)        |
  OpenParent           (Int, Int)        |
  CloseParent          (Int, Int)        |
  Comma                (Int, Int)        |
  Int                  (Int, Int)        |
  Double               (Int, Int)        |
  IntLit               Int (Int, Int)    |
  DoubleLit            Double (Int, Int) |
  Id                   String (Int, Int) |
  TypeId               String (Int, Int)  
  deriving (Eq,Show)

-- token_posn (Double p) = p
-- token_posn (In p) = p
-- token_posn (Sym p _) = p
-- token_posn (Var p _) = p
-- token_posn (IntLit p _) = p
-- token_posn (Int p) = p

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}

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
  "-"                             { \p s -> Minus (getLC p) }
  "*"                             { \p s -> Mult (getLC p) }
  "/"                             { \p s -> Divi (getLC p) }
  "%"                             { \p s -> Mod (getLC p) }
  "**"                            { \p s -> Expo (getLC p) }
  ">="                            { \p s -> GreatEq (getLC p) }
  "<="                            { \p s -> LessEq (getLC p) }
  "<"                             { \p s -> Lesser (getLC p) }
  ">"                             { \p s -> Greater (getLC p) }
  "&&"                            { \p s -> And (getLC p) }
  "||"                            { \p s -> Or (getLC p) }
  "!="                            { \p s -> Diff (getLC p) }
  "=="                            { \p s -> Eq (getLC p) }
  "!"                             { \p s -> Neg (getLC p) }
  "="                             { \p s -> Assign (getLC p) }
  "("                             { \p s -> OpenParent (getLC p) }
  ")"                             { \p s -> CloseParent (getLC p) }
  ","                             { \p s -> Comma (getLC p) }

  int                             { \p s -> Int (getLC p) }
  double                          { \p s -> Double (getLC p) }
  -- float                           { \p s -> Float (getLC p) }
  bool                            { \p s -> Bool (getLC p) }

  cast                            { \p s -> Cast (getLC p) }
  puts                            { \p s -> Puts (getLC p) }

  $digit+	                            { \p s -> IntLit (read s) (getLC p) }
  $digit+\.$digit+	                  { \p s -> DoubleLit (read s) (getLC p) }
  ("True"|"False")                    { \p s -> BoolLit (read s) (getLC p) }
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
  Mult                 (Int, Int)        |
  Divi                 (Int, Int)        |
  Mod                  (Int, Int)        |
  Lesser               (Int, Int)        |
  Greater              (Int, Int)        |
  LessEq               (Int, Int)        |
  GreatEq              (Int, Int)        |
  And                  (Int, Int)        |
  Or                   (Int, Int)        |
  Eq                   (Int, Int)        |
  Diff                 (Int, Int)        |
  Neg                  (Int, Int)        |
  Minus                (Int, Int)        |
  Expo                 (Int, Int)        |
  Assign               (Int, Int)        |
  OpenParent           (Int, Int)        |
  CloseParent          (Int, Int)        |
  Comma                (Int, Int)        |
  Int                  (Int, Int)        |
  Double               (Int, Int)        |
  Bool                 (Int, Int)        |
  Cast                 (Int, Int)        |
  Puts                 (Int, Int)        |
  IntLit               Int (Int, Int)    |
  DoubleLit            Double (Int, Int) |
  BoolLit              Bool   (Int, Int) |
  Id                   String (Int, Int) |
  TypeId               String (Int, Int)  
  deriving (Eq, Show)

-- instance Show Token where
--   show (DoubleLit v p) = show v
--   show (IntLit v p) = show v
--   show _ = ""

-- "<"                             { \p s -> Lesser (getLC p) }
--   ">"                             { \p s -> Greater (getLC p) }
--   ">="                            { \p s -> LessEq (getLC p) }
--   "<="                            { \p s -> GreatEq (getLC p) }
--   "&&"                            { \p s -> And (getLC p) }
--   "||"                            { \p s -> Or (getLC p) }
--   "!="                            { \p s -> Diff (getLC p) }
--   "=="                            { \p s -> Eq (getLC p) }
  
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

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
  register                        { \p s -> Register (getLC p) }
  endRegister                     { \p s -> EndRegister (getLC p) }

  globalVariables                 { \p s -> GlobalVariables (getLC p) }
  endGlobalVariables              { \p s -> EndGlobalVariables (getLC p) }

  subprograms                     { \p s -> Subprograms (getLC p) }
  endSubprograms                  { \p s -> EndSubprograms (getLC p) }

  program                         { \p s -> Program (getLC p) }
  end                             { \p s -> End (getLC p) }

  fun                             { \p s -> Fun (getLC p) }
  endFun                          { \p s -> EndFun (getLC p) }
  return                          { \p s -> Return (getLC p) }
  
  proc                             { \p s -> Proc (getLC p) }
  endProc                          { \p s -> EndProc (getLC p) }
  exitproc                         { \p s -> ExitProc (getLC p) }

  while                           { \p s -> While (getLC p) }
  endWhile                        { \p s -> EndWhile (getLC p) }
  continue                        { \p s -> Continue (getLC p) }
  break                           { \p s -> Break (getLC p) }

  if                              { \p s -> If (getLC p) }
  else                            { \p s -> Else (getLC p) }
  endIf                           { \p s -> EndIf (getLC p) }

  ";"                             { \p s -> Semicolon (getLC p) }
  "->"                            { \p s -> Arrow (getLC p) }
  "<+>"                           { \p s -> PlusMatrix (getLC p) }
  "<*>"                           { \p s -> MultMatrix (getLC p) } 
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
  "["                             { \p s -> OpenBrack (getLC p) }
  "]"                             { \p s -> CloseBrack (getLC p) }
  ","                             { \p s -> Comma (getLC p) }

  int                             { \p s -> Int (getLC p) }
  double                          { \p s -> Double (getLC p) }
  -- float                           { \p s -> Float (getLC p) }
  bool                            { \p s -> Bool (getLC p) }
  string                          { \p s -> Str (getLC p) }
  matrix                          { \p s -> Matrix (getLC p) }
  ref                             { \p s -> Ref (getLC p) }

  cast                            { \p s -> Cast (getLC p) }
  Default                         { \p s -> Default (getLC p) }
  puts                            { \p s -> Puts (getLC p) }
  stup                            { \p s -> Stup (getLC p) }

  $digit+	                            { \p s -> IntLit (read s) (getLC p) }
  $digit+\.$digit+	                  { \p s -> DoubleLit (read s) (getLC p) }
  ("True"|"False")                    { \p s -> BoolLit (read s) (getLC p) }
  \" [$printable]* \"     { \p s -> StringLit (read s) (getLC p) }
  $lowerAlpha [$alpha $digit \_]*	    { \p s -> Id s (getLC p)}
  $upperAlpha [$alpha $digit \_]*	    { \p s -> TypeId s (getLC p)}

  .                                   { \p s -> Unknown (getLC p) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeDeclarations     (Int, Int)        |
  EndTypeDeclarations  (Int, Int)        |
  Register             (Int, Int)        |                 
  EndRegister          (Int, Int)        |
  GlobalVariables      (Int, Int)        |
  EndGlobalVariables   (Int, Int)        |
  Subprograms          (Int, Int)        |
  EndSubprograms       (Int, Int)        |
  Program              (Int, Int)        |
  End                  (Int, Int)        |
  Fun                  (Int, Int)        |
  EndFun               (Int, Int)        |
  Return               (Int, Int)        |
  Proc                 (Int, Int)        |
  EndProc              (Int, Int)        |
  ExitProc             (Int, Int)        |
  While                (Int, Int)        |
  EndWhile             (Int, Int)        |
  Continue             (Int, Int)        |
  Break                (Int, Int)        |
  If                   (Int, Int)        |
  Else                 (Int, Int)        |
  EndIf                (Int, Int)        |
  Semicolon            (Int, Int)        |
  Arrow                (Int, Int)        |
  PlusMatrix           (Int, Int)        |
  MultMatrix           (Int, Int)        |
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
  OpenBrack            (Int, Int)        |
  CloseBrack           (Int, Int)        |
  Comma                (Int, Int)        |
  Int                  (Int, Int)        |
  Matrix               (Int, Int)        |
  IntMatrix            (Int, Int)        |
  DoubleMatrix         (Int, Int)        |
  Double               (Int, Int)        |
  Bool                 (Int, Int)        |
  Str                  (Int, Int)        |
  Ref                  (Int, Int)        |
  Cast                 (Int, Int)        |
  Default              (Int, Int)        |
  Puts                 (Int, Int)        |
  Stup                 (Int, Int)        |
  IntLit               Int (Int, Int)    |
  DoubleLit            Double (Int, Int) |
  BoolLit              Bool   (Int, Int) |
  StringLit            String (Int, Int) |
  Id                   String (Int, Int) |
  TypeId               String (Int, Int) |
  Unknown               (Int, Int)
  deriving (Eq)

instance Show Token where
  show (DoubleLit v p) = "Double"
  show (IntLit v p) = "Int"
  show (BoolLit v p) = "Bool"
  show (TypeDeclarations p) = "TypeDeclarations"
  show (EndTypeDeclarations p) = "EndTypeDeclarations"
  show (Register p) = "Register"
  show (EndRegister p) = "EndRegister"
  show (GlobalVariables p) = "GlobalVariables"
  show (EndGlobalVariables p) = "EndGlobalVariables"
  show (Subprograms p) = "Subprograms"
  show (EndSubprograms p) = "EndSubprograms"
  show (Program p) = "Program"
  show (End p) = "End"
  show (Fun p) = "Fun"
  show (EndFun p) = "EndFun"
  show (Return p) = "Return"
  show (Proc p) = "Proc"
  show (EndProc p) = "EndProc"
  show (ExitProc p) = "ExitProc"
  show (While p) = "While"
  show (EndWhile p) = "EndWhile"
  show (Continue p) = "Continue"
  show (Break p) = "Break"
  show (Semicolon p) = "Semicolon"
  show (Arrow p) = "Arrow"
  show (PlusMatrix p) = "PlusMatrix"
  show (MultMatrix p) = "MultMatrix"
  show (Plus p) = "Plus"
  show (Mult p) = "Mult"
  show (Divi p) = "Divi"
  show (Mod p) = "Mod"
  show (Lesser p) = "Lesser"
  show (Greater p) = "Greater"
  show (LessEq p) = "LessEq"
  show (GreatEq p) = "GreatEq"
  show (And p) = "And"
  show (Or p) = "Or"
  show (Eq p) = "Eq"
  show (Diff p) = "Diff"
  show (Neg p) = "Neg"
  show (Minus p) = "Minus"
  show (Expo p) = "Expo"
  show (Assign p) = "Assign"
  show (OpenParent p) = "OpenParent"
  show (CloseParent p) = "CloseParent"
  show (OpenBrack p) = "OpenBrack"
  show (CloseBrack p) = "CloseBrack"
  show (Comma p) = "Comma"
  show (Int p) = "Int"
  show (Matrix p) = "Matrix"
  show (IntMatrix p) = "MatrixInt"
  show (DoubleMatrix p) = "MatrixDouble"
  show (Double p) = "Double"
  show (Bool p) = "Bool"
  show (Str p) = "String"
  show (Ref p) = "Ref"
  show (Cast p) = "Cast"
  show (Default p) = "Default"
  show (Puts p) = "Puts"
  show (Stup p) = "Stup"
  show (StringLit v p) = "String"
  show (Id v p) = show v
  show (TypeId v p) = show v
  show (If p) = "If"
  show (Else p) = "Else"
  show (EndIf p) = "EndIf"
  show (Unknown p) = "Unknown Token"

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

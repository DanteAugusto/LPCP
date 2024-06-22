{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Tokens where

import Lexer
import Text.Parsec

arrowToken :: ParsecT [Token] st IO (Token)
arrowToken = tokenPrim show update_pos get_token where
  get_token (Arrow p) = Just (Arrow p)
  get_token _         = Nothing

programToken :: ParsecT [Token] st IO (Token)
programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _           = Nothing

typeDeclarationsToken :: ParsecT [Token] st IO (Token)
typeDeclarationsToken = tokenPrim show update_pos get_token where
  get_token (TypeDeclarations p) = Just (TypeDeclarations p)
  get_token _           = Nothing

endTypeDeclarationsToken :: ParsecT [Token] st IO (Token)
endTypeDeclarationsToken = tokenPrim show update_pos get_token where
  get_token (EndTypeDeclarations p) = Just (EndTypeDeclarations p)
  get_token _           = Nothing

globalVariablesToken :: ParsecT [Token] st IO (Token)
globalVariablesToken = tokenPrim show update_pos get_token where
  get_token (GlobalVariables p) = Just (GlobalVariables p)
  get_token _           = Nothing

endGlobalVariablesToken :: ParsecT [Token] st IO (Token)
endGlobalVariablesToken = tokenPrim show update_pos get_token where
  get_token (EndGlobalVariables p) = Just (EndGlobalVariables p)
  get_token _           = Nothing

subprogramsToken :: ParsecT [Token] st IO (Token)
subprogramsToken = tokenPrim show update_pos get_token where
  get_token (Subprograms p) = Just (Subprograms p)
  get_token _           = Nothing

endSubprogramsToken :: ParsecT [Token] st IO (Token)
endSubprogramsToken = tokenPrim show update_pos get_token where
  get_token (EndSubprograms p) = Just (EndSubprograms p)
  get_token _           = Nothing

registerToken :: ParsecT [Token] st IO (Token)
registerToken = tokenPrim show update_pos get_token where
  get_token (Register p) = Just (Register p)
  get_token _           = Nothing

matrixToken :: ParsecT [Token] st IO (Token)
matrixToken = tokenPrim show update_pos get_token where
  get_token (Matrix p) = Just (Matrix p)
  get_token _           = Nothing

plusMatrixToken :: ParsecT [Token] st IO (Token)
plusMatrixToken = tokenPrim show update_pos get_token where
  get_token (PlusMatrix p) = Just (PlusMatrix p)
  get_token _           = Nothing 

multMatrixToken :: ParsecT [Token] st IO (Token)
multMatrixToken = tokenPrim show update_pos get_token where
  get_token (MultMatrix p) = Just (MultMatrix p)
  get_token _           = Nothing

endRegisterToken :: ParsecT [Token] st IO (Token)
endRegisterToken = tokenPrim show update_pos get_token where
  get_token (EndRegister p) = Just (EndRegister p)
  get_token _           = Nothing

endToken :: ParsecT [Token] st IO (Token)
endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _           = Nothing

openParentToken :: ParsecT [Token] st IO (Token)
openParentToken = tokenPrim show update_pos get_token where
  get_token (OpenParent p) = Just (OpenParent p)
  get_token _           = Nothing

closeParentToken :: ParsecT [Token] st IO (Token)
closeParentToken = tokenPrim show update_pos get_token where
  get_token (CloseParent p) = Just (CloseParent p)
  get_token _           = Nothing

openBrackToken :: ParsecT [Token] st IO (Token)
openBrackToken = tokenPrim show update_pos get_token where
  get_token (OpenBrack p) = Just (OpenBrack p)
  get_token _           = Nothing

closeBrackToken :: ParsecT [Token] st IO (Token)
closeBrackToken = tokenPrim show update_pos get_token where
  get_token (CloseBrack p) = Just (CloseBrack p)
  get_token _           = Nothing

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _        = Nothing

typeIdToken :: ParsecT [Token] st IO (Token)
typeIdToken = tokenPrim show update_pos get_token where
  get_token (TypeId x p) = Just (TypeId x p)
  get_token _        = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon p) = Just (Semicolon p)
  get_token _             = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _             = Nothing

assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _          = Nothing

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int p) = Just (Int p)
  get_token _         = Nothing

doubleToken :: ParsecT [Token] st IO (Token)
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double p) = Just (Double p)
  get_token _         = Nothing

boolToken :: ParsecT [Token] st IO (Token)
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p) = Just (Bool p)
  get_token _         = Nothing

stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (Str p) = Just (Str p)
  get_token _         = Nothing

castToken :: ParsecT [Token] st IO (Token)
castToken = tokenPrim show update_pos get_token where
  get_token (Cast p) = Just (Cast p)
  get_token _         = Nothing

defaultToken :: ParsecT [Token] st IO (Token)
defaultToken = tokenPrim show update_pos get_token where
  get_token (Default p) = Just (Default p)
  get_token _         = Nothing

putsToken :: ParsecT [Token] st IO (Token)
putsToken = tokenPrim show update_pos get_token where
  get_token (Puts p) = Just (Puts p)
  get_token _         = Nothing

stupToken :: ParsecT [Token] st IO (Token)
stupToken = tokenPrim show update_pos get_token where
  get_token (Stup p) = Just (Stup p)
  get_token _         = Nothing

whileToken :: ParsecT [Token] st IO (Token)
whileToken = tokenPrim show update_pos get_token where
  get_token (While p) = Just (While p)
  get_token _         = Nothing

endWhileToken :: ParsecT [Token] st IO (Token)
endWhileToken = tokenPrim show update_pos get_token where
  get_token (EndWhile p) = Just (EndWhile p)
  get_token _         = Nothing

continueToken :: ParsecT [Token] st IO (Token)
continueToken = tokenPrim show update_pos get_token where
  get_token (Continue p) = Just (Continue p)
  get_token _         = Nothing

breakToken :: ParsecT [Token] st IO (Token)
breakToken = tokenPrim show update_pos get_token where
  get_token (Break p) = Just (Break p)
  get_token _         = Nothing

minusToken :: ParsecT [Token] st IO (Token)
minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p) = Just (Minus p)
  get_token _       = Nothing

diviToken :: ParsecT [Token] st IO (Token)
diviToken = tokenPrim show update_pos get_token where
  get_token (Divi p) = Just (Divi p)
  get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (Token)
plusToken = tokenPrim show update_pos get_token where
  get_token (Plus p) = Just (Plus p)
  get_token _       = Nothing 

multToken :: ParsecT [Token] st IO (Token)
multToken = tokenPrim show update_pos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _       = Nothing 

modToken :: ParsecT [Token] st IO (Token)
modToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _       = Nothing 

expoToken :: ParsecT [Token] st IO (Token)
expoToken = tokenPrim show update_pos get_token where
  get_token (Expo p) = Just (Expo p)
  get_token _       = Nothing 

andToken :: ParsecT [Token] st IO (Token)
andToken = tokenPrim show update_pos get_token where
  get_token (And p) = Just (And p)
  get_token _       = Nothing

orToken :: ParsecT [Token] st IO (Token)
orToken = tokenPrim show update_pos get_token where
  get_token (Or p) = Just (Or p)
  get_token _       = Nothing 

lessToken :: ParsecT [Token] st IO (Token)
lessToken = tokenPrim show update_pos get_token where
  get_token (Lesser p) = Just (Lesser p)
  get_token _       = Nothing 

greatToken :: ParsecT [Token] st IO (Token)
greatToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _       = Nothing 

leqToken :: ParsecT [Token] st IO (Token)
leqToken = tokenPrim show update_pos get_token where
  get_token (LessEq p) = Just (LessEq p)
  get_token _       = Nothing 

geqToken :: ParsecT [Token] st IO (Token)
geqToken = tokenPrim show update_pos get_token where
  get_token (GreatEq p) = Just (GreatEq p)
  get_token _       = Nothing 

eqToken :: ParsecT [Token] st IO (Token)
eqToken = tokenPrim show update_pos get_token where
  get_token (Eq p) = Just (Eq p)
  get_token _       = Nothing 

diffToken :: ParsecT [Token] st IO (Token)
diffToken = tokenPrim show update_pos get_token where
  get_token (Diff p) = Just (Diff p)
  get_token _       = Nothing 

negToken :: ParsecT [Token] st IO (Token)
negToken = tokenPrim show update_pos get_token where
  get_token (Neg p) = Just (Neg p)
  get_token _       = Nothing 


intLitToken :: ParsecT [Token] st IO (Token)
intLitToken = tokenPrim show update_pos get_token where
  get_token (IntLit x p) = Just (IntLit x p)
  get_token _      = Nothing

doubleLitToken :: ParsecT [Token] st IO (Token)
doubleLitToken = tokenPrim show update_pos get_token where
  get_token (DoubleLit x p) = Just (DoubleLit x p)
  get_token _      = Nothing

boolLitToken :: ParsecT [Token] st IO (Token)
boolLitToken = tokenPrim show update_pos get_token where
  get_token (BoolLit x p) = Just (BoolLit x p)
  get_token _      = Nothing

stringLitToken :: ParsecT [Token] st IO (Token)
stringLitToken = tokenPrim show update_pos get_token where
  get_token (StringLit x p) = Just (StringLit x p)
  get_token _      = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  
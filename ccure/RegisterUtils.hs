{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RegisterUtils where

import Lexer
import Text.Parsec
import State
import Tokens (intLitToken)
import Data.List

-- type UserTypes = [(Token, [(Token, Type)])]

-- userTypesInsert :: Token  -> CCureState -> CCureState
-- userTypeInsert a

insertUserType :: Token -> CCureState -> CCureState
insertUserType (Id id p) (a, b, c, d, e, f) = (a, b, c, d, (Id id p, []):e, f)

addAttrToUserTypes :: Token -> (Token, Type) -> CCureState -> CCureState
addAttrToUserTypes id a (b, c, d, e, f, g) = (b, c, d, e, addAttrToUserTypesAux id a f, g)

addAttrToUserTypesAux :: Token -> (Token, Type) -> UserTypes -> UserTypes
addAttrToUserTypesAux _ _ [] = fail "user type not found"
addAttrToUserTypesAux (Id idType pType) (Id idAtt pAtt, vAtt) ( (Id id1 p1, ( Id id2 p2, v2):tail ):t ) =
    if idType == id1 then (Id id1 p1, (Id idAtt pAtt, vAtt):(Id id2 p2, v2):tail ):t
    else (Id id1 p1, (Id id2 p2, v2):tail ):addAttrToUserTypesAux (Id idType pType) (Id idAtt pAtt, vAtt) t
addAttrToUserTypesAux (Id idType pType) (Id idAtt pAtt, vAtt) ( (Id id1 p1, [] ):t ) =
    if idType == id1 then (Id id1 p1, [(Id idAtt pAtt, vAtt)] ):t
    else (Id id1 p1, [] ):addAttrToUserTypesAux (Id idType pType) (Id idAtt pAtt, vAtt) t
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
insertUserType (TypeId id p) (a, b, c, d, e, f) = (a, b, c, d, (TypeId id p, []):e, f)

addAttrToUserTypes :: Token -> (Token, Type) -> CCureState -> CCureState
addAttrToUserTypes id a (b, c, d, e, f, g) = (b, c, d, e, addAttrToUserTypesAux id a f, g)

addAttrToUserTypesAux :: Token -> (Token, Type) -> UserTypes -> UserTypes
addAttrToUserTypesAux _ _ [] = fail "user type not found"
addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) ( (TypeId id1 p1, ( Id id2 p2, v2):tail ):t ) =
    if idType == id1 then (TypeId id1 p1, (Id idAtt pAtt, vAtt):(Id id2 p2, v2):tail ):t
    else (TypeId id1 p1, (Id id2 p2, v2):tail ):addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) t
addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) ( (TypeId id1 p1, [] ):t ) =
    if idType == id1 then (TypeId id1 p1, [(Id idAtt pAtt, vAtt)] ):t
    else (TypeId id1 p1, [] ):addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) t

getUserType :: Token -> CCureState -> Type
getUserType (TypeId id p) (_, _, _, _, e, _) = getUserTypeAux (TypeId id p) e

getUserTypeAux :: Token -> UserTypes -> Type
getUserTypeAux _ [] = error "user type not found"
getUserTypeAux (TypeId idType pType) ( (TypeId id1 p1, v1):t ) =
    if idType == id1 then RegisterType (TypeId id1 p1, v1)
    else getUserTypeAux (TypeId idType pType) t

isInUserTypes :: Token -> CCureState -> Bool
isInUserTypes (TypeId id p) (_, _, _, _, e, _) = isInUserTypesAux (TypeId id p) e

isInUserTypesAux :: Token -> UserTypes -> Bool
isInUserTypesAux _ [] = False
isInUserTypesAux (TypeId id p) ( (TypeId id1 p1, _):t ) = (id == id1) || isInUserTypesAux (TypeId id p) t

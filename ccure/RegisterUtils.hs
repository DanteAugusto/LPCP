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
insertUserType (TypeId id p) (a, b, c, d, e, f, g, h) = (a, b, c, d, (TypeId id p, []):e, f, g, h)

addAttrToUserTypes :: Token -> (Token, Type) -> CCureState -> CCureState
addAttrToUserTypes id a (b, c, d, e, f, g, h, i) = (b, c, d, e, addAttrToUserTypesAux id a f, g, h, i)

addAttrToUserTypesAux :: Token -> (Token, Type) -> UserTypes -> UserTypes
addAttrToUserTypesAux _ _ [] = fail "user type not found"
addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) ( (TypeId id1 p1, ( Id id2 p2, v2):tail ):t ) =
    if idType == id1 then (TypeId id1 p1, (Id idAtt pAtt, vAtt):(Id id2 p2, v2):tail ):t
    else (TypeId id1 p1, (Id id2 p2, v2):tail ):addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) t
addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) ( (TypeId id1 p1, [] ):t ) =
    if idType == id1 then (TypeId id1 p1, [(Id idAtt pAtt, vAtt)] ):t
    else (TypeId id1 p1, [] ):addAttrToUserTypesAux (TypeId idType pType) (Id idAtt pAtt, vAtt) t

getUserType :: Token -> CCureState -> Type
getUserType (TypeId id p) (_, _, _, _, e, _, _, _) = getUserTypeAux (TypeId id p) e

getUserTypeAux :: Token -> UserTypes -> Type
getUserTypeAux _ [] = error "user type not found"
getUserTypeAux (TypeId idType pType) ( (TypeId id1 p1, v1):t ) =
    if idType == id1 then RegisterType (TypeId id1 p1, v1)
    else getUserTypeAux (TypeId idType pType) t

isRegisterType :: Type -> Bool
isRegisterType (RegisterType _) = True
isRegisterType _ = False

isRegAttr :: Token -> Type -> Bool
isRegAttr (Id id p) (RegisterType (TypeId id1 p1, l)) = isRegAttrAux (Id id p) l

isRegAttrAux :: Token -> [(Token, Type)] -> Bool
isRegAttrAux _ [] = False
isRegAttrAux (Id id p) ( (Id id1 p1, _):t ) = (id == id1) || isRegAttrAux (Id id p) t

getRegAttr :: Token -> Type -> Type
getRegAttr (Id id p) (RegisterType (TypeId id1 p1, l)) = getRegAttrAux (Id id p) l

getRegAttrAux :: Token -> [(Token, Type)] -> Type
getRegAttrAux _ [] = error "attribute not found"
getRegAttrAux (Id id p) ( (Id id1 p1, v):t ) = if id == id1 then v else getRegAttrAux (Id id p) t

isInUserTypes :: Token -> CCureState -> Bool
isInUserTypes (TypeId id p) (_, _, _, _, e, _, _, _) = isInUserTypesAux (TypeId id p) e

isInUserTypesAux :: Token -> UserTypes -> Bool
isInUserTypesAux _ [] = False
isInUserTypesAux (TypeId id p) ( (TypeId id1 p1, _):t ) = (id == id1) || isInUserTypesAux (TypeId id p) t

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module State where

import Lexer
import Text.Parsec

data LoopStatus = OK | BREAK | CONTINUE deriving (Eq, Show)

data Type =
    NULL |
    DoubleType Double |
    IntType Int |
    BoolType Bool |
    StringType String |
    RegisterType (Token, [(Token, Type)]) | 
    PointerType (Type, (String, String)) |
    ArrayType (Int, [Type]) |
    MatrixInt (Int, Int, [[Int]]) |
    MatrixDouble (Int, Int, [[Double]])
    deriving (Eq)

instance Show Type where
    show (IntType x) = id show x
    show (DoubleType x) = show x
    show (BoolType True) = id "true"
    show (BoolType False) = id "false"
    show (StringType x) = id x
    show (RegisterType (TypeId id p, l)) = show blz where
        blz = "Register " ++ id ++ " { " ++ show l ++ " }"
    show (MatrixInt (l, c, m)) = show m
    show (MatrixDouble (l, c, m)) = show m
    show (NULL) = "Isso nao significa nada"


type ExAct = Bool
type DynamicDepth = Int
-- Nome, profundidade de chamada, valor e escopo
type SymTable = [(Token, String, [(Int, Type)])]

-- type ExecStack = [[]] pilha de instancias de ra (linha pra onde devo voltar e valor de retorno)
-- type ActRegSubprog = [(String, [String])]
type ScopeStack = [String]
type LoopStack = [LoopStatus]
type UserTypes = [(Token, [(Token, Type)])]

type CCureState = (SymTable, ScopeStack, LoopStack, DynamicDepth, UserTypes, ExAct)

-- funções auxiliares para o CCureState

addToLoopStack :: LoopStatus -> CCureState -> CCureState
addToLoopStack newLoopStatus (a, b,stack, c, d, e)  = (a, b, newLoopStatus:stack, c, d, e)

-- removeFromLoopStack :: CCureState -> CCureState
-- removeFromLoopStack (_, _, [], _) = error "trying to remove unexistent Loop"
-- removeFromLoopStack (a, b, stack:tail, c) = (a, b, tail, c)

removeFromLoopStack :: CCureState -> CCureState
removeFromLoopStack (_, _, [], _, _, _) = error "trying to remove unexistent Loop"
removeFromLoopStack (a, b, stack:tail, c , d, e) = (a, b, tail, c, d, e)

getCurrentLoopStatus :: CCureState -> LoopStatus
getCurrentLoopStatus (_, _, top:tail, _, _, _) = top
getCurrentLoopStatus _ = error "trying to access unexistent LoopStatus"



addToScopeStack :: String -> CCureState -> CCureState
-- addToScopeStack newScope (a, stack, b, c) = (a, newScope:stack, b, c)
addToScopeStack newScope (a, top:tail, b, c, d, e)  = (a, (newScope ++ ['#'] ++ top):top:tail, b, c, d, e)
addToScopeStack newScope (a, [], b, c, d, e)  = (a, [newScope], b, c, d, e)

getCurrentScope :: CCureState -> String
getCurrentScope (_, top:tail, _, _, _, _) = top
getCurrentScope _ = error "trying to access unexistent scope"

getTopScope :: CCureState -> String
getTopScope a = takeWhile (/= '#') (getCurrentScope a)

removeFromScopeStack :: CCureState -> CCureState
removeFromScopeStack (_, [], _, _, _, _) = error "trying to remove unexistent scope"
removeFromScopeStack (a, stack:tail, b, c, d, e) = (a, tail, b, c, d, e)

-- getCurrentScope :: CCureState -> String
-- getCurrentScope (_, [top]:tail, _) = getCurrentScopeAux(top)

-- getCurrentScopeAux :: String -> String
-- getCurrentScopeAux [] = ""
-- getCurrentScopeAux '#':tail = ""
-- getCurrentScopeAux top:tail = top : getCurrentScopeAux tail

getCurrentDepth :: CCureState -> Int
getCurrentDepth (_, _, _, a, _, _) = a

addDepth :: CCureState -> CCureState
addDepth (a, b, c, d, e, f) = (a, b, c, d + 1, e, f)

removeDepth :: CCureState -> CCureState
removeDepth (a, b, c, 0, e, f) = error "trying to remove base depth"
removeDepth (a, b, c, d, e, f) = (a, b, c, d - 1, e, f)


execOn :: CCureState -> Bool
execOn (_, _, _, _, _, True) = True
execOn (_, _, _, _, _, False) = False

turnExecOn :: CCureState -> CCureState
turnExecOn (a, b, c, d, e, _) = (a, b, c, d, e, True)

turnExecOff :: CCureState -> CCureState
turnExecOff (a, b, c, d, e, _) = (a, b, c, d, e, False)
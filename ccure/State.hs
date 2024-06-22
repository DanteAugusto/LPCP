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
    StructType (String, [(String, Type)]) | 
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
    show (MatrixInt (l, c, m)) = show m
    show (MatrixDouble (l, c, m)) = show m
    show (NULL) = "Isso nao significa nada"

type ExAct = Bool
-- Nome, profundidade de chamada, valor e escopo
type SymTable = [(Token, String, [(Int, Type)])]

-- type ExecStack = [[]] pilha de instancias de ra (linha pra onde devo voltar e valor de retorno)
-- type ActRegSubprog = [(String, [String])]
type ScopeStack = [String]
type LoopStack = [LoopStatus]

type CCureState = (SymTable, ScopeStack, LoopStack, ExAct)

-- funções auxiliares para o CCureState

addToLoopStack :: LoopStatus -> CCureState -> CCureState
addToLoopStack newLoopStatus (a, b,stack, c)  = (a, b, newLoopStatus:stack, c)

-- removeFromLoopStack :: CCureState -> CCureState
-- removeFromLoopStack (_, _, [], _) = error "trying to remove unexistent Loop"
-- removeFromLoopStack (a, b, stack:tail, c) = (a, b, tail, c)

removeFromLoopStack :: CCureState -> CCureState
removeFromLoopStack (_, _, [], _) = error "trying to remove unexistent Loop"
removeFromLoopStack (a, b, stack:tail, c) = (a, b, tail, c)

getCurrentLoopStatus :: CCureState -> LoopStatus
getCurrentLoopStatus (_, _, top:tail, _) = top
getCurrentLoopStatus _ = error "trying to access unexistent LoopStatus"



addToScopeStack :: String -> CCureState -> CCureState
-- addToScopeStack newScope (a, stack, b, c) = (a, newScope:stack, b, c)
addToScopeStack newScope (a, top:tail, b, c)  = (a, (newScope ++ ['#'] ++ top):top:tail, b, c)
addToScopeStack newScope (a, [], b, c)  = (a, [newScope], b, c)

getCurrentScope :: CCureState -> String
getCurrentScope (_, top:tail, _, _) = top
getCurrentScope _ = error "trying to access unexistent scope"

getTopScope :: CCureState -> String
getTopScope a = takeWhile (/= '#') (getCurrentScope a)

removeFromScopeStack :: CCureState -> CCureState
removeFromScopeStack (_, [], _, _) = error "trying to remove unexistent scope"
removeFromScopeStack (a, stack:tail, b, c) = (a, tail, b, c)

-- getCurrentScope :: CCureState -> String
-- getCurrentScope (_, [top]:tail, _) = getCurrentScopeAux(top)

-- getCurrentScopeAux :: String -> String
-- getCurrentScopeAux [] = ""
-- getCurrentScopeAux '#':tail = ""
-- getCurrentScopeAux top:tail = top : getCurrentScopeAux tail

execOn :: CCureState -> Bool
execOn (_, _, _, True) = True
execOn (_, _, _, False) = False

turnExecOn :: CCureState -> CCureState
turnExecOn (a, b, c, _) = (a, b, c, True)

turnExecOff :: CCureState -> CCureState
turnExecOff (a, b, c, _) = (a, b, c, False)
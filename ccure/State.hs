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

-- Lista de:
-- Nome do procedimento, pc, lista de parametros
type UserProcedures = [(Token, [Token], [(Token, Type)])]

-- Lista de:
-- Nome da funcao, pc, tipo de retorno, lista de parametros
type UserFunctions = [(Token, [Token], Type, [(Token, Type)])]

-- type StackWhereToReturn = [[Token]]

type CCureState = (SymTable, ScopeStack, LoopStack, DynamicDepth, UserTypes, UserProcedures, UserFunctions, ExAct)
-- type CCureState = (SymTable, ScopeStack, LoopStack, DynamicDepth, UserTypes, ExAct)

-- funções auxiliares para o CCureState

addToLoopStack :: LoopStatus -> CCureState -> CCureState
addToLoopStack newLoopStatus (a, b,stack, c, d, e, f, g)  = (a, b, newLoopStatus:stack, c, d, e, f, g)

-- removeFromLoopStack :: CCureState -> CCureState
-- removeFromLoopStack (_, _, [], _) = error "trying to remove unexistent Loop"
-- removeFromLoopStack (a, b, stack:tail, c) = (a, b, tail, c)

removeFromLoopStack :: CCureState -> CCureState
removeFromLoopStack (_, _, [], _, _, _, _, _) = error "trying to remove unexistent Loop"
removeFromLoopStack (a, b, stack:tail, c , d, e, f, g) = (a, b, tail, c, d, e, f, g)

getCurrentLoopStatus :: CCureState -> LoopStatus
getCurrentLoopStatus (_, _, top:tail, _, _, _, _ ,_) = top
getCurrentLoopStatus _ = error "trying to access unexistent LoopStatus"



addToScopeStack :: String -> CCureState -> CCureState
-- addToScopeStack newScope (a, stack, b, c) = (a, newScope:stack, b, c)
addToScopeStack newScope (a, top:tail, b, c, d, e, f, g)  = (a, (newScope ++ ['#'] ++ top):top:tail, b, c, d, e, f, g)
addToScopeStack newScope (a, [], b, c, d, e, f, g)  = (a, [newScope], b, c, d, e, f, g)

getCurrentScope :: CCureState -> String
getCurrentScope (_, top:tail, _, _, _, _, _, _) = top
getCurrentScope _ = error "trying to access unexistent scope"

getTopScope :: CCureState -> String
getTopScope a = takeWhile (/= '#') (getCurrentScope a)

getLastScop :: String -> String
getLastScop a = reverse $ takeWhile (/= '#') $ reverse a

removeFromScopeStack :: CCureState -> CCureState
removeFromScopeStack (_, [], _, _, _, _, _, _) = error "trying to remove unexistent scope"
removeFromScopeStack (a, stack:tail, b, c, d, e, f, g) = (a, tail, b, c, d, e, f, g)

-- getCurrentScope :: CCureState -> String
-- getCurrentScope (_, [top]:tail, _) = getCurrentScopeAux(top)

-- getCurrentScopeAux :: String -> String
-- getCurrentScopeAux [] = ""
-- getCurrentScopeAux '#':tail = ""
-- getCurrentScopeAux top:tail = top : getCurrentScopeAux tail

getCurrentDepth :: CCureState -> Int
getCurrentDepth (_, _, _, a, _, _, _, _) = a

addDepth :: CCureState -> CCureState
addDepth (a, b, c, d, e, f, g, h) = (a, b, c, d + 1, e, f, g, h)

removeDepth :: CCureState -> CCureState
removeDepth (a, b, c, 0, e, f, g, h) = error "trying to remove base depth"
removeDepth (a, b, c, d, e, f, g, h) = (a, b, c, d - 1, e, f, g, h)


execOn :: CCureState -> Bool
execOn (_, _, _, _, _, _, _, True) = True
execOn (_, _, _, _, _, _, _, False) = False

turnExecOn :: CCureState -> CCureState
turnExecOn (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g, True)

turnExecOff :: CCureState -> CCureState
turnExecOff (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g, False)
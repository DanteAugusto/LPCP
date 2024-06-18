module State where

import Lexer
import Text.Parsec

type ExAct = Bool
-- Nome, profundidade de chamada, valor idea do grande amigo fouquet
type SymTable = [(String, [(Int, Token)])]
type SymTableErrada = [(Token, Token)]


-- type ExecStack = [[]] pilha de instancias de ra (linha pra onde devo voltar e valor de retorno)
-- type ActRegSubprog = [(String, [String])]
type ScopeStack = [String]

type CCureState = (SymTableErrada, ScopeStack, ExAct)

-- funções auxiliares para o CCureState

execOn :: CCureState -> Bool
execOn (_, _, True) = True
execOn (_, _, False) = False

turnExecOn :: CCureState -> CCureState
turnExecOn (a, b, _) = (a, b, True)

turnExecOff :: CCureState -> CCureState
turnExecOff (a, b, _) = (a, b, False)
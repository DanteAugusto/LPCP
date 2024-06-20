{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use <$>" #-}
module Main (main) where

import Lexer
import State
import Tokens
import Text.Read
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Read.Lex
import Data.Maybe
import Distribution.Compat.Lens (_1)

-- alias para tipos usados
type ParsecType = ParsecT [Token] CCureState IO(Type, [Token])
type ParsecTokenType = ParsecT [Token] CCureState IO (Token)

-- parsers para os não-terminais
program :: ParsecT [Token] CCureState IO ([Token])
program = do
            updateState(addToScopeStack "program")
            a <- programToken 
            b <- stmts
            c <- endToken
            s <- getState
            updateState(symtable_remove_scope (getCurrentScope s))
            updateState(removeFromScopeStack)
            eof
            s <- getState
            liftIO (print s)
            return ([a] ++ b ++ [c])

typeToken :: ParsecT [Token] CCureState IO(Token)
typeToken = try intToken <|> doubleToken <|> boolToken

numericalTypeToken :: ParsecT [Token] CCureState IO(Token)
numericalTypeToken = try intToken <|> doubleToken


matrixDecl :: ParsecT [Token] CCureState IO([Token])
matrixDecl = do
              -- matrix<linha, coluna, tipo(int ou double)> m = 0;
              mat <- matrixToken

              l <- lessToken
              lin <- intLitToken
              a <- commaToken

              col <- intLitToken
              b <- commaToken

              typ <- numericalTypeToken
              r <- greatToken
              
              id <- idToken
              assig <- assignToken
              defaultVal <- numericalTypeToken

              sc <- semiColonToken 

              s <- getState
              
              if(execOn s) then do
                -- aq tem q checar se o tipo é valido, ou seja:
                -- se lin e col sao > 0
                -- se defaultVal é compatível com typ 

                -- se tiver tudo ok, construir a matriz e colocar na tabela de simbolos
                return []
              else
                return (mat:l:lin:a:col:b:typ:r:id:assig:defaultVal:[sc])


varDecl :: ParsecT [Token] CCureState IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- expression
            -- liftIO (print "olha a expressao ai o")
            -- liftIO (print d)
            e <- semiColonToken
            s <- getState

            if(execOn s) then do
              -- liftIO(print c)
              if (not (compatible_varDecl a d)) then fail "type error on declaration"
              else 
                do
                  s <- getState
                  updateState(symtable_insert (b, getCurrentScope s, [(0, fst d)]))
                  s <- getState
                  -- liftIO (print s)
                  return (a:b:[c] ++ (snd d) ++ [e])
            else
              return (a:b:[c] ++ (snd d) ++ [e])

stmts :: ParsecT [Token] CCureState IO([Token])
stmts = do
          first <- stmt
          next <- remainingStmts
          return (first ++ next)

stmt :: ParsecT [Token] CCureState IO([Token])
stmt = try varDecl <|> assign <|> printPuts <|> readStup <|> whileStmt <|> breakStmt

remainingStmts :: ParsecT [Token] CCureState IO([Token])
remainingStmts = (do
                  a <- stmt
                  b <- remainingStmts 
                  return (a ++ b)) <|> return ([])

printPuts :: ParsecT [Token] CCureState IO([Token])
printPuts = do 
              a <- putsToken
              b <- openParentToken
              c <- expression
              d <- closeParentToken
              e <- semiColonToken
              s <- getState
              if(execOn s) then do
                liftIO (print (fst c))
              else pure()
              return (a:[b] ++ (snd c) ++ d:[e])

readStup :: ParsecT [Token] CCureState IO([Token])
readStup = do 
              a <- stupToken
              b <- openParentToken
              c <- idToken
              d <- commaToken
              e <- typeToken
              f <- closeParentToken
              g <- semiColonToken
              s <- getState
              if(execOn s) then do
                input <- liftIO(getLine)
                let strConverted = convertStringToType input e
                if(not (compatible (get_type c s, []) (strConverted, []) )) then fail "type error on input"
                else do
                  updateState(symtable_update (c, 0 ,strConverted))
                  return (a:b:c:d:e:f:[g])
                
              else return (a:b:c:d:e:f:[g])

convertStringToType :: String -> Token -> Type
convertStringToType x (Double _) =
      case (readDouble x) of
            Right num -> (DoubleType num)
            Left err  -> error "could not convert input to double value"
convertStringToType x (Int _) =
      case (readInt x) of
            Right num -> (IntType num)
            Left err  -> error "could not convert input to int value"
convertStringToType x (Bool _) =
      if (x == "True") then (BoolType True)
      else do
        if (x == "False") then (BoolType False)
        else error "could not convert input to boolean value"

-- Função para ler e converter uma String em Int
readInt :: String -> Either String Int
readInt str = readEither str

-- Função para ler e converter uma String em Double
readDouble :: String -> Either String Double
readDouble str = readEither str

whileStmt :: ParsecT [Token] CCureState IO([Token])
whileStmt = do
              -- s <- getState
              -- liftIO(print s)
              pc <- getInput
              -- liftIO( print pc )
              a <- whileToken
              b <- enclosed_exp
              
              s <- getState

              if(execOn s) then do
                updateState(addToScopeStack "while")
                updateState(addToLoopStack OK)
                if( not (compatible b (BoolType True, []) ) ) then fail "control expression on while must be a boolean"
                else 
                  if(get_bool_value (fst b)) then do
                    c <- stmts
                    d <- endWhileToken
                    -- liftIO(print pc)


                    s <- getState
                    -- Se apos os stmts o exec tiver off, leu um break ou continue
                    if(not (execOn s)) then do
                      -- Se foi um break, acaba o loop
                      if((getCurrentLoopStatus s) == BREAK) then do 
                        updateState(turnExecOn)
                        s <- getState
                        updateState(symtable_remove_scope (getCurrentScope s))
                        updateState(removeFromScopeStack)
                        updateState(removeFromLoopStack)
                        return ([a] ++ (snd b) ++ c ++ [d])
                      else do
                        return ([a] ++ (snd b) ++ c ++ [d])
                      -- Se foi um continue, continua o loop
                    else do
                      setInput(pc)
                      s <- getState
                      updateState(symtable_remove_scope (getCurrentScope s))
                      updateState(removeFromScopeStack)
                      updateState(removeFromLoopStack)
                      return ([a] ++ (snd b) ++ c ++ [d])
                  else do
                    -- liftIO(print pc)
                    -- Desativo a execução aqui
                    updateState(turnExecOff)
                    c <- stmts
                    d <- endWhileToken
                    -- Ativo a execução aqui
                    updateState(turnExecOn)
                    s <- getState
                    updateState(removeFromScopeStack)
                    updateState(removeFromLoopStack)
                    return ([a] ++ (snd b) ++ c ++ [d])
              else do
                c <- stmts
                d <- endWhileToken
                return ([a] ++ (snd b) ++ c ++ [d])

breakStmt :: ParsecT [Token] CCureState IO([Token])
breakStmt = do
              a <- breakToken
              b <- semiColonToken

              s <- getState
              if(execOn s) then do

                
                -- Checar se estou dentro de um loop
                s <- getState


                if((getCurrentScope s)== "while") then do
                  updateState(turnExecOff)
                  -- updateState((addToLoopStack.removeFromLoopStack) BREAK)
                  updateState(removeFromLoopStack)
                  updateState(addToLoopStack BREAK)
                  -- if((getCurrentLoopStatus s) == OK) then
                  --   updateState(addToLoopStack BREAK)
                  -- else pure()
                else
                  fail "break statement out of loop structure"
              else pure()
              
              return (a:[b])


assign :: ParsecT [Token] CCureState IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          d <- semiColonToken
          s <- getState
          if(execOn s) then do
            -- liftIO(print c)
            if (not (compatible (get_type a s, []) c)) then fail "type error on assign"
            else 
              do 
                updateState(symtable_update (a, 0, fst c))
                -- s <- getState
                -- liftIO (print s)
                return (a:[b] ++ (snd c) ++ [d])
          else
            return (a:[b] ++ (snd c) ++ [d])

-- expression :: ParsecT [Token] CCureState IO(Token)
-- expression = try bin_expression <|> una_expression

expression :: ParsecT [Token] CCureState IO(Type, [Token])
expression = bool_expression

bool_expression :: ParsecT [Token] CCureState IO(Type, [Token])
bool_expression = and_expression

and_expression :: ParsecT [Token] CCureState IO(Type, [Token])
and_expression = (do 
                    a <- or_expression
                    result <- eval_remaining a andToken or_expression
                    return result)

or_expression :: ParsecT [Token] CCureState IO(Type, [Token])
or_expression = (do 
                    a <- neg_expression
                    result <- eval_remaining a orToken neg_expression
                    return result)

neg_expression :: ParsecT [Token] CCureState IO(Type, [Token])
neg_expression = (do 
                    op <- negToken
                    rel <- rel_expression
                    s <- getState
                    return (eval_unary op rel (execOn s))) <|>
                  (do 
                    a <- rel_expression
                    return a)

rel_operator :: ParsecT [Token] CCureState IO(Token)
rel_operator = try eqToken <|> diffToken <|> leqToken <|> geqToken <|> lessToken <|> greatToken

rel_expression :: ParsecT [Token] CCureState IO(Type, [Token])
rel_expression = (do 
                    a <- sum_expression
                    result <- eval_remaining a rel_operator sum_expression
                    return result)

sum_minus :: ParsecT [Token] CCureState IO(Token)
sum_minus = try plusToken <|> minusToken

sum_expression :: ParsecT [Token] CCureState IO(Type, [Token])
sum_expression = (do 
                    a <- unary_minus_expression
                    result <- eval_remaining a sum_minus unary_minus_expression
                    return result)

unary_minus_expression :: ParsecT [Token] CCureState IO(Type, [Token])
unary_minus_expression = (do 
                            op <- minusToken
                            t <- term
                            s <- getState
                            return (eval_unary op t (execOn s))) <|>
                         (do 
                            a <- term
                            return a)                  

mult_div_mod :: ParsecT [Token] CCureState IO(Token)
mult_div_mod = try multToken <|> diviToken <|> modToken
                
term :: ParsecT [Token] CCureState IO(Type, [Token])
term = (do
          a <- factor
          result <- eval_remaining a mult_div_mod factor
          return result)

factor :: ParsecT [Token] CCureState IO(Type, [Token])
factor = (do
            a <- expi
            result <- eval_remaining_right a expoToken expi
            return result
          )

enclosed_exp :: ParsecT [Token] CCureState IO(Type, [Token])      
enclosed_exp = do
                a <- openParentToken
                b <- expression
                c <- closeParentToken
                return b 

numTypeToken :: ParsecT [Token] CCureState IO(Token) 
numTypeToken = try intToken <|> doubleToken

castParser :: ParsecT [Token] CCureState IO(Type, [Token])   
castParser = do 
              ct <- castToken
              openP <- openParentToken
              (valExp, exp) <- expression
              c <- commaToken
              t <- numTypeToken
              closeP <- closeParentToken  
              if (not (canCast valExp t)) then fail "cast error"
              else
                do
                  return (cast valExp t, ct:[openP] ++ exp ++ c:t:[closeP])

canCast :: Type -> Token -> Bool
canCast (IntType _) (Int _)       = True
canCast (IntType _) (Double _)    = True
canCast (DoubleType _) (Double _) = True
canCast _ _                        = False

cast :: Type -> Token -> Type
cast (IntType v) (Int _)       = IntType v 
cast (IntType v) (Double _)    = DoubleType (fromIntegral v)
cast (DoubleType v) (Double _) = DoubleType v

expi :: ParsecT [Token] CCureState IO(Type, [Token])      
expi = try (do
        x <- intLitToken <|> doubleLitToken <|> boolLitToken
        return (tokenToType x, [x])) <|> variableParser <|> castParser <|> enclosed_exp

tokenToType :: Token -> Type
tokenToType (IntLit v _)    = IntType v
tokenToType (DoubleLit v _) = DoubleType v
tokenToType (BoolLit v _)   = BoolType v
tokenToType _               = undefined

variableParser :: ParsecT [Token] CCureState IO(Type, [Token])
variableParser = do
                  id <- idToken
                  s <- getState
                  if(execOn s) then
                    return (getMayb (symtable_get (id, 0) s), [id])
                  else
                    return (NULL, [id])

                  
getMayb :: Maybe a -> a
getMayb (Just a) = a

--- funções considerando associatividade à esquerda                  
eval_remaining :: (Type, [Token]) -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Type, [Token])
eval_remaining n1 operator remain = (do
                                op <- operator
                                n2 <- remain
                                s <- getState
                                if (execOn s) then do
                                  if (not (compatible_op n1 op n2)) then fail "type error on evaluating expression"
                                  else
                                    do
                                      result <- eval_remaining (eval n1 op n2 (execOn s)) operator remain
                                      return (result)
                                else do
                                  result <- eval_remaining (eval n1 op n2 (execOn s)) operator remain
                                  return (result))
                              <|> return n1

eval_remaining_right :: (Type, [Token]) -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Type, [Token])
eval_remaining_right n1 operator remain = (do
                                op <- operator
                                n2 <- remain
                                s <- getState
                                result <- eval_remaining_right n2 operator remain
                                if(execOn s) then do
                                  if (not (compatible_op n1 op n2)) then fail "type error on evaluating expression"
                                  else
                                    do
                                      return (eval n1 op result (execOn s))
                                else do
                                  return (eval n1 op result (execOn s)))
                              <|> return n1    

eval_unary :: Token -> (Type, [Token]) -> Bool -> (Type, [Token])
eval_unary op a False = (NULL, op:(snd a))                         -- Não importa a saída, pois o exec ta off
eval_unary (Neg p) (BoolType x, a) True = (BoolType (not x), a)
eval_unary (Plus p) (IntType x, a) True = (IntType (x), a)
eval_unary (Plus p) (DoubleType x, a) True = (DoubleType (x), a)
eval_unary (Minus p) (IntType x, a) True = (IntType (-x), a)
eval_unary (Minus p) (DoubleType x, a) True = (DoubleType (-x), a)

eval :: (Type, [Token]) -> Token -> (Type, [Token]) -> Bool -> (Type, [Token])
eval a op b False = (NULL, (snd a) ++ [op] ++ (snd b))                         -- Não importa a saída, pois o exec ta off
eval (IntType x, a) (Plus op) (IntType y, b) True = (IntType (x + y), a ++ [(Plus op)] ++ b)
eval (IntType x, a) (Minus op) (IntType y, b) True = (IntType (x - y), a ++ [(Minus op)] ++ b)
eval (IntType x, a) (Mult op) (IntType y, b) True = (IntType (x * y), a ++ [(Mult op)] ++ b)
eval (IntType x, a) (Divi op) (IntType y, b) True = (IntType (x `div` y), a ++ [(Divi op)] ++ b)
eval (IntType x, a) (Mod op) (IntType y, b) True = (IntType (x `mod` y), a ++ [(Mod op)] ++ b)
eval (IntType x, a) (Expo op) (IntType y, b) True = (IntType (x ^ y), a ++ [(Expo op)] ++ b)
eval (IntType x, a) (Lesser op) (IntType y, b) True = (BoolType (x < y), a ++ [(Lesser op)] ++ b)
eval (IntType x, a) (Greater op) (IntType y, b) True = (BoolType (x > y), a ++ [(Greater op)] ++ b)
eval (IntType x, a) (LessEq op) (IntType y, b) True = (BoolType (x <= y), a ++ [(LessEq op)] ++ b)
eval (IntType x, a) (GreatEq op) (IntType y, b) True = (BoolType (x >= y), a ++ [(GreatEq op)] ++ b)
eval (IntType x, a) (Eq op) (IntType y, b) True = (BoolType (x == y), a ++ [(Eq op)] ++ b)
eval (IntType x, a) (Diff op) (IntType y, b) True = (BoolType (x /= y), a ++ [(Diff op)] ++ b)

eval (DoubleType x, a) (Plus op) (DoubleType y, b) True = (DoubleType (x + y), a ++ [(Plus op)] ++ b)
eval (DoubleType x, a) (Minus op) (DoubleType y, b) True = (DoubleType (x - y), a ++ [(Minus op)] ++ b)
eval (DoubleType x, a) (Mult op) (DoubleType y, b) True = (DoubleType (x * y), a ++ [(Mult op)] ++ b)
eval (DoubleType x, a) (Divi op) (DoubleType y, b) True = (DoubleType (x / y), a ++ [(Divi op)] ++ b)
eval (DoubleType x, a) (Expo op) (DoubleType y, b) True = (DoubleType (x ** y), a ++ [(Expo op)] ++ b)
eval (DoubleType x, a) (Lesser op) (DoubleType y, b) True = (BoolType (x < y), a ++ [(Lesser op)] ++ b)
eval (DoubleType x, a) (Greater op) (DoubleType y, b) True = (BoolType (x > y), a ++ [(Greater op)] ++ b)
eval (DoubleType x, a) (LessEq op) (DoubleType y, b) True = (BoolType (x <= y), a ++ [(LessEq op)] ++ b)
eval (DoubleType x, a) (GreatEq op) (DoubleType y, b) True = (BoolType (x >= y), a ++ [(GreatEq op)] ++ b)
eval (DoubleType x, a) (Eq op) (DoubleType y, b) True = (BoolType (x == y), a ++ [(Eq op)] ++ b)
eval (DoubleType x, a) (Diff op) (DoubleType y, b) True = (BoolType (x /= y), a ++ [(Diff op)] ++ b)

eval (BoolType x, a) (And op) (BoolType y, b) True = (BoolType (x && y), a ++ [(And op)] ++ b)
eval (BoolType x, a) (Or op) (BoolType y, b) True = (BoolType (x || y), a ++ [(Or op)] ++ b)
eval (BoolType x, a) (Eq op) (BoolType y, b) True = (BoolType (x == y), a ++ [(Eq op)] ++ b)
eval (BoolType x, a) (Diff op) (BoolType y, b) True = (BoolType (x /= y), a ++ [(Diff op)] ++ b)

compatible_op :: (Type, [Token]) -> Token -> (Type, [Token]) -> Bool
compatible_op (IntType _, _) (Plus _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Minus _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Mult _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Divi _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Mod _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Expo _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Lesser _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Greater _ ) (IntType _, _) = True
compatible_op (IntType _, _) (LessEq _ ) (IntType _, _) = True
compatible_op (IntType _, _) (GreatEq _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Eq _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Diff _ ) (IntType _, _) = True

compatible_op (DoubleType _, _) (Plus _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Minus _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Mult _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Divi _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Expo _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Lesser _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Greater _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (LessEq _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (GreatEq _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Eq _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Diff _ ) (DoubleType _, _) = True

compatible_op (BoolType _, _) (And _ ) (BoolType _, _) = True
compatible_op (BoolType _, _) (Or _ ) (BoolType _, _) = True
compatible_op (BoolType _, _) (Eq _ ) (BoolType _, _) = True
compatible_op (BoolType _, _) (Diff _ ) (BoolType _, _) = True

compatible_op _ _ _ = False

compatible :: (Type, [Token]) -> (Type, [Token]) -> Bool
compatible (IntType _, _) (IntType _, _) = True
compatible (DoubleType _, _) (DoubleType _, _) = True
compatible (BoolType _, _) (BoolType _, _) = True
compatible _ _ = False

compatible_varDecl :: Token -> (Type, [Token]) -> Bool
compatible_varDecl (Int _) (IntType _, _) = True
compatible_varDecl (Double _) (DoubleType _, _) = True
compatible_varDecl (Bool _) (BoolType _, _) = True
compatible_varDecl _ _ = False

-- funções para a tabela de símbolos

get_type :: Token -> CCureState -> Type
get_type _ ([], _, _, _) = error "variable not found"
get_type (Id id1 p1) (  (Id id2 _, _, (_, value):tail):t , a, b, c) = if id1 == id2 then value
                                             else get_type (Id id1 p1) (t, a, b, c)
-- get_type (Id id1 p1) _ = error "o misterio"

get_bool_value :: Type -> Bool
get_bool_value (BoolType a) = a
get_bool_value _ = error "token is not a boolean"

symtable_insert :: (Token, String, [(Int, Type)]) -> CCureState -> CCureState
symtable_insert symbol ([], a, b, c)  = ([symbol], a, b, c)
symtable_insert symbol (symtable, a, b, c) = ((symbol:symtable), a, b, c)

symtable_update :: (Token, Int, Type) -> CCureState -> CCureState
symtable_update a (b, c, d, e) = (symtable_update_aux a b, c, d, e)

symtable_update_aux :: (Token, Int, Type) -> SymTable -> SymTable
symtable_update_aux _ [] = fail "variable not found"
symtable_update_aux (Id id1 p1, depth1, v1) ((Id id2 p2, scop, (depth2, v2):tail):t   ) = 
                               if ((id1, depth1) == (id2, depth2)) then (Id id2 p1, scop, (depth2, v1):tail):t
                               else (Id id2 p2, scop, (depth2, v2):tail) : symtable_update_aux (Id id1 p1, depth1, v1) t

-- Percorre a lista enquanto String for igual a escopo. Depois para
symtable_remove_scope :: String -> CCureState -> CCureState
symtable_remove_scope a (b, c, d, e) = (symtable_remove_scope_aux a b, c, d, e)

symtable_remove_scope_aux :: String -> SymTable -> SymTable
symtable_remove_scope_aux _ [] = []
symtable_remove_scope_aux a ((Id id2 p2, scop, (depth2, v2):tail):t   ) =
                            if(a == scop) then symtable_remove_scope_aux a t
                            else (Id id2 p2, scop, (depth2, v2):tail) : t

-- symtable_remove :: (Token,Token) -> CCureState -> CCureState
-- symtable_remove a (b, c, d, e) = (symtable_remove_aux a b, c, d, e)

-- symtable_remove_aux :: (Token,Token) -> SymTable -> SymTable
-- symtable_remove_aux _ [] = fail "variable not found"
-- symtable_remove_aux (id1, v1) ((id2, v2):t) = 
--                                if id1 == id2 then t
--                                else (id2, v2) : symtable_remove_aux (id1, v1) t   

symtable_get :: (Token, Int) -> CCureState -> Maybe Type
symtable_get (a, d) (b, _, _, _) = symtable_get_aux (a, d) b

symtable_get_aux :: (Token, Int) -> SymTable -> Maybe Type
symtable_get_aux _ [] = fail "variable not found"
symtable_get_aux (Id id1 p1, depth1) ((Id id2 p2, scop, (depth2, v2):tail):t   ) = 
                            if ((id1, depth1) == (id2, depth2)) then Just v2
                            else symtable_get_aux (Id id1 p1, depth1) t

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program ([], [], [], True) "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "problemas/problema2.ccr")) of
            { Left err -> print err; 
              Right ans -> print ans
            }

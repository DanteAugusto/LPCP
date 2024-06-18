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
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

-- alias para tipos usados
type ParsecType = ParsecT [Token] CCureState IO (Token)
type ParsecTokenType = ParsecT [Token] CCureState IO (Token)

-- parsers para os não-terminais
program :: ParsecT [Token] CCureState IO ([Token])
program = do
            a <- programToken 
            b <- stmts
            c <- endToken
            s <- getState
            eof
            liftIO (print s)
            return ([a] ++ b ++ [c])

typeToken :: ParsecT [Token] CCureState IO(Token)
typeToken = try intToken <|> doubleToken <|> boolToken

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
                  updateState(symtable_insert (b, d))
                  s <- getState
                  -- liftIO (print s)
                  return (a:b:c:d:[e])
            else
              return (a:b:c:d:[e])

stmts :: ParsecT [Token] CCureState IO([Token])
stmts = do
          first <- stmt
          next <- remainingStmts
          return (first ++ next)

stmt :: ParsecT [Token] CCureState IO([Token])
stmt = try varDecl <|> assign <|> printPuts <|> whileStmt

printPuts :: ParsecT [Token] CCureState IO([Token])
printPuts = do 
              a <- putsToken
              b <- openParentToken
              c <- expression
              d <- closeParentToken
              e <- semiColonToken
              s <- getState
              if(execOn s) then do
                liftIO (print c)
              else pure()
              return (a:b:c:d:[e])

whileStmt :: ParsecT [Token] CCureState IO([Token])
whileStmt = do
              -- s <- getState
              -- liftIO(print s)
              pc <- getInput
              -- liftIO( print pc )
              a <- whileToken
              b <- enclosed_exp
              
              s <- getState

              if(execOn s) then
                if( not (compatible b (BoolLit True (0,0))) ) then fail "control expression on while must be a boolean"
                else 
                  if(get_bool_value b) then do
                    c <- stmts
                    d <- endWhileToken
                    -- liftIO(print pc)
                    setInput(pc)
                    return (a:b:c ++ [d])
                  else do
                    -- liftIO(print pc)
                    -- Desativo a execução aqui
                    updateState(turnExecOff)
                    c <- stmts
                    d <- endWhileToken
                    -- Ativo a execução aqui
                    updateState(turnExecOn)
                    return (a:b:c ++ [d])
              else do
                c <- stmts
                d <- endWhileToken
                return (a:b:c ++ [d])

remainingStmts :: ParsecT [Token] CCureState IO([Token])
remainingStmts = (do
                  a <- stmt
                  b <- remainingStmts 
                  return (a ++ b)) <|> return ([])

assign :: ParsecT [Token] CCureState IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          d <- semiColonToken
          s <- getState
          if(execOn s) then do
            -- liftIO(print c)
            if (not (compatible (get_type a s) c)) then fail "type error on assign"
            else 
              do 
                updateState(symtable_update (a, c))
                -- s <- getState
                -- liftIO (print s)
                return (a:b:c:[d])
          else
            return (a:b:c:[d])

-- expression :: ParsecT [Token] CCureState IO(Token)
-- expression = try bin_expression <|> una_expression

expression :: ParsecT [Token] CCureState IO(Token)
expression = bool_expression

bool_expression :: ParsecT [Token] CCureState IO(Token)
bool_expression = and_expression

and_expression :: ParsecT [Token] CCureState IO(Token)
and_expression = (do 
                    a <- or_expression
                    result <- eval_remaining a andToken or_expression
                    return result)

or_expression :: ParsecT [Token] CCureState IO(Token)
or_expression = (do 
                    a <- neg_expression
                    result <- eval_remaining a orToken neg_expression
                    return result)

neg_expression :: ParsecT [Token] CCureState IO(Token)
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

rel_expression :: ParsecT [Token] CCureState IO(Token)
rel_expression = (do 
                    a <- sum_expression
                    result <- eval_remaining a rel_operator sum_expression
                    return result)

sum_minus :: ParsecT [Token] CCureState IO(Token)
sum_minus = try plusToken <|> minusToken

sum_expression :: ParsecT [Token] CCureState IO(Token)
sum_expression = (do 
                    a <- unary_minus_expression
                    result <- eval_remaining a sum_minus unary_minus_expression
                    return result)

unary_minus_expression :: ParsecT [Token] CCureState IO(Token)
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
                
term :: ParsecT [Token] CCureState IO(Token)
term = (do
          a <- factor
          result <- eval_remaining a mult_div_mod factor
          return result)

factor :: ParsecT [Token] CCureState IO(Token)
factor = (do
            a <- expi
            result <- eval_remaining_right a expoToken expi
            return result
          )

enclosed_exp :: ParsecT [Token] CCureState IO(Token)      
enclosed_exp = do
                a <- openParentToken
                b <- expression
                c <- closeParentToken
                return b 

numTypeToken :: ParsecT [Token] CCureState IO(Token) 
numTypeToken = try intToken <|> doubleToken

castParser :: ParsecT [Token] CCureState IO(Token)   
castParser = do 
              ct <- castToken
              openP <- openParentToken
              exp <- expression
              c <- commaToken
              t <- numTypeToken
              closeP <- closeParentToken  
              if (not (canCast exp t)) then fail "cast error"
              else
                do
                  return (cast exp t)

canCast :: Token -> Token -> Bool
canCast (IntLit _ _) (Int _)       = True
canCast (IntLit _ _) (Double _)    = True
canCast (DoubleLit _ _) (Double _) = True
canCast _ _                        = False

cast :: Token -> Token -> Token
cast (IntLit v p) (Int _)       = IntLit v p
cast (IntLit v p) (Double _)    = DoubleLit (fromIntegral v) p
cast (DoubleLit v p) (Double _) = DoubleLit v p

expi :: ParsecT [Token] CCureState IO(Token)      
expi = try intLitToken <|> doubleLitToken <|> boolLitToken <|> castParser <|> enclosed_exp <|> variableParser

variableParser :: ParsecT [Token] CCureState IO(Token)
variableParser = do
                  id <- idToken
                  s <- getState
                  if(execOn s) then
                    return (getMayb (symtable_get id s))
                  else
                    return (IntLit (0) (0 , 0))

                  
getMayb :: Maybe a -> a
getMayb (Just a) = a

--- funções considerando associatividade à esquerda                  
eval_remaining :: Token -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Token)
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
                              <|> return (n1)
                    
eval_remaining_right :: Token -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Token)
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
                              <|> return (n1)        

eval_unary :: Token -> Token -> Bool -> Token
eval_unary _ _ False = IntLit (0) (0 , 0)                         -- Não importa a saída, pois o exec ta off
eval_unary (Neg p) (BoolLit x _) True = BoolLit (not x) p
eval_unary (Plus p) (IntLit x _) True = IntLit (x) p
eval_unary (Plus p) (DoubleLit x _) True = DoubleLit (x) p
eval_unary (Minus p) (IntLit x _) True = IntLit (-x) p
eval_unary (Minus p) (DoubleLit x _) True = DoubleLit (-x) p

eval :: Token -> Token -> Token -> Bool -> Token
eval _ _ _ False = IntLit (0) (0 , 0)                         -- Não importa a saída, pois o exec ta off
eval (IntLit x p) (Plus _ ) (IntLit y _) True = IntLit (x + y) p
eval (IntLit x p) (Minus _ ) (IntLit y _) True = IntLit (x - y) p
eval (IntLit x p) (Mult _ ) (IntLit y _) True = IntLit (x * y) p
eval (IntLit x p) (Divi _ ) (IntLit y _) True = IntLit (x `div` y) p
eval (IntLit x p) (Mod _ ) (IntLit y _) True = IntLit (x `mod` y) p
eval (IntLit x p) (Expo _ ) (IntLit y _) True = IntLit (x ^ y) p
eval (IntLit x p) (Lesser _ ) (IntLit y _) True = BoolLit (x < y) p
eval (IntLit x p) (Greater _ ) (IntLit y _) True = BoolLit (x > y) p
eval (IntLit x p) (LessEq _ ) (IntLit y _) True = BoolLit (x <= y) p
eval (IntLit x p) (GreatEq _ ) (IntLit y _) True = BoolLit (x >= y) p
eval (IntLit x p) (Eq _ ) (IntLit y _) True = BoolLit (x == y) p
eval (IntLit x p) (Diff _ ) (IntLit y _) True = BoolLit (x /= y) p

eval (DoubleLit x p) (Plus _ ) (DoubleLit y _) True = DoubleLit (x + y) p
eval (DoubleLit x p) (Minus _ ) (DoubleLit y _) True = DoubleLit (x - y) p
eval (DoubleLit x p) (Mult _ ) (DoubleLit y _) True = DoubleLit (x * y) p
eval (DoubleLit x p) (Divi _ ) (DoubleLit y _) True = DoubleLit (x / y) p
eval (DoubleLit x p) (Expo _ ) (DoubleLit y _) True = DoubleLit (x ** y) p
eval (DoubleLit x p) (Lesser _ ) (DoubleLit y _) True = BoolLit (x < y) p
eval (DoubleLit x p) (Greater _ ) (DoubleLit y _) True = BoolLit (x > y) p
eval (DoubleLit x p) (LessEq _ ) (DoubleLit y _) True = BoolLit (x <= y) p
eval (DoubleLit x p) (GreatEq _ ) (DoubleLit y _) True = BoolLit (x >= y) p
eval (DoubleLit x p) (Eq _ ) (DoubleLit y _) True = BoolLit (x == y) p
eval (DoubleLit x p) (Diff _ ) (DoubleLit y _) True = BoolLit (x /= y) p

eval (BoolLit x p) (And _ ) (BoolLit y _) True = BoolLit (x && y) p
eval (BoolLit x p) (Or _ ) (BoolLit y _) True = BoolLit (x || y) p
eval (BoolLit x p) (Eq _ ) (BoolLit y _) True = BoolLit (x == y) p
eval (BoolLit x p) (Diff _ ) (BoolLit y _) True = BoolLit (x /= y) p

compatible_op :: Token -> Token -> Token -> Bool
compatible_op (IntLit _ _) (Plus _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Minus _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Mult _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Divi _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Mod _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Expo _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Lesser _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Greater _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (LessEq _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (GreatEq _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Eq _ ) (IntLit _ _) = True
compatible_op (IntLit _ _) (Diff _ ) (IntLit _ _) = True

compatible_op (DoubleLit _ _) (Plus _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Minus _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Mult _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Divi _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Expo _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Lesser _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Greater _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (LessEq _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (GreatEq _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Eq _ ) (DoubleLit _ _) = True
compatible_op (DoubleLit _ _) (Diff _ ) (DoubleLit _ _) = True

compatible_op (BoolLit _ _) (And _ ) (BoolLit _ _) = True
compatible_op (BoolLit _ _) (Or _ ) (BoolLit _ _) = True
compatible_op (BoolLit _ _) (Eq _ ) (BoolLit _ _) = True
compatible_op (BoolLit _ _) (Diff _ ) (BoolLit _ _) = True

compatible_op _ _ _ = False

compatible :: Token -> Token -> Bool
compatible (IntLit _ _) (IntLit _ _) = True
compatible (DoubleLit _ _) (DoubleLit _ _) = True
compatible (BoolLit _ _) (BoolLit _ _) = True
compatible _ _ = False

compatible_varDecl :: Token -> Token -> Bool
compatible_varDecl (Int _) (IntLit _ _) = True
compatible_varDecl (Double _) (DoubleLit _ _) = True
compatible_varDecl (Bool _) (BoolLit _ _) = True
compatible_varDecl _ _ = False

-- funções para a tabela de símbolos

get_type :: Token -> CCureState -> Token
get_type _ ([], _, _) = error "variable not found"
get_type (Id id1 p1) (((Id id2 _, value):t), a, b) = if id1 == id2 then value
                                             else get_type (Id id1 p1) (t, a, b)
-- get_type (Id id1 p1) _ = error "o misterio"

get_bool_value :: Token -> Bool
get_bool_value (BoolLit a _) = a
get_bool_value _ = error "token is not a boolean"

symtable_insert :: (Token,Token) -> CCureState -> CCureState
symtable_insert symbol ([], a, b)  = ([symbol], a, b)
symtable_insert symbol (symtable, a, b) = ((symtable ++ [symbol]), a, b)

symtable_update :: (Token,Token) -> CCureState -> CCureState
symtable_update a (b, c, d) = (symtable_update_aux a b, c, d)

symtable_update_aux :: (Token, Token) -> SymTableErrada -> SymTableErrada
symtable_update_aux _ [] = fail "variable not found"
symtable_update_aux (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
                               if id1 == id2 then (Id id1 p2, v1) : t
                               else (Id id2 p2, v2) : symtable_update_aux (Id id1 p1, v1) t

symtable_remove :: (Token,Token) -> CCureState -> CCureState
symtable_remove a (b, c, d) = (symtable_remove_aux a b, c, d)

symtable_remove_aux :: (Token,Token) -> SymTableErrada -> SymTableErrada
symtable_remove_aux _ [] = fail "variable not found"
symtable_remove_aux (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove_aux (id1, v1) t   

symtable_get :: Token -> CCureState -> Maybe Token
symtable_get a (b, _, _) = symtable_get_aux a b

symtable_get_aux :: Token -> SymTableErrada -> Maybe Token
symtable_get_aux _ [] = fail "variable not found"
symtable_get_aux (Id id1 p1) ((Id id2 p2, v2):t) = 
                            if (id1 == id2) then Just v2
                            else symtable_get_aux (Id id1 p1) t

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program ([], [], True) "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "soma.ccr")) of
            { Left err -> print err; 
              Right ans -> print ans
            }

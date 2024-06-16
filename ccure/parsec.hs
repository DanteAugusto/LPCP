{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use <$>" #-}
module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- parsers para os tokens

type ParsecType = ParsecT [Token] [(Token,Token)] IO (Token)
type ParsecTokenType = ParsecT [Token] [(Token,Token)] IO (Token)

programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _           = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _           = Nothing

openParentToken = tokenPrim show update_pos get_token where
  get_token (OpenParent p) = Just (OpenParent p)
  get_token _           = Nothing

closeParentToken = tokenPrim show update_pos get_token where
  get_token (CloseParent p) = Just (CloseParent p)
  get_token _           = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _        = Nothing

semiColonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon p) = Just (Semicolon p)
  get_token _             = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _          = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int p) = Just (Int p)
  get_token _         = Nothing

doubleToken = tokenPrim show update_pos get_token where
  get_token (Double p) = Just (Double p)
  get_token _         = Nothing

boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p) = Just (Bool p)
  get_token _         = Nothing

minusToken :: ParsecT [Token] st IO (Token)
minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p) = Just (Minus p)
  get_token _       = Nothing

diviToken :: ParsecT [Token] st IO (Token)
diviToken = tokenPrim show update_pos get_token where
  get_token (Divi p) = Just (Divi p)
  get_token _       = Nothing

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


update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  


intLitToken = tokenPrim show update_pos get_token where
  get_token (IntLit x p) = Just (IntLit x p)
  get_token _      = Nothing

doubleLitToken = tokenPrim show update_pos get_token where
  get_token (DoubleLit x p) = Just (DoubleLit x p)
  get_token _      = Nothing

boolLitToken :: ParsecT [Token] st IO (Token)
boolLitToken = tokenPrim show update_pos get_token where
  get_token (BoolLit x p) = Just (BoolLit x p)
  get_token _      = Nothing

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- stmts
            c <- endToken
            s <- getState
            eof
            -- liftIO (print s)
            return ([a] ++ b ++ [c])

typeToken :: ParsecT [Token] [(Token,Token)] IO(Token)
typeToken = try intToken <|> doubleToken <|> boolToken

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- expression
            -- liftIO (print "olha a expressao ai o")
            -- liftIO (print d)
            e <- semiColonToken
            s <- getState
            liftIO(print c)
            if (not (compatible_varDecl a d)) then fail "type error on declaration"
            else 
              do
                updateState(symtable_insert (b, d))
                s <- getState
                liftIO (print s)
                return (a:b:c:d:[e])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- varDecl
          next <- remainingStmts
          return (first ++ next)

remainingStmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remainingStmts = (do a <- assign
                     return a) <|> (return [])

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          d <- semiColonToken
          s <- getState
          liftIO(print c)
          if (not (compatible (get_type a s) c)) then fail "type error on assign"
          else 
            do 
              updateState(symtable_update (a, c))
              s <- getState
              liftIO (print s)
              return (a:b:c:[d])

get_type :: Token -> [(Token, Token)] -> Token
get_type _ [] = error "variable not found"
get_type (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                             else get_type (Id id1 p1) t
-- get_type (Id id1 p1) _ = error "o misterio"

-- expression :: ParsecT [Token] [(Token,Token)] IO(Token)
-- expression = try bin_expression <|> una_expression

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = bool_expression

bool_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bool_expression = and_expression

and_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
and_expression = (do 
                    a <- or_expression
                    result <- eval_remaining a andToken or_expression
                    return result)

or_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
or_expression = (do 
                    a <- neg_expression
                    result <- eval_remaining a orToken neg_expression
                    return result)

neg_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
neg_expression = (do 
                    op <- negToken
                    rel <- rel_expression
                    result <- eval_unary op rel
                    return result) <|>
                  (do 
                    a <- rel_expression
                    return a)

rel_operator :: ParsecT [Token] [(Token,Token)] IO(Token)
rel_operator = try eqToken <|> diffToken <|> leqToken <|> geqToken <|> lessToken <|> greatToken

rel_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
rel_expression = (do 
                    a <- sum_expression
                    result <- eval_remaining a rel_operator sum_expression
                    return result)

sum_minus :: ParsecT [Token] [(Token,Token)] IO(Token)
sum_minus = try plusToken <|> minusToken

sum_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
sum_expression = (do 
                    a <- term
                    result <- eval_remaining a sum_minus term
                    return result)

mult_div_mod :: ParsecT [Token] [(Token,Token)] IO(Token)
mult_div_mod = try multToken <|> diviToken <|> modToken
                
term :: ParsecT [Token] [(Token,Token)] IO(Token)
term = (do
          a <- factor
          result <- eval_remaining a mult_div_mod factor
          return result)

factor :: ParsecT [Token] [(Token,Token)] IO(Token)
factor = (do
            a <- expi
            result <- eval_remaining_right a expoToken expi
            return result
          )

enclosed_exp :: ParsecT [Token] [(Token,Token)] IO(Token)      
enclosed_exp = do
                a <- openParentToken
                b <- expression
                c <- closeParentToken
                return b 

expi :: ParsecT [Token] [(Token,Token)] IO(Token)      
expi = try intLitToken <|> doubleLitToken <|> boolLitToken <|> enclosed_exp

una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression = do
                   a <- intLitToken
                   return (a)
   
--- funções considerando associatividade à esquerda                  
eval_remaining :: Token -> ParsecTokenType -> ParsecType -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 operator remain = (do
                                op <- operator
                                n2 <- remain
                                if (not (compatible_op n1 op n2)) then fail "type error on evaluating expression"
                                else
                                  do
                                    result <- eval_remaining (eval n1 op n2) operator remain
                                    return (result)) 
                              <|> return (n1)
                    
eval_remaining_right :: Token -> ParsecTokenType -> ParsecType -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_right n1 operator remain = (do
                                op <- operator
                                n2 <- remain
                                result <- eval_remaining_right n2 operator remain
                                if (not (compatible_op n1 op n2)) then fail "type error on evaluating expression"
                                else
                                  do
                                    return (eval n1 op result)) 
                              <|> return (n1)        

eval_unary :: Token -> Token -> Token
eval_unary (Neg p) (BoolLit x _) = BoolLit (not x) p
eval_unary (Plus p) (IntLit x _) = IntLit (x) p
eval_unary (Plus p) (DoubleLit x _) = DoubleLit (x) p
eval_unary (Minus p) (IntLit x _) = IntLit (-x) p
eval_unary (Minus p) (DoubleLit x _) = DoubleLit (-x) p

eval :: Token -> Token -> Token -> Token
eval (IntLit x p) (Plus _ ) (IntLit y _) = IntLit (x + y) p
eval (IntLit x p) (Minus _ ) (IntLit y _) = IntLit (x - y) p
eval (IntLit x p) (Mult _ ) (IntLit y _) = IntLit (x * y) p
eval (IntLit x p) (Divi _ ) (IntLit y _) = IntLit (x `div` y) p
eval (IntLit x p) (Mod _ ) (IntLit y _) = IntLit (x `mod` y) p
eval (IntLit x p) (Expo _ ) (IntLit y _) = IntLit (x ^ y) p
eval (IntLit x p) (Lesser _ ) (IntLit y _) = BoolLit (x < y) p
eval (IntLit x p) (Greater _ ) (IntLit y _) = BoolLit (x > y) p
eval (IntLit x p) (LessEq _ ) (IntLit y _) = BoolLit (x <= y) p
eval (IntLit x p) (GreatEq _ ) (IntLit y _) = BoolLit (x >= y) p
eval (IntLit x p) (Eq _ ) (IntLit y _) = BoolLit (x == y) p
eval (IntLit x p) (Diff _ ) (IntLit y _) = BoolLit (x /= y) p

eval (DoubleLit x p) (Plus _ ) (DoubleLit y _) = DoubleLit (x + y) p
eval (DoubleLit x p) (Minus _ ) (DoubleLit y _) = DoubleLit (x - y) p
eval (DoubleLit x p) (Mult _ ) (DoubleLit y _) = DoubleLit (x * y) p
eval (DoubleLit x p) (Divi _ ) (DoubleLit y _) = DoubleLit (x / y) p
eval (DoubleLit x p) (Expo _ ) (DoubleLit y _) = DoubleLit (x ** y) p
eval (DoubleLit x p) (Lesser _ ) (DoubleLit y _) = BoolLit (x < y) p
eval (DoubleLit x p) (Greater _ ) (DoubleLit y _) = BoolLit (x > y) p
eval (DoubleLit x p) (LessEq _ ) (DoubleLit y _) = BoolLit (x <= y) p
eval (DoubleLit x p) (GreatEq _ ) (DoubleLit y _) = BoolLit (x >= y) p
eval (DoubleLit x p) (Eq _ ) (DoubleLit y _) = BoolLit (x == y) p
eval (DoubleLit x p) (Diff _ ) (DoubleLit y _) = BoolLit (x /= y) p

eval (BoolLit x p) (And _ ) (BoolLit y _) = BoolLit (x && y) p
eval (BoolLit x p) (Or _ ) (BoolLit y _) = BoolLit (x || y) p
eval (BoolLit x p) (Eq _ ) (BoolLit y _) = BoolLit (x == y) p
eval (BoolLit x p) (Diff _ ) (BoolLit y _) = BoolLit (x /= y) p

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

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
                               if id1 == id2 then (Id id1 p2, v1) : t
                               else (Id id2 p2, v2) : symtable_update (Id id1 p1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "soma.ccr")) of
            { Left err -> print err; 
              Right ans -> print ans
            }

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module ExpressionsUtil where

import Data.Char
import Data.List

shuntingYard' :: [Char] -> [Char] -> [Char] -> [Char]

-- when all lists but output empty, reverse output
shuntingYard' [] [] outQueue = reverse outQueue

-- when no more input tokens but still has operator tokens, add them to the output queue
shuntingYard' [] (op:ops) outQueue = shuntingYard' [] ops (op:outQueue)

-- | parsing input tokens
shuntingYard' (token:tokens) opStack outQueue

    -- if the token is a digit then put it on the output queue
    | isDigit token = shuntingYard' tokens opStack (token:outQueue)

    -- if the token is an operator
    | isOperator token = case opStack of

        -- if operator stack is empty then add the operator to the stack
        [] -> shuntingYard' tokens (token:opStack) outQueue

        -- if the operator stack isn't empty compare precedence with operator on top of opStack 
        (op2:ops) -> if ((isLeftAssociativeOperator token) && ((operatorPrecedence token) <= (operatorPrecedence op2))) || ((isRightAssociativeOperator token) && ((operatorPrecedence token) < (operatorPrecedence op2)))
                     then shuntingYard' (token:tokens) ops (op2:outQueue)
                     else shuntingYard' tokens (token:opStack) outQueue

    -- if token is left bracket  then push onto operator stack
    | isLeftParenthesis token = shuntingYard' tokens (token:opStack) outQueue

    -- if token is right bracket then check operator stack 
    | isRightParenthesis token = case opStack of

        -- if top of operator stack is left bracket then just pop
        ('(':ops) -> shuntingYard' tokens ops outQueue

        -- pop from operator stack and push into output queue till reach left bracket
        (op:ops) -> shuntingYard' (token:tokens) ops (op:outQueue)

-- | calls the helper shunting yard function to do the calucations
shuntingYard :: [Char] -> [Char]
shuntingYard tokens = shuntingYard' tokens [] []


-- | evaluates an expression in Reverse Polish Notation using a left fold
evalPostfix :: [Char] -> Int
evalPostfix ex = head (foldl func [] ex)
    where func (x:y:xs) '+' = (x + y):xs
          func (x:y:xs) '-' = (y - x):xs
          func (x:y:xs) '*' = (x * y):xs
          func (x:y:xs) '/' = (y `div` x):xs
          func (x:y:xs) '^' = (y ^ x):xs                              
          func xs digit = (digitToInt digit):xs


-- accepts on character and returs true iff it's an operator (ie + - / * ^)
isOperator :: Char -> Bool
isOperator '+' = True 
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator '^' = True
isOperator _ = False

-- accepts on character and returs true iff it's an operator that is left-associative (ie + - * /)
isLeftAssociativeOperator :: Char -> Bool
isLeftAssociativeOperator '+' = True
isLeftAssociativeOperator '-' = True
isLeftAssociativeOperator '*' = True
isLeftAssociativeOperator '/' = True
isLeftAssociativeOperator _ = False

-- accepts one character and returs true iff it's an operator that is right=associative (ie ^)
isRightAssociativeOperator :: Char -> Bool
isRightAssociativeOperator '^' = True
isRightAssociativeOperator _ = False

-- accepts one character and returns it's precedence (2 for + and -, 3 for * and /, 4 for ^)
operatorPrecedence :: Char -> Int
operatorPrecedence '+' = 2
operatorPrecedence '-' = 2
operatorPrecedence '*' = 3
operatorPrecedence '/' = 3
operatorPrecedence '^' = 4
operatorPrecedence _ = 0

-- accepts one character and returns true if character is left brack
isLeftParenthesis :: Char -> Bool
isLeftParenthesis '(' = True
isLeftParenthesis _ = False

-- accepts one character and returns true if character is right bracket
isRightParenthesis :: Char -> Bool
isRightParenthesis ')' = True
isRightParenthesis _ = False
-- accepts one character and returns true if character either left bracket or right bracket
isParenthesis :: Char -> Bool
isParenthesis ')' = True
isParenthesis '(' = True
isParenthesis _ = False
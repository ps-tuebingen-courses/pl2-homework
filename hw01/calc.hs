module Main where

data Expr = Literal Int | Plus Expr Expr


eval :: Expr -> Int
eval (Literal i) = i 
eval (Plus e1 e2) = (eval e1) + (eval e2)

main :: IO ()
main = print $ eval (Plus (Literal 2) (Literal 3))
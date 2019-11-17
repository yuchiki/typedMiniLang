module Main where

import Expr
import Parser
import Eval
import SimpleType
import SimpleTyping
import System.IO
import Control.Monad

main :: IO ()
main = repl

repl :: IO ()
repl = do
    parseInput
    repl


parseInput :: IO ()
parseInput = do
    putStr ">"
    hFlush stdout
    stringExpr <- getLine
    unless (null stringExpr) $ do
        -- putStrLn "----------- parse ------------"
        putStr ""
        case parseExpr stringExpr of
            Left s -> do
                putStrLn s
                return ()
            Right e -> do
                -- print e
                putStr ""
                getEquations e

getEquations :: Expr -> IO ()
getEquations e = do
    putStrLn "---------- extract -----------"
    case extract e of
        Nothing -> do
            putStrLn "fail to extract equations."
            return ()
        Just (eqs, t) -> do
            putStrLn "equations:"
            putStr $ showTypeEquations eqs
            putStrLn ""
            putStrLn $ "type: " ++ show t
            getSubstitution e eqs t

getSubstitution :: Expr -> [TypeEquation] -> SimpleType -> IO ()
getSubstitution e eqs t = do
    putStrLn "---------- unify ----------"
    case unify eqs of
        Nothing -> do
            putStrLn "fail to unify equations."
            return ()
        Just substitution -> do
            putStrLn "substition:"
            putStr $ showSubstitution substitution
            putStrLn ""
            printExprAndType e t substitution

printExprAndType :: Expr -> SimpleType -> Substitution -> IO ()
printExprAndType e t substitution = do
    -- putStrLn $ "type: " ++ show e ++ " : " ++ show (substitute substitution t)
    putStrLn $ "type: " ++ show (substitute substitution t)
    putStrLn ""
    -- execute e

execute :: Expr -> IO ()
execute e = do
    putStrLn "---------- eval ----------"
    putStrLn "(結果省略)"
    -- putStrLn $ "result: " ++ show (eval e)

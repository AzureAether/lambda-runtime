module Lambda where

import Parser
import Lexer

main :: IO ()
main = do
    putStr ">>> "
    fileName <- getLine
    program <- readFile $ fileName
    print $ (parser.lexer) program
    putStrLn ""
    Lambda.main
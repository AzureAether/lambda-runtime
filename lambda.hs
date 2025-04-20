module Lambda where

import Parser
import Lexer
import Data.Map (Map)
import qualified Data.Map as Map

-- ******************** Program -> Lambda ********************

data Lambda = Var String | F String Lambda | Ap Lambda Lambda deriving Eq

instance Show Lambda where
  show (Var s) = s
  show (F s l) = "\\" ++ s ++ "." ++ show l
  show (Ap x y) = wrap (show x) ++ wrap (show y) where wrap s = if (length s == 1) then s else "(" ++ s ++ ")"

conv_to_lambda :: Map String Lambda_term -> [Line] -> [Lambda_term]
conv_to_lambda _ [] = []
conv_to_lambda dict (l:ls) =
  case l of (Assign s f) -> conv_to_lambda (Map.insert s f dict) ls
            (RunTerm x)  -> (conv_term x dict) : conv_to_lambda dict ls


conv_term :: Lambda_term -> Map String Lambda_term -> Lambda_term
conv_term l dict =
  case l of (Apply x y)          -> Apply (conv_term x dict) (conv_term y dict)
            (Func ps x)          -> Func ps (conv_term x new_dict)
                                      where new_dict = foldr f dict ps
                                            f p d      = Map.insert p (Variable (Str p)) d
            (Variable (Str v))   -> case (Map.lookup v dict) of (Just i) -> if i == (Variable (Str v)) then i else conv_term i dict
                                                                Nothing  -> (Variable (Str v))
            (Variable (Num n))   -> Variable (Num n)

num_to_lambda :: Int -> Lambda
num_to_lambda 0 = Var "x"
num_to_lambda n = Ap (Var "f") (num_to_lambda $ n-1)

-- ******************** end Program -> Lambda ********************

main :: IO ()
main = do
    putStr ">>> "
    fileName <- getLine
    program <- readFile $ fileName
    print $ (parser.lexer) program
    putStrLn ""
    Lambda.main
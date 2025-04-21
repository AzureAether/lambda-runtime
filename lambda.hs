module Lambda where

import Parser
import Lexer
import Data.Map (Map,(!))
import qualified Data.Map as Map
import System.IO

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

-- convert an integer to a Church numeral
num_to_lambda :: Int -> Lambda_term
num_to_lambda = Func ["f","x"] . num_to_grounded_church

num_to_grounded_church :: Int -> Lambda_term
num_to_grounded_church 0 = Variable (Str "x")
num_to_grounded_church n = Apply (Variable $ Str "f") (num_to_lambda $ n-1)

-- ******************** end Program -> Lambda ********************

-- Convert a lambda term with variables into one with de Bruijn
-- indices, EG \xy.x -> \\1
deBruijnString :: Lambda_term -> String
deBruijnString = deBruijnIndices Map.empty

deBruijnIndices :: Map String Int -> Lambda_term -> String
deBruijnIndices indices (Variable (Str s)) = if Map.member s indices then show (indices ! s) else "("++s++")"
deBruijnIndices indices (Variable (Num n)) = deBruijnIndices indices $ num_to_lambda n
deBruijnIndices indices (Apply t1 t2) = '+' : deBruijnIndices indices t1 ++ deBruijnIndices indices t2
deBruijnIndices indices (Func [s] t) = '\\' : deBruijnIndices (bindVariable s indices) t
deBruijnIndices indices (Func (s:ss) t) = '\\' : deBruijnIndices indices' (Func ss t)
    where indices' = (bindVariable s indices)

bindVariable :: String -> Map String Int -> Map String Int
bindVariable s indices = Map.insert s 1 (Map.map (+1) indices)

main :: IO ()
main = do
    putStrLn "Welcome to the interactive lambda-runtime command line tool!"
    putStrLn "To get started, try running the 'help' command."
    mainloop

mainloop :: IO ()
mainloop = do
    putStr ">>> "
    hFlush stdout
    input <- getLine
    case takeWhile (/=' ') input of
        "help" -> runHelp
        "show" -> runShow $ tail $ dropWhile (/=' ') input

runHelp :: IO ()
runHelp = do
    putStrLn "help            - list all commands"
    putStrLn "show [fileName] - show the de Bruijn forms of run terms in a lambda file"
    putStrLn ""
    mainloop

runShow :: String -> IO ()
runShow fileName = do
    program <- readFile $ fileName
    putStrLn $ unlines.map deBruijnString $ conv_to_lambda Map.empty ((parser.lexer) program)
    mainloop
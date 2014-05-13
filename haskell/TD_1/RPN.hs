module RPN where

import Prelude
import System.IO

type Stack  = [Int]

type Operator = Stack -> Stack

binOp :: (a -> a -> a) -> [a] -> [a]
binOp op (b : a : xs) =  a `op` b : xs

-- Parse function
parseOp :: String -> Operator
parseOp "+"     = binOp (+)
parseOp "-"     = binOp (-)
parseOp "/"     = binOp div
parseOp "*"     = binOp (*)
parseOp "dup"   = \(x : xs) -> (x : x : xs)
parseOp "swap"  = \(x : y : xs) -> (y : x : xs)
parseOp "drop"  = tail
parseOp "depth" = \s -> toEnum(length s) : s
parseOp "pick"  = \(x : s) -> s !! fromEnum x : s
parseOp x       = (read x :)

-- eval
eval :: a -> [a -> a] -> a
eval a [] = a
eval a (op : xs) = eval (op a)  xs

-- parse
parse :: String -> [Operator]
parse = (map parseOp) . words

-- repl
repl :: Stack -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack
main = repl []

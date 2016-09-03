{-# LANGUAGE BangPatterns #-}

module CodeGenerator where

import Common
import Parser

import           Data.Maybe
import           Debug.Trace
import System.IO (Handle, hSetEncoding, stdout, utf8)
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec 
import Text.Format

exp_gen :: Exp -> Maybe String
exp_gen (BinOpExp op e1 e2) = do
    s1 <- exp_gen e1
    sop <- op_gen op
    s2 <- exp_gen e2
    return (format "({0} {1} {2})" [s1, sop, s2])
exp_gen (ConstExp val) = do
    case val of 
        IntVal int_val -> return (show int_val)
        FloatVal float_val -> return (show float_val)

op_gen :: BinOp -> Maybe String
op_gen op = do
    case op of 
        "add" -> return "+"
        "sub" -> return "-"
        "mul" -> return "*"
        "div" -> return "/"
        _     -> Nothing

main_gen :: String -> IO ()
main_gen moduleName = do
    context <- C.readFile (moduleName ++ ".s")
    if C.null context
    then putStrLn $ moduleName ++ " is not exist"
    else case (parse p_exp "" (C.unpack context)) of
        (Right exp) -> case (exp_gen exp) of 
            (Just output) -> putStrLn $ show output
        err -> do
            putStrLn "compile error:"
            putStrLn $ show (err)

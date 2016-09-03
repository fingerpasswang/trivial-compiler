{-# LANGUAGE BangPatterns #-}

module Parser where

import Common
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Char
import Numeric (readHex, readDec, readSigned, readFloat, readInt)
import Control.Monad (liftM2, liftM3, liftM4)
import System.IO (Handle, hSetEncoding, stdout, utf8)
import Data.Char(isLower, isUpper, isAlpha, isAlphaNum)

import qualified Data.ByteString.Char8 as C
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)

-- 辅助用 begin
listplus :: [Parser a] -> Parser a
listplus lst = foldr (<|>) mzero (map try lst)

p_parse   :: Parser a -> Parser a
p_parse p =  spaces *> p

p_between :: Char -> Char -> Parser a -> Parser a
p_between l r p =   char l 
                *>  (spaces *> p <* spaces)
                <*  char r
                <?> "p_between"

p_int :: Parser Integer
p_int  = do s <- getInput
            case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
        <?> "p_int"

p_float :: Parser Float
p_float  = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
        <?> "p_float"

-- 辅助用 end

-- val begin
p_int_val :: Parser Val
p_int_val =  IntVal <$> p_int 
            <?> "p_int_val"

p_float_val :: Parser Val
p_float_val =  FloatVal <$> p_float 
            <?> "p_float_val"

p_val :: Parser Val
p_val =  listplus [p_int_val,p_float_val]
-- val end

-- exp begin	 
p_const_exp :: Parser Exp
p_const_exp =  ConstExp <$> p_parse p_val
            <?> "p_const_exp"

p_bin_op_exp :: Parser Exp
p_bin_op_exp =  p_between '(' ')' inner <?> "p_bin_op_exp"
    where
        inner = BinOpExp
                <$> p_parse (listplus [string "add", string "sub", string "mul", string "div"])
                <*> p_parse p_exp
                <*> p_parse p_exp
                <?> "p_bin_op_exp_inner"

p_exp :: Parser Exp
p_exp =  listplus [p_const_exp, p_bin_op_exp]
        <?> "p_exp"
-- exp end

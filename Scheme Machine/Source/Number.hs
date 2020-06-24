module Number (
    Number(..),
    parseNumber,
) where

import Data.Char (ord)
import Data.List (foldl')
import Control.Monad (liftM)
import qualified Data.Complex as C
import qualified Data.Ratio as R
import Text.Parsec

data Number
    = CxRat (C.Complex Rational)
    | CxReal (C.Complex Double)
    deriving (Eq)

showrat :: Rational -> String
showrat x
    = case (R.numerator x, R.denominator x) of
        (0, 0) -> "NaN"
        (1, 0) -> "Infinity"
        (-1, 0) -> "-Infinity"
        (num, 1) -> show num
        (num, den) -> show num ++ "/" ++ show den

shownumber :: Number -> String
shownumber (CxRat (re C.:+ im))
    | im == 0 = showrat re
    | otherwise = showrat re ++ "@" ++ showrat im
shownumber (CxReal (re C.:+ im))
    | im == 0 && not (isNegativeZero im) = show re
    | otherwise = show re ++ "@" ++ show im

instance Show Number where
    show n = shownumber n

type Parser = Parsec [Char] ()

readExChar :: Parser Char
readExChar
    = (char 'e' >> return 'e')
    <|> (char 'E' >> return 'e')
    <|> (char 'i' >> return 'i')
    <|> (char 'I' >> return 'i')

readExPrefix :: Parser Char
readExPrefix = (char '#' >> readExChar) <|> return '.'

readRadixChar :: Parser Int
readRadixChar
    = (char 'b' >> return 2)
    <|> (char 'B' >> return 2)
    <|> (char 'o' >> return 8)
    <|> (char 'O' >> return 8)
    <|> (char 'd' >> return 10)
    <|> (char 'D' >> return 10)
    <|> (char 'x' >> return 16)
    <|> (char 'X' >> return 16)

readRadixPrefix :: Parser Int
readRadixPrefix = (char '#' >> readRadixChar) <|> return 10

readPrefixGiven :: Char -> Parser (Char, Int)
readPrefixGiven 'e' = liftM (\x -> ('e', x)) readRadixPrefix
readPrefixGiven 'E' = liftM (\x -> ('e', x)) readRadixPrefix
readPrefixGiven 'i' = liftM (\x -> ('i', x)) readRadixPrefix
readPrefixGiven 'I' = liftM (\x -> ('i', x)) readRadixPrefix
readPrefixGiven 'b' = liftM (\x -> (x, 2)) readExPrefix
readPrefixGiven 'B' = liftM (\x -> (x, 2)) readExPrefix
readPrefixGiven 'o' = liftM (\x -> (x, 8)) readExPrefix
readPrefixGiven 'O' = liftM (\x -> (x, 8)) readExPrefix
readPrefixGiven 'd' = liftM (\x -> (x, 10)) readExPrefix
readPrefixGiven 'D' = liftM (\x -> (x, 10)) readExPrefix
readPrefixGiven 'x' = liftM (\x -> (x, 16)) readExPrefix
readPrefixGiven 'X' = liftM (\x -> (x, 16)) readExPrefix

readPrefix' :: Parser (Char, Int)
readPrefix' = char '#' >> anyChar >>= readPrefixGiven

readPrefix :: Parser (Char, Int)
readPrefix = readPrefix' <|> return ('.', 10)

readSign' :: Parser Integer
readSign' = (char '-' >> return (-1)) <|> (char '+' >> return 1)

readSign :: Parser Integer
readSign = readSign' <|> return 1

charToDigit :: Char -> Integer
charToDigit ch
    | '0' <= ch && ch <= '9' = toInteger $ ord ch - ord '0'
    | 'a' <= ch && ch <= 'f' = toInteger $ ord ch - (ord 'a' - 10)
    | 'A' <= ch && ch <= 'F' = toInteger $ ord ch - (ord 'A' - 10)

readDigit :: Int -> Parser Integer
readDigit 2 = liftM charToDigit $ oneOf "01"
readDigit 8 = liftM charToDigit $ satisfy (\x -> '0' <= x && x <= '7')
readDigit 10 = liftM charToDigit $ satisfy (\x -> '0' <= x && x <= '9')
readDigit 16 = liftM charToDigit $ satisfy (\x -> False
    || '0' <= x && x <= '9'
    || 'a' <= x && x <= 'f'
    || 'A' <= x && x <= 'F')

-- given digits 'ds = [d1, d2, d3..dn]':
-- 'fst (readInteger ds)' gives the integer "d1 d2 d3..dn"
-- 'snd (readInteger ds)' gives 'base^n'
-- 'let (n,d) = readInteger ds in n/d' given the fraction "0.d1 d2 d3..dn"
readInteger' :: Int -> Parser (Integer, Integer)
readInteger' base = do
    let ibase = toInteger base
    digits <- many1 (readDigit base)
    return $ foldl'
        (\(x,y) d -> (x * ibase + toInteger d, y * ibase))
        (0, 1)
        digits

readInteger :: Int -> Parser (Integer, Integer)
readInteger base = readInteger' base <|> return (0, 1)

readBlank :: Int -> Parser (Bool, Integer)
readBlank base = do
    let ibase = toInteger base
    digits <- many (char '#')
    return $ foldl'
        (\(_,y) _ -> (False, y * ibase))
        (True, 1)
        digits

-- bool element tells if the number is exact
readIntegerBlank' :: Int -> Parser (Bool, Integer)
readIntegerBlank' base = do
    (left, _) <- readInteger' base
    (ex, scale) <- readBlank base
    return $ (ex, left * scale)

-- positive exponent "e+dd" is returned as (_, base^dd, 1)
-- negative exponent "e-dd" is returned as (_, 1, base^dd)
-- bool element tells if the number is exact
readExponent' :: Int -> Parser (Bool, (Integer, Integer))
readExponent' base = do
    oneOf (marker base)
    sign <- readSign
    (val, _) <- readInteger' 10
    if sign > 0
        then return $ (False, (ebase base ^ val, 1))
        else return $ (False, (1, ebase base ^ val))
    where
    marker 10 = "esfdlESFDL"
    marker _ = "pP"
    ebase 10 = 10
    ebase _ = 2

readExponent :: Int -> Parser (Bool, (Integer, Integer))
readExponent base = readExponent' base <|> return (True, (1, 1))

readRealLeadingDigit :: Int -> Parser (Bool, (Integer, Integer))
readRealLeadingDigit base = do
    (nex, num) <- readIntegerBlank' base
    choice [
        do
            char '.'
            (fnum, fden) <- if nex
                then do
                    frac <- readInteger base
                    readBlank base
                    return $ frac
                else do
                    readBlank base
                    return $ (0, 1)
            (_, (enum, eden)) <- readExponent base
            return $ (False, ((num * fden + fnum) * enum, fden * eden)),
        do
            char '/'
            (dex, den) <- readIntegerBlank' base
            return $ (nex && dex, (num, den)),
        do
            (eex, (enum, eden)) <- readExponent base
            return $ (nex && eex, (num * enum, eden))]

readRealLeadingDot :: Int -> Parser (Bool, (Integer, Integer))
readRealLeadingDot base = do
    char '.'
    (fnum, fden) <- readInteger' base
    readBlank base
    (_, (enum, eden)) <- readExponent base
    return $ (False, (fnum * enum, fden * eden))

-- result of '(_, (num, den))' represents a number 'num/den'
-- bool element tells if the number is exact
readReal :: Int -> Parser (Bool, (Integer, Integer))
readReal base = readRealLeadingDigit base <|> readRealLeadingDot base

-- result of '(_, (a,b), (c,d))' represents the complex number 'a/b + c/d I'
-- bool element tells if the number is exact
readComplex :: Int -> Parser (Bool, (Integer, Integer), (Integer, Integer))
readComplex base = do
    maybesign <- optionMaybe readSign'
    (signed, fsign) <- case maybesign of
        Nothing -> return $ (False, 1)
        Just s -> return $ (True, s)
    choice [
        do
            if signed
                then char 'i'
                else fail ""
            return $ (True, (0,1), (fsign,1)),
        do
            (fex, (fnum, fden)) <- readReal base
            choice [
                do
                    char 'i'
                    return $ (fex, (0,1), (fsign*fnum, fden)),
                do
                    char '@'
                    isign <- readSign
                    (iex, (inum, iden)) <- readReal base
                    return $ (
                        fex && iex,
                        (fsign*fnum, fden),
                        (isign*inum, iden)),
                do
                    isign <- readSign'
                    (iex, (inum,iden)) <- option (True, (1,1)) (readReal base)
                    char 'i'
                    return $ (
                        fex,
                        (fsign*fnum, fden),
                        (isign*inum, iden)),
                do
                    return $ (fex, (fsign*fnum, fden), (0,1))]]

readNumber :: Parser Number
readNumber = do
    (exprefix, base) <- readPrefix
    (exflag, (rnum, rden), (inum, iden)) <- readComplex base
    eof
    exact <- case exprefix of
        'e' -> return $ True
        'i' -> return $ False
        '.' -> return $ exflag
    let re = rnum R.% rden
    let im = inum R.% iden
    if exact
        then return $ CxRat (re C.:+ im)
        else return $ CxReal (fromRational re C.:+ fromRational im)

parseNumber :: [Char] -> Maybe Number
parseNumber s
    = case parse readNumber "" s of
        Left _ -> Nothing
        Right num -> Just num

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Lex (
    TokParser,
    isValidIdent,
    keywords,
    parseGrammar,
    tokEof,
    tokIdent,
    tokInt,
    tokKeyword,
    tokReal,
    tokString,
    tokSymbol,
) where


import Data.Bits
import Data.List (foldl')
import Data.Ratio
import Data.String
import Data.STRef
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Lua.SourceRange


data TokenData
    = TokIdent BSt.ByteString
    | TokKeyword BSt.ByteString
    | TokInt Integer
    | TokReal Rational
    | TokString BSt.ByteString
    | TokSymbol Int
    | TokEof
    deriving (Eq, Show)


newtype Token = Token (SourceRange, TokenData) deriving (Show)


showTokenType :: TokenData -> String
showTokenType (TokIdent _) = "identifier"
showTokenType (TokKeyword s) = show s
showTokenType (TokInt _) = "integer literal"
showTokenType (TokReal _) = "number literal"
showTokenType (TokString _) = "string literal"
showTokenType (TokSymbol c)
    | c < 0x100 = [
        '\"',
        toEnum c,
        '\"']
    | c < 0x10000 = [
        '\"',
        toEnum (c `mod` 0x100),
        toEnum (c `div` 0x100),
        '\"']
    | c < 0x1000000 = [
        '\"',
        toEnum (c `mod` 0x100),
        toEnum (c `div` 0x100 `mod` 0x100),
        toEnum (c `div` 0x10000),
        '\"']
showTokenType (TokEof) = "end of file"


within :: Char -> Char -> Char -> Bool
within a b x = a <= x && x <= b


type LParser s = ParsecT B.ByteString () (ST s)


lexWhitespace :: LParser s ()
lexWhitespace = do
    satisfy (\x -> within '\0' '\32' x)
    return ()


lexLongComment :: LParser s ()
lexLongComment = do
    char '['
    level <- ascend 0
    final <- optionMaybe (char '[')
    case final of
        Nothing -> lexLineComment
        Just _ -> inner level

    where

    ascend :: Int -> LParser s Int
    ascend n = do
        (char '=' >> ascend (n+1)) <|> return n

    inner :: Int -> LParser s ()
    inner n = do
        init <- optionMaybe (char ']')
        case init of
            Nothing -> anyChar >> inner n
            Just _ -> descend n n

    descend :: Int -> Int -> LParser s ()
    descend n 0 = do
        final <- optionMaybe (char ']')
        case final of
            Nothing -> inner n
            Just _ -> return ()
    descend n i = do
        (char '=' >> descend n (i-1)) <|> inner n


lexLineComment :: LParser s ()
lexLineComment = do
    (satisfy (\x -> x /= '\n' && x /= '\r') >> lexLineComment) <|> return ()


lexComment :: LParser s ()
lexComment = do
    try (string "--")
    lexLongComment <|> lexLineComment


lexSpace :: LParser s ()
lexSpace = do
    ((lexWhitespace <|> lexComment) >> lexSpace) <|> return ()


keywords :: IsString s => [s]
keywords = [
    "and",   "break", "do",       "else", "elseif", "end",
    "false", "for",   "function", "goto", "if",     "in",
    "local", "nil",   "not",      "or",   "repeat", "return",
    "then",  "true",  "until",    "while"]


lexWord :: LParser s TokenData
lexWord = do
    init <- satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || x == '_')
    rest <- many (satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || within '0' '9' x
        || x == '_'))
    let word = BSt.pack (init:rest)
    if word `elem` keywords
        then return $ TokKeyword word
        else return $ TokIdent word


lexNumber :: LParser s TokenData
lexNumber = do
    lookAhead (try (do
        optional (char '.')
        satisfy (\x -> within '0' '9' x)))
    prefix <- optionMaybe (try (char '0' >> oneOf "xX"))
    let base = numberBase prefix
    (inum, _) <- readInteger base
    frac <- optionMaybe (readFraction base)
    exp <- optionMaybe (readExponent base)
    notFollowedBy (satisfy (\x -> False
        || within '0' '9' x
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || x == '_'
        || x == '.'))
    case (frac, exp) of
        (Nothing, Nothing) ->
            return $ makeInt (base /= 10) inum
        (Just (fnum, fden), Nothing) ->
            return $ makeReal (inum * fden + fnum, fden)
        (Nothing, Just (enum, eden)) ->
            return $ makeReal (inum * enum, eden)
        (Just (fnum, fden), Just (enum, eden)) ->
            return $ makeReal ((inum * fden + fnum) * enum, fden * eden)

    where

    numberBase :: Maybe Char -> Int
    numberBase Nothing = 10
    numberBase (Just c)
        | c == 'x' || c == 'X' = 16

    readFraction :: Int -> LParser s (Integer, Integer)
    readFraction base = do
        char '.'
        readInteger base

    readExponent :: Int -> LParser s (Integer, Integer)
    readExponent 10 = readExponentGen "eE" 10
    readExponent 16 = readExponentGen "pP" 2

    readExponentGen :: [Char] -> Integer -> LParser s (Integer, Integer)
    readExponentGen echars ebase = do
        oneOf echars
        sign <- option ('+') (oneOf "+-")
        lookAhead (satisfy (\x -> within '0' '9' x))
        (val, _) <- readInteger 10
        if sign == '+'
            then return $ (ebase ^ val, 1)
            else return $ (1, ebase ^ val)

    readInteger :: Int -> LParser s (Integer, Integer)
    readInteger base = do
        readIntegerNext base (0, 1)

    readIntegerNext
        :: Int -> (Integer, Integer) -> LParser s (Integer, Integer)
    readIntegerNext base (num, den) = do
            (readDigit base (num, den) >>= readIntegerNext base)
        <|> return (num, den)

    readDigit
        :: Int -> (Integer, Integer) -> LParser s (Integer, Integer)
    readDigit 10 (num, den)
        = readDigitGen (num, den) 10 '0' '9' (fromEnum '0')
    readDigit 16 (num, den) = do
            readDigitGen (num, den) 16 '0' '9' (fromEnum '0')
        <|> readDigitGen (num, den) 16 'a' 'f' (fromEnum 'a' - 10)
        <|> readDigitGen (num, den) 16 'A' 'F' (fromEnum 'A' - 10)

    readDigitGen
        :: (Integer, Integer)
        -> Integer -> Char -> Char -> Int
        -> LParser s (Integer, Integer)
    readDigitGen (num, den) ibase firstch lastch choffset = do
        char <- satisfy (\x -> within firstch lastch x)
        let cval = toInteger (fromEnum char - choffset)
        return $ (num * ibase + cval, den * ibase)

    makeInt :: Bool -> Integer -> TokenData
    makeInt _ inum = do
        TokInt inum

    makeReal :: (Integer, Integer) -> TokenData
    makeReal (num, den) = do
        TokReal (num % den)


lexShortString :: LParser s TokenData
lexShortString = do
    init <- oneOf "'\""
    value <- readRest init
    return $ TokString (BSt.pack value)

    where

    readRest :: Char -> LParser s [Char]
    readRest init
        =   (satisfy (==init) >> return [])
        <|> (do
                left <- readEscape <|> readCh
                right <- readRest init
                return (left ++ right))

    readEscape :: LParser s [Char]
    readEscape = do
        char '\\'
        choice [
            char 'a' >> return "\7",
            char 'b' >> return "\8",
            char 'f' >> return "\12",
            char 'n' >> return "\10",
            char 'r' >> return "\13",
            char 't' >> return "\9",
            char 'v' >> return "\11",
            char 'z' >> escSpace,
            char 'x' >> escHex,
            char 'u' >> escUtf,
            escDec,
            escNewline,
            escSymbol]

    readCh :: LParser s [Char]
    readCh = do
        char <- satisfy (\x -> x /= '\n' && x /= '\r')
        return [char]

    escSpace :: LParser s [Char]
    escSpace = do
        (satisfy (\x -> within '\0' '\32' x) >> escSpace) <|> return []

    escHex :: LParser s [Char]
    escHex = do
        a <- hexDigit
        b <- hexDigit
        return $ [toEnum (16*a + b)]

    escUtf :: LParser s [Char]
    escUtf = do
        char '{'
        str <- many1 hexDigit
        char '}'
        let value = foldl' (\a d -> a * 16 + toInteger (fromEnum d)) 0 str
        encodeUtf8 value

    escDec :: LParser s [Char]
    escDec = do
        a <- decDigit
        mb <- optionMaybe decDigit
        mc <- optionMaybe decDigit
        case (mb, mc) of
            (Nothing, Nothing) -> return $ [toEnum a]
            (Just b, Nothing) -> return $ [toEnum (10*a + b)]
            (Just b, Just c) -> do
                let v = 100*a + 10*b + c
                if v < 0x100
                    then return $ [toEnum v]
                    else invalidCharcode

    escNewline :: LParser s [Char]
    escNewline = do
        init <- oneOf "\n\r"
        case init of
            '\n' -> optional (char '\r')
            '\r' -> optional (char '\n')
        return $ "\n"

    escSymbol :: LParser s [Char]
    escSymbol = do
        char <- satisfy (\x -> True
            && not (within 'a' 'z' x)
            && not (within 'A' 'Z' x)
            && x /= '_')
        return $ [char]

    decDigit :: LParser s Int
    decDigit = do
        genDigit '0' '9' (fromEnum '0')

    hexDigit :: LParser s Int
    hexDigit = do
            genDigit '0' '9' (fromEnum '0')
        <|> genDigit 'a' 'f' (fromEnum 'a' - 10)
        <|> genDigit 'A' 'F' (fromEnum 'A' - 10)

    genDigit :: Char -> Char -> Int -> LParser s Int
    genDigit firstch lastch choffset = do
        char <- satisfy (\x -> within firstch lastch x)
        return $ fromEnum char - choffset

    encodeUtf8 :: Integer -> LParser s [Char]
    encodeUtf8 i = do
        let d = fromInteger i
        if toInteger d == i
            then encodeUtf8' d
            else invalidCharcode

    encodeUtf8' :: Int -> LParser s [Char]
    encodeUtf8' d
        | d <= 0x7f = do
            return [
                toEnum d]
        | d <= 0x7ff = do
            return [
                toEnum (0xc0 .|.  shiftR d 6),
                toEnum (0x80 .|.  d           .&. 0x3f)]
        | d <= 0xffff = do
            return [
                toEnum (0xe0 .|.  shiftR d 12),
                toEnum (0x80 .|. (shiftR d 6  .&. 0x3f)),
                toEnum (0x80 .|.  d           .&. 0x3f)]
        | d <= 0x1fffff = do
            return [
                toEnum (0xf0 .|.  shiftR d 18),
                toEnum (0x80 .|. (shiftR d 12 .&. 0x3f)),
                toEnum (0x80 .|. (shiftR d 6  .&. 0x3f)),
                toEnum (0x80 .|.  d           .&. 0x3f)]
        | d <= 0x3ffffff = do
            return [
                toEnum (0xf8 .|.  shiftR d 24),
                toEnum (0x80 .|. (shiftR d 18 .&. 0x3f)),
                toEnum (0x80 .|. (shiftR d 12 .&. 0x3f)),
                toEnum (0x80 .|. (shiftR d 6  .&. 0x3f)),
                toEnum (0x80 .|.  d           .&. 0x3f)]
        | d <= 0x7fffffff = do
            return [
                toEnum (0xfc .|.  shiftR d 30),
                toEnum (0x80 .|. (shiftR d 24 .&. 0x3f)),
                toEnum (0x80 .|. (shiftR d 18 .&. 0x3f)),
                toEnum (0x80 .|. (shiftR d 12 .&. 0x3f)),
                toEnum (0x80 .|. (shiftR d 6  .&. 0x3f)),
                toEnum (0x80 .|.  d           .&. 0x3f)]
        | otherwise = invalidCharcode

    invalidCharcode :: LParser s a
    invalidCharcode = fail "Invalid charcode"


lexLongString :: LParser s TokenData
lexLongString = do
    try (char '[' >> lookAhead (oneOf "[="))
    level <- ascend 0
    char '['
    cont <- inner level
    return $ TokString (BSt.pack cont)

    where

    ascend :: Int -> LParser s Int
    ascend n = do
        (char '=' >> ascend (n+1)) <|> return n

    inner :: Int -> LParser s [Char]
    inner n = do
        init <- optionMaybe (char ']')
        case init of
            Nothing -> do
                ch <- anyChar
                case ch of
                    '\n' -> optional (char '\r')
                    '\r' -> optional (char '\n')
                    _ -> return ()
                rest <- inner n
                let tch = if ch /= '\r'
                    then ch
                    else '\n'
                return $ tch:rest
            Just _ -> descend n n

    descend :: Int -> Int -> LParser s [Char]
    descend n 0 = do
        final <- optionMaybe (char ']')
        case final of
            Nothing -> do
                let head = replicate n '='
                rest <- inner n
                return $ ']':(head ++ rest)
            Just _ -> return $ []
    descend n i = do
        next <- optionMaybe (char '=')
        case next of
            Nothing -> do
                let head = replicate (n-i) '='
                rest <- inner n
                return $ ']':(head ++ rest)
            Just _ -> descend n (i-1)


lexSymbol :: LParser s TokenData
lexSymbol = do
    a <- oneOf "+-*/%^#&~|<>=(){}[];:,."
    given1 a

    where

    given1 :: Char -> LParser s TokenData
    given1 a
        | a == '/' = try2 a "/"
        | a == ':' = try2 a ":"
        | a == '<' = try2 a "<="
        | a == '=' = try2 a "="
        | a == '>' = try2 a ">="
        | a == '~' = try2 a "="
        | a == '.' = do
            mb <- optionMaybe (char '.')
            mc <- optionMaybe (char '.')
            case (mb, mc) of
                (Nothing, Nothing) -> return $ TokSymbol (fromEnum a)
                (Just _, Nothing) -> return $ TokSymbol (fromEnum a * 0x101)
                (Just _, Just _) -> return $ TokSymbol (fromEnum a * 0x10101)
        | otherwise = return $ TokSymbol (fromEnum a)

    try2 :: Char -> [Char] -> LParser s TokenData
    try2 a nexts = do
        mb <- optionMaybe (oneOf nexts)
        case mb of
            Nothing -> return $ TokSymbol (fromEnum a)
            Just b -> return $ TokSymbol (fromEnum a + fromEnum b * 0x100)


lexToken :: LParser s Token
lexToken = do
    spos <- getPosition
    tokdata <- choice [
        lexWord,
        lexNumber,
        lexShortString,
        lexLongString,
        lexSymbol,
        eof >> return TokEof]
    epos <- getPosition
    return $ Token (newRange spos epos, tokdata)


newtype Cell s = Cell (STRef s (Maybe (Token, Cell s)))


fetch :: Cell s -> LParser s (Token, Cell s)
fetch (Cell ref) = do
    cv <- lift (readSTRef ref)
    case cv of
        Just (tok, next) -> return $ (tok, next)
        Nothing -> do
            lexSpace
            tok <- lexToken
            nref <- lift (newSTRef Nothing)
            let result = (tok, Cell nref)
            lift (writeSTRef ref (Just result))
            return $ result


newtype TokenStream s = TokenStream (Cell s, SourcePos)


instance Stream (TokenStream s) (LParser s) Token where
    uncons (TokenStream (cell, _)) = do
        (tok, next) <- fetch cell
        (Token (pr, _), _) <- fetch next
        return $ Just (tok, TokenStream (next, rangeLeft pr))


type TokParser s = ParsecT (TokenStream s) () (LParser s)


tokPrim :: (Token -> Maybe a) -> TokParser s a
tokPrim f = do
    tokenPrim
        (\(Token (_, td)) -> showTokenType td)
        (\_ _ (TokenStream (_, npos)) -> npos)
        f


lexGrammar :: TokParser s a -> SourceName -> LParser s (Either ParseError a)
lexGrammar gram name = do
    init <- lift (newSTRef Nothing)
    (Token (pr, _), next) <- fetch (Cell init)
    let ipos = rangeLeft pr
    runParserT gram () name (TokenStream (Cell init, ipos))


parseGrammarST
    :: TokParser s a
    -> SourceName
    -> B.ByteString
    -> ST s (Either ParseError a)
parseGrammarST gram name input = do
    parseret <- runParserT (lexGrammar gram name) () name input
    return $ join parseret


parseGrammar
    :: (forall s . TokParser s a)
    -> SourceName
    -> B.ByteString
    -> Either ParseError a
parseGrammar gram name input = runST (parseGrammarST gram name input)


tokIdent :: TokParser s (SourceRange, BSt.ByteString)
tokIdent = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokIdent w -> Just (pr, w)
                _ -> Nothing)
    <?> "identifier"


tokKeyword
    :: BSt.ByteString -> TokParser s SourceRange
tokKeyword kw = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokKeyword w ->
                    if w == kw
                        then Just pr
                        else Nothing
                _ -> Nothing)
    <?> "\"" ++ BSt.unpack kw ++ "\""


tokInt :: TokParser s (SourceRange, Integer)
tokInt = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokInt n -> Just (pr, n)
                _ -> Nothing)
    <?> "integer"


tokReal :: TokParser s (SourceRange, Rational)
tokReal = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokReal n -> Just (pr, n)
                _ -> Nothing)
    <?> "number"


tokString :: TokParser s (SourceRange, BSt.ByteString)
tokString = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokString s -> Just (pr, s)
                _ -> Nothing)
    <?> "string"


tokSymbolId :: Int -> TokParser s SourceRange
tokSymbolId ref = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokSymbol i ->
                    if i == ref
                        then Just pr
                        else Nothing
                _ -> Nothing)


tokSymbol :: String -> TokParser s SourceRange
tokSymbol s@[ca] = do
    tokSymbolId
        (fromEnum ca)
    <?> "\"" ++ s ++ "\""
tokSymbol s@[ca,cb] = do
    tokSymbolId
        (fromEnum ca + fromEnum cb * 0x100)
    <?> "\"" ++ s ++ "\""
tokSymbol s@[ca,cb,cc] = do
    tokSymbolId
        (fromEnum ca + fromEnum cb * 0x100 + fromEnum cc * 0x10000)
    <?> "\"" ++ s ++ "\""
tokSymbol s = do
    tokPrim
        (\_ -> Nothing)
    <?> "\"" ++ s ++ "\""


tokEof :: TokParser s SourceRange
tokEof = do
    tokPrim
        (\(Token (pr, td)) ->
            case td of
                TokEof -> Just pr
                _ -> Nothing)
    <?> "end of file"


isValidIdent :: BSt.ByteString -> Bool
isValidIdent name = do
    checkFirst (BSt.unpack name)
    where
    checkFirst (x:xs)
        = True
            && (False
                || within 'a' 'z' x
                || within 'A' 'Z' x
                || x == '_')
            && checkRest xs
    checkFirst _ = False
    checkRest [] = True
    checkRest (x:xs)
        = True
            && (False
                || within 'a' 'z' x
                || within 'A' 'Z' x
                || within '0' '9' x
                || x == '_')
            && checkRest xs

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Parse (
    IrSlot(..),
    IrValue(..),
    IrList(..),
    IrSink(..),
    IrAction(..),
    IrBody,
    translateLua,
) where


import Data.Bits
import Data.Function
import Data.Int
import Data.List (foldl')
import Data.Maybe
import Data.Ratio
import Data.String
import Data.STRef
import Data.Word
import qualified Data.ByteString as BSt
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (State)
import System.Mem.StableName
import SourceRange


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


byte = toEnum . fromEnum :: Char -> Word8
unbyte = toEnum . fromEnum :: Word8 -> Char


within :: Char -> Char -> Word8 -> Bool
within a b x = do
    let ia = byte a
    let ib = byte b
    ia <= x && x <= ib


type LParser s = ParsecT ByteSource () (ST s)


newtype ByteSource = ByteSource B.ByteString deriving (Show)


instance (Monad m) => Stream ByteSource m Word8 where
    uncons (ByteSource str) = do
        case B.uncons str of
            Nothing -> return $ Nothing
            Just (byte, rest) -> return $ Just (byte, ByteSource rest)


instance IsString ByteSource where
    fromString s = ByteSource (fromString s)


bytePrim :: (Word8 -> Maybe a) -> LParser s a
bytePrim = tokenPrim
    (\i -> show (unbyte i))
    (\pos i _cs -> updatePosChar pos (unbyte i))


satisfy :: (Word8 -> Bool) -> LParser s Word8
satisfy f = bytePrim (\i -> if f i then Just i else Nothing)


char :: Char -> LParser s Word8
char ch = satisfy (\i -> i == byte ch)


oneOf :: [Char] -> LParser s Word8
oneOf cs = satisfy (\i -> i `elem` (map byte cs))


anyChar :: LParser s Word8
anyChar = bytePrim (\i -> Just i)


string :: [Char] -> LParser s ()
string [] = return ()
string (x:xs) = char x >> string xs


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
    (satisfy (\x -> x /= 10 && x /= 13) >> lexLineComment) <|> return ()


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
        || x == byte '_')
    rest <- many (satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || within '0' '9' x
        || x == byte '_'))
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
        || x == byte '_'
        || x == byte '.'))
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

    numberBase :: Maybe Word8 -> Int
    numberBase Nothing = 10
    numberBase (Just c)
        | c == byte 'x' || c == byte 'X' = 16

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
        sign <- option (byte '+') (oneOf "+-")
        lookAhead (satisfy (\x -> within '0' '9' x))
        (val, _) <- readInteger 10
        if sign == byte '+'
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
        = readDigitGen (num, den) 10 '0' '9' (byte '0')
    readDigit 16 (num, den) = do
            readDigitGen (num, den) 16 '0' '9' (byte '0')
        <|> readDigitGen (num, den) 16 'a' 'f' (byte 'a' - 10)
        <|> readDigitGen (num, den) 16 'A' 'F' (byte 'A' - 10)

    readDigitGen
        :: (Integer, Integer)
        -> Integer -> Char -> Char -> Word8
        -> LParser s (Integer, Integer)
    readDigitGen (num, den) ibase firstch lastch choffset = do
        char <- satisfy (\x -> within firstch lastch x)
        let cval = toInteger (char - choffset)
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

    readRest :: Word8 -> LParser s [Word8]
    readRest init
        =   (satisfy (==init) >> return [])
        <|> (do
                left <- readEscape <|> readCh
                right <- readRest init
                return (left ++ right))

    readEscape :: LParser s [Word8]
    readEscape = do
        char '\\'
        choice [
            char 'a' >> return [7],
            char 'b' >> return [8],
            char 'f' >> return [12],
            char 'n' >> return [10],
            char 'r' >> return [13],
            char 't' >> return [9],
            char 'v' >> return [11],
            char 'z' >> escSpace,
            char 'x' >> escHex,
            char 'u' >> escUtf,
            escDec,
            escNewline,
            escSymbol]

    readCh :: LParser s [Word8]
    readCh = do
        char <- satisfy (\x -> x /= 10 && x /= 13)
        return [char]

    escSpace :: LParser s [Word8]
    escSpace = do
        (satisfy (\x -> within '\0' '\32' x) >> escSpace) <|> return []

    escHex :: LParser s [Word8]
    escHex = do
        a <- hexDigit
        b <- hexDigit
        return $ [toEnum (16*a + b)]

    escUtf :: LParser s [Word8]
    escUtf = do
        char '{'
        str <- many1 hexDigit
        char '}'
        let value = foldl' (\a d -> a * 16 + toInteger (fromEnum d)) 0 str
        encodeUtf8 value

    escDec :: LParser s [Word8]
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

    escNewline :: LParser s [Word8]
    escNewline = do
        init <- oneOf "\10\13"
        case init of
            10 -> optional (char '\13')
            13 -> optional (char '\10')
        return $ [10]

    escSymbol :: LParser s [Word8]
    escSymbol = do
        char <- satisfy (\x -> True
            && not (within 'a' 'z' x)
            && not (within 'A' 'Z' x)
            && x /= byte '_')
        return $ [char]

    decDigit :: LParser s Int
    decDigit = do
        genDigit '0' '9' (byte '0')

    hexDigit :: LParser s Int
    hexDigit = do
            genDigit '0' '9' (byte '0')
        <|> genDigit 'a' 'f' (byte 'a' - 10)
        <|> genDigit 'A' 'F' (byte 'A' - 10)

    genDigit :: Char -> Char -> Word8 -> LParser s Int
    genDigit firstch lastch choffset = do
        char <- satisfy (\x -> within firstch lastch x)
        return $ fromEnum (char - choffset)

    encodeUtf8 :: Integer -> LParser s [Word8]
    encodeUtf8 i = do
        let d = fromInteger i
        if toInteger d == i
            then encodeUtf8' d
            else invalidCharcode

    encodeUtf8' :: Int -> LParser s [Word8]
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

    inner :: Int -> LParser s [Word8]
    inner n = do
        init <- optionMaybe (char ']')
        case init of
            Nothing -> do
                ch <- anyChar
                case ch of
                    10 -> optional (char '\13')
                    13 -> optional (char '\10')
                    _ -> return ()
                rest <- inner n
                let tch = if ch /= 13
                    then ch
                    else 10
                return $ tch:rest
            Just _ -> descend n n

    descend :: Int -> Int -> LParser s [Word8]
    descend n 0 = do
        final <- optionMaybe (char ']')
        case final of
            Nothing -> do
                let head = replicate n (byte '=')
                rest <- inner n
                return $ byte ']':(head ++ rest)
            Just _ -> return $ []
    descend n i = do
        next <- optionMaybe (char '=')
        case next of
            Nothing -> do
                let head = replicate (n-i) (byte '=')
                rest <- inner n
                return $ byte ']':(head ++ rest)
            Just _ -> descend n (i-1)


lexSymbol :: LParser s TokenData
lexSymbol = do
    a <- oneOf "+-*/%^#&~|<>=(){}[];:,."
    given1 a

    where

    given1 :: Word8 -> LParser s TokenData
    given1 a
        | a == byte '/' = try2 a "/"
        | a == byte ':' = try2 a ":"
        | a == byte '<' = try2 a "<="
        | a == byte '=' = try2 a "="
        | a == byte '>' = try2 a ">="
        | a == byte '~' = try2 a "="
        | a == byte '.' = do
            mb <- optionMaybe (char '.')
            mc <- optionMaybe (char '.')
            case (mb, mc) of
                (Nothing, Nothing) -> return $ TokSymbol (fromEnum a)
                (Just _, Nothing) -> return $ TokSymbol (fromEnum a * 0x101)
                (Just _, Just _) -> return $ TokSymbol (fromEnum a * 0x10101)
        | otherwise = return $ TokSymbol (fromEnum a)

    try2 :: Word8 -> [Char] -> LParser s TokenData
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
    parseret <- runParserT (lexGrammar gram name) () name (ByteSource input)
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
    <?> "\"" ++ map unbyte (BSt.unpack kw) ++ "\""


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


class DefString a where
    defString :: Int -> a -> ShowS


defBr :: Int -> ShowS
defBr d rest = ('\n' : replicate (d*4) ' ') ++ rest


defList :: DefString a => ShowS -> Int -> [a] -> ShowS
defList sep d [] rest = rest
defList sep d [x] rest = defString d x $ rest
defList sep d (x:y:ys) rest = defString d x $ sep $ defList sep d (y:ys) $ rest


instance IsString ShowS where
    fromString s rest = s ++ rest


unpackSt :: BSt.ByteString -> ShowS
unpackSt s rest = map unbyte (BSt.unpack s) ++ rest


data NameNode = NameNode (SourceRange, BSt.ByteString) deriving (Show)


instance DefString NameNode where
    defString d (NameNode (_, name)) rest
        = unpackSt name $ rest


newtype VarargNode = VarargNode SourceRange deriving (Show)


instance DefString VarargNode where
    defString d (VarargNode _)
        = "..."


data Expression
    = ExprNil
    | ExprBool Bool
    | ExprInt Integer
    | ExprReal Rational
    | ExprString BSt.ByteString
    | ExprEllipsis
    | ExprFunction
        [NameNode] -- parameter names
        (Maybe VarargNode) -- vararg
        [StatNode] -- body
    | ExprTable [(Maybe ExprNode, ExprNode)] (Maybe ExprNode)
    | ExprVar NameNode
    | ExprIndex ExprNode ExprNode
    | ExprCall ExprNode [ExprNode] (Maybe ExprNode)
    | ExprMethodCall ExprNode BSt.ByteString [ExprNode] (Maybe ExprNode)
    | ExprUnary UnaryOp ExprNode
    | ExprBinary BinaryOp ExprNode ExprNode
    | ExprAnd ExprNode ExprNode
    | ExprOr ExprNode ExprNode
    deriving (Show)


data UnaryOp
    = UnaryNot
    | UnaryLength
    | UnaryMinus
    | UnaryBNot
    deriving (Show)


data BinaryOp
    = BinaryPower
    | BinaryTimes
    | BinaryDivide
    | BinaryFloorDiv
    | BinaryModulo
    | BinaryPlus
    | BinaryMinus
    | BinaryConcat
    | BinaryLShift
    | BinaryRShift
    | BinaryBAnd
    | BinaryBXor
    | BinaryBOr
    | BinaryLess
    | BinaryGreater
    | BinaryLessEqual
    | BinaryGreaterEqual
    | BinaryNotEqual
    | BinaryEqual
    deriving (Show)


instance DefString UnaryOp where
    defString d UnaryNot = "not"
    defString d UnaryLength = "#"
    defString d UnaryMinus = "-"
    defString d UnaryBNot = "~"


instance DefString BinaryOp where
    defString d BinaryPower = "^"
    defString d BinaryTimes = "*"
    defString d BinaryDivide = "/"
    defString d BinaryFloorDiv = "//"
    defString d BinaryModulo = "%"
    defString d BinaryPlus = "+"
    defString d BinaryMinus = "-"
    defString d BinaryConcat = ".."
    defString d BinaryLShift = "<<"
    defString d BinaryRShift = ">>"
    defString d BinaryBAnd = "&"
    defString d BinaryBXor = "~"
    defString d BinaryBOr = "|"
    defString d BinaryLess = "<"
    defString d BinaryGreater = ">"
    defString d BinaryLessEqual = "<="
    defString d BinaryGreaterEqual = ">="
    defString d BinaryNotEqual = "~="
    defString d BinaryEqual = "=="


newtype ExprNode = ExprNode (SourceRange, Expression) deriving (Show)


isMultretExpr :: ExprNode -> Bool
isMultretExpr (ExprNode (_, ExprEllipsis)) = True
isMultretExpr (ExprNode (_, ExprCall _ _ _)) = True
isMultretExpr (ExprNode (_, ExprMethodCall _ _ _ _)) = True
isMultretExpr _ = False


isPrefixExpr :: ExprNode -> Bool
isPrefixExpr (ExprNode (_, ExprVar _)) = True
isPrefixExpr (ExprNode (_, ExprIndex _ _)) = True
isPrefixExpr (ExprNode (_, ExprCall _ _ _)) = True
isPrefixExpr (ExprNode (_, ExprMethodCall _ _ _ _)) = True
isPrefixExpr _ = False


defFunctionHeadless :: Int -> ExprNode -> ShowS
defFunctionHeadless
        d (ExprNode (pr, ExprFunction params vararg body)) rest
    = "(" $ paramStr params vararg $ ")"
        $ defString (d+1) body $ defBr d $ "end" $ rest

    where

    paramStr [] Nothing rest = rest
    paramStr [] (Just va) rest = defString d va $ rest
    paramStr ns Nothing rest = defList ", " d ns $ rest
    paramStr ns (Just va) rest
        = defList ", " d ns $ ", " $ defString d va $ rest


defTableBody :: Int -> [(Maybe ExprNode, ExprNode)] -> ShowS
defTableBody d [] rest = rest
defTableBody d [(Just key, value)] rest
    = defBr d $ "[" $ defString d key $ "] = "
        $ defString d value $ rest
defTableBody d [(Nothing, value)] rest
    = defBr d $ defString d value $ rest
defTableBody d ((Just key, value):x:xs) rest
    = defBr d $ "[" $ defString d key $ "] = "
        $ defString d value $ "," $ defTableBody d (x:xs) $ rest
defTableBody d ((Nothing, value):x:xs) rest
    = defBr d $ defString d value $ "," $ defTableBody d (x:xs) $ rest


defPrefixExpr :: Int -> ExprNode -> ShowS
defPrefixExpr d expr rest
    | isPrefixExpr expr = defString d expr $ rest
    | otherwise = "(" $ defString d expr $ ")" $ rest


isValidIdent :: BSt.ByteString -> Bool
isValidIdent name = do
    checkFirst (BSt.unpack name)
    where
    checkFirst (x:xs)
        = True
            && (False
                || within 'a' 'z' x
                || within 'A' 'Z' x
                || x == byte '_')
            && checkRest xs
    checkFirst _ = False
    checkRest [] = True
    checkRest (x:xs)
        = True
            && (False
                || within 'a' 'z' x
                || within 'A' 'Z' x
                || within '0' '9' x
                || x == byte '_')
            && checkRest xs


instance DefString ExprNode where
    defString d (ExprNode (pr, ExprNil)) rest
        = "nil" $ rest
    defString d (ExprNode (pr, ExprBool False)) rest
        = "false" $ rest
    defString d (ExprNode (pr, ExprBool True)) rest
        = "true" $ rest
    defString d (ExprNode (pr, ExprInt n)) rest
        = shows n $ rest
    defString d (ExprNode (pr, ExprReal n)) rest
        = shows (numerator n) $ "/" $ shows (denominator n) $ rest
    defString d (ExprNode (pr, ExprString s)) rest
        = shows s $ rest
    defString d (ExprNode (pr, ExprEllipsis)) rest
        = "..." $ rest
    defString d e@(ExprNode (pr, ExprFunction _ _ _)) rest
        = "function" $ defFunctionHeadless d e $ rest
    defString d (ExprNode (pr, ExprTable xs Nothing)) rest
        = "{" $ defTableBody (d+1) xs $ "}" $ rest
    defString d (ExprNode (pr, ExprTable xs (Just last))) rest
        = "{" $ defTableBody (d+1) (xs ++ [(Nothing, last)])
            $ " --[[multret]]}" $ rest
    defString d (ExprNode (pr, ExprVar name)) rest
        = defString d name $ rest
    defString d (ExprNode (pr, ExprIndex table key)) rest = do
        case identKey key of
            Just name -> defPrefixExpr d table $ "." $ unpackSt name $ rest
            Nothing -> defPrefixExpr d table
                $ "[" $ defString d key $ "]" $ rest
        where
        identKey (ExprNode (_, ExprString str))
            = if isValidIdent str && not (str `elem` keywords)
                then Just str
                else Nothing
        identKey _ = Nothing
    defString d (ExprNode (pr, ExprCall func args Nothing)) rest
        = defPrefixExpr d func $ "(" $ defList ", " d args $ ")" $ rest
    defString d (ExprNode (pr, ExprCall func args (Just last))) rest
        = defPrefixExpr d func $ "(" $ defList ", " d (args ++ [last])
            $ " --[[multret]])" $ rest
    defString d (ExprNode (pr, ExprMethodCall obj name args Nothing)) rest
        = defPrefixExpr d obj $ ":" $ unpackSt name
            $ "(" $ defList ", " d args $ ")" $ rest
    defString d (ExprNode (pr, ExprMethodCall obj name args (Just last))) rest
        = defPrefixExpr d obj $ ":" $ unpackSt name
            $ "(" $ defList ", " d (args ++ [last]) $ " --[[multret]])" $ rest
    defString d (ExprNode (pr, ExprUnary op x)) rest
        = "(" $ defString d op $ " " $ defString d x $ ")" $ rest
    defString d (ExprNode (pr, ExprBinary op x y)) rest
        = "(" $ defString d x $ " " $ defString d op $ " "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (pr, ExprAnd x y)) rest
        = "(" $ defString d x $ " and "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (pr, ExprOr x y)) rest
        = "(" $ defString d x $ " or "
            $ defString d y $ ")" $ rest


data Statement
    = StatNull
    | StatAssign [ExprNode] [ExprNode] (Maybe ExprNode)
    | StatInvoke ExprNode
    | StatLabel NameNode
    | StatBreak
    | StatGoto NameNode
    | StatDo [StatNode]
    | StatWhile ExprNode [StatNode]
    | StatRepeat [StatNode] ExprNode
    | StatIf ExprNode [StatNode] StatNode
    | StatForNum
        NameNode -- param
        ExprNode -- init
        ExprNode -- final
        (Maybe ExprNode) -- step
        [StatNode] -- body
    | StatForEach [NameNode] [ExprNode] (Maybe ExprNode) [StatNode]
    | StatFunction ExprNode ExprNode
    | StatLocalFunction NameNode ExprNode [StatNode]
    | StatLocalDef
        [(NameNode, Maybe BSt.ByteString)]
        [ExprNode]
        (Maybe ExprNode)
        [StatNode]
    | StatReturn [ExprNode] (Maybe ExprNode)
    deriving (Show)


newtype StatNode = StatNode (SourceRange, Statement) deriving (Show)


defNameAttrs :: Int -> [(NameNode, Maybe BSt.ByteString)] -> ShowS
defNameAttrs d [] rest = rest
defNameAttrs d [(name, Nothing)] rest
    = defString d name $ rest
defNameAttrs d [(name, Just attr)] rest
    = defString d name $ "<" $ unpackSt attr $ ">" $ rest
defNameAttrs d ((name, Nothing):x:xs) rest
    = defString d name $ ", " $ defNameAttrs d (x:xs) $ rest
defNameAttrs d ((name, Just attr):x:xs) rest
    = defString d name $ "<" $ unpackSt attr $ ">, "
        $ defNameAttrs d (x:xs) $ rest


instance DefString StatNode where
    defString d (StatNode (pr, StatNull)) rest
        = ";" $ rest
    defString d (StatNode (pr, StatAssign lhs rhs Nothing)) rest
        = defList ", " d lhs $ " = " $ defList ", " d rhs $ ";" $ rest
    defString d (StatNode (pr, StatAssign lhs rhs (Just last))) rest
        = defList ", " d lhs $ " = " $ defList ", " d (rhs ++ [last])
            $ " --[[multret]];" $ rest
    defString d (StatNode (pr, StatInvoke e)) rest
        = defString d e $ ";" $ rest
    defString d (StatNode (pr, StatLabel name)) rest
        = "::" $ defString d name $ "::;" $ rest
    defString d (StatNode (pr, StatBreak)) rest
        = "break;" $ rest
    defString d (StatNode (pr, StatGoto name)) rest
        = "goto " $ defString d name $ ";" $ rest
    defString d (StatNode (pr, StatDo body)) rest
        = "do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatWhile cond body)) rest
        = "while " $ defString d cond $ " do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatRepeat body cond)) rest
        = "repeat" $ defString (d+1) body
            $ "until " $ defString d cond $ ";" $ rest
    defString d (StatNode (pr, StatIf cond body alt)) rest
        = "if " $ defString d cond $ " then"
            $ defString (d+1) body $ defBr d $ next alt $ rest
        where
        next :: StatNode -> ShowS
        next (StatNode (pr, StatNull)) rest
            = "end;" $ rest
        next (StatNode (pr, StatIf cond body alt)) rest
            = "elseif " $ defString d cond $ " then"
                $ defString (d+1) body $ defBr d $ next alt $ rest
        next (StatNode (pr, StatDo body)) rest
            = "else" $ defString (d+1) body $ defBr d $ "end;" $ rest
        next stat rest
            = "else" $ defString (d+1) [stat] $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForNum pvar a b Nothing body)) rest
        = "for " $ defString d pvar $ " = "
            $ defString d a $ ", " $ defString d b
            $ " do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForNum pvar a b (Just st) body)) rest
        = "for " $ defString d pvar $ " = "
            $ defString d a $ ", " $ defString d b $ ", " $ defString d st
            $ " do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForEach pvars rhs Nothing body)) rest
        = "for " $ defList ", " d pvars  $ " in "
            $ defList ", " d rhs $ " do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForEach pvars rhs (Just last) body)) rest
        = "for " $ defList ", " d pvars  $ " in "
            $ defList ", " d (rhs ++ [last]) $ " --[[multret]] do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatFunction lhs fvalue)) rest
        = "function " $ defString d lhs
            $ defFunctionHeadless d fvalue $ ";" $ rest
    defString d (StatNode (pr, StatLocalFunction name fvalue body)) rest
        = "local function " $ defString d name
            $ defFunctionHeadless d fvalue $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defString d name $ "]]" $ rest
    defString d (StatNode (pr, StatLocalDef nameattrs [] Nothing body)) rest
        = "local " $ defNameAttrs d nameattrs $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr,
            StatLocalDef nameattrs [] (Just last) body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defString d last $ " --[[multret]];"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr, StatLocalDef nameattrs rhs Nothing body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defList ", " d rhs $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr,
            StatLocalDef nameattrs rhs (Just last) body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defList ", " d (rhs ++ [last]) $ " --[[multret]];"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr, StatReturn [] Nothing)) rest
        = "return;" $ rest
    defString d (StatNode (pr, StatReturn [] (Just last))) rest
        = "return " $ defString d last $ " --[[multret]];" $ rest
    defString d (StatNode (pr, StatReturn rhs Nothing)) rest
        = "return " $ defList ", " d rhs $ ";" $ rest
    defString d (StatNode (pr, StatReturn rhs (Just last))) rest
        = "return " $ defList ", " d (rhs ++ [last]) $ " --[[multret]];" $ rest


instance DefString [StatNode] where
    defString d [] rest
        = rest
    defString d (x:xs) rest
        = defBr d $ defString d x $ defString d xs $ rest


defChunkString :: [StatNode] -> ShowS
defChunkString stats rest
    = defList "\n" 0 stats $ rest


gramName :: TokParser s NameNode
gramName = do
    (pr, name) <- tokIdent
    return $ NameNode (pr, name)


gramChunk :: TokParser s [StatNode]
gramChunk = do
    body <- gramBlock
    tokEof
    return $ body


gramBlock :: TokParser s [StatNode]
gramBlock = do
    choice [
        do
            stat <- gramReturnStat
            return $ [stat],
        do
            tokSymbol ";"
            gramBlock,
        do
            stat <- gramScopingStat
            rest <- gramBlock
            return $ [stat rest],
        do
            stat <- gramSingleStat
            rest <- gramBlock
            return $ stat:rest,
        return $ []]


gramReturnStat :: TokParser s StatNode
gramReturnStat = do
    pra <- tokKeyword "return"
    (prb, rhs, mlast) <- option (pra, [], Nothing) gramExprList
    prc <- option prb (tokSymbol ";")
    return $ StatNode (pra <> prc, StatReturn rhs mlast)


gramSingleStat :: TokParser s StatNode
gramSingleStat = do
    choice [
        do
            pra <- tokSymbol "::"
            name <- gramName
            prb <- tokSymbol "::"
            return $ StatNode (pra <> prb, StatLabel name),
        do
            pr <- tokKeyword "break"
            return $ StatNode (pr, StatBreak),
        do
            pra <- tokKeyword "goto"
            name@(NameNode (prb, _)) <- gramName
            return $ StatNode (pra <> prb, StatGoto name),
        do
            pra <- tokKeyword "do"
            body <- gramBlock
            prb <- tokKeyword "end"
            return $ StatNode (pra <> prb, StatDo body),
        do
            pra <- tokKeyword "while"
            cond <- gramExpr
            tokKeyword "do"
            body <- gramBlock
            prb <- tokKeyword "end"
            return $ StatNode (pra <> prb, StatWhile cond body),
        do
            pra <- tokKeyword "repeat"
            body <- gramBlock
            tokKeyword "until"
            cond@(ExprNode (prb, _)) <- gramExpr
            return $ StatNode (pra <> prb, StatRepeat body cond),
        gramIfStat,
        gramForStat,
        gramFunctionStat,
        gramExprStat]


gramScopingStat :: TokParser s ([StatNode] -> StatNode)
gramScopingStat = gramLocalFunctionStat <|> gramLocalStat


gramIfStat :: TokParser s StatNode
gramIfStat = do
    pra <- tokKeyword "if"
    cond <- gramExpr
    tokKeyword "then"
    body <- gramBlock
    (prb, alt) <- readAlt
    return $ StatNode (pra <> prb, StatIf cond body alt)

    where

    readAlt :: TokParser s (SourceRange, StatNode)
    readAlt = readElseif <|> readElse <|> readEnd

    readElseif :: TokParser s (SourceRange, StatNode)
    readElseif = do
        pra <- tokKeyword "elseif"
        cond <- gramExpr
        tokKeyword "then"
        body <- gramBlock
        (prb, alt) <- readAlt
        return $ (pra <> prb, StatNode (pra <> prb, StatIf cond body alt))

    readElse :: TokParser s (SourceRange, StatNode)
    readElse = do
        pra <- tokKeyword "else"
        body <- gramBlock
        prb <- tokKeyword "end"
        return $ (pra <> prb, StatNode (pra <> prb, StatDo body))

    readEnd :: TokParser s (SourceRange, StatNode)
    readEnd = do
        pr <- tokKeyword "end"
        return $ (pr, StatNode (pr, StatNull))


gramForStat :: TokParser s StatNode
gramForStat = do
    pra <- tokKeyword "for"
    names <- readNameList
    case names of
        [name] -> gramForNumStat pra name <|> gramForEachStat pra names
        _ -> gramForEachStat pra names

    where

    readNameList :: TokParser s [NameNode]
    readNameList = do
        init <- gramName
        choice [
            do
                tokSymbol ","
                rest <- readNameList
                return $ init:rest,
            return $ [init]]


gramForNumStat :: SourceRange -> NameNode -> TokParser s StatNode
gramForNumStat pra name = do
    tokSymbol "="
    init <- gramExpr
    tokSymbol ","
    final <- gramExpr
    mstep <- optionMaybe (do
        tokSymbol ","
        gramExpr)
    tokKeyword "do"
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ StatNode (pra <> prb, StatForNum name init final mstep body)


gramForEachStat :: SourceRange -> [NameNode] -> TokParser s StatNode
gramForEachStat pra names = do
    tokKeyword "in"
    (_, rhs, mlast) <- gramExprList
    tokKeyword "do"
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ StatNode (pra <> prb, StatForEach names rhs mlast body)


gramFunctionStat :: TokParser s StatNode
gramFunctionStat = do
    try (lookAhead (tokKeyword "function" >> tokIdent))
    pra <- tokKeyword "function"
    (target, isMethod) <- readTarget
    func@(ExprNode (prb, _)) <- gramMethodFunctionBody isMethod
    return $ StatNode (pra <> prb, StatFunction target func)

    where

    readTarget :: TokParser s (ExprNode, Bool)
    readTarget = do
        base <- gramVarExpr
        suffixes base

    suffixes :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixes base
        = suffixIndex base <|> suffixMethod base <|> (return $ (base, False))

    suffixIndex :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixIndex base@(ExprNode (pra, _)) = do
        tokSymbol "."
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixMethod :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixMethod base@(ExprNode (pra, _)) = do
        tokSymbol ":"
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        return $ (ExprNode (pra <> prb, ExprIndex base index), True)


gramLocalFunctionStat :: TokParser s ([StatNode] -> StatNode)
gramLocalFunctionStat = do
    try (lookAhead (tokKeyword "local" >> tokKeyword "function"))
    pra <- tokKeyword "local"
    tokKeyword "function"
    name <- gramName
    func@(ExprNode (prb, _)) <- gramFunctionBody
    return $ (\body ->
        StatNode (pra <> prb, StatLocalFunction name func body))


gramLocalStat :: TokParser s ([StatNode] -> StatNode)
gramLocalStat = do
    pra <- tokKeyword "local"
    nameattrs <- readNameattrs
    localInit pra nameattrs <|> localPure pra nameattrs

    where

    readNameattrs :: TokParser s [(NameNode, Maybe BSt.ByteString)]
    readNameattrs = do
        name <- gramName
        mattr <- optionMaybe readAttr
        choice [
            do
                tokSymbol ","
                rest <- readNameattrs
                return $ (name, mattr):rest,
            return $ [(name, mattr)]]

    readAttr :: TokParser s BSt.ByteString
    readAttr = do
        tokSymbol "<"
        (_, attr) <- tokIdent
        tokSymbol ">"
        return $ attr

    localInit
        :: SourceRange -> [(NameNode, Maybe BSt.ByteString)]
        -> TokParser s ([StatNode] -> StatNode)
    localInit pra nameattrs = do
        tokSymbol "="
        (prb, rhs, mlast) <- gramExprList
        return $ (\body ->
            StatNode (pra <> prb, StatLocalDef nameattrs rhs mlast body))

    localPure
        :: SourceRange -> [(NameNode, Maybe BSt.ByteString)]
        -> TokParser s ([StatNode] -> StatNode)
    localPure pra nameattrs = do
        return $ (\body ->
            StatNode (pra, StatLocalDef nameattrs [] Nothing body))


gramExprStat :: TokParser s StatNode
gramExprStat = do
    (pra, lhs', mlast) <- gramExprList
    let lhs = case mlast of
            Nothing -> lhs'
            Just last -> lhs' ++ [last]
    case lhs of
        [expr] -> assignment pra lhs <|> invocation pra expr
        _ -> assignment pra lhs

    where

    assignment :: SourceRange -> [ExprNode] -> TokParser s StatNode
    assignment pra lhs = do
        tokSymbol "="
        (prb, rhs, mlast) <- gramExprList
        return $ StatNode (pra <> prb, StatAssign lhs rhs mlast)

    invocation :: SourceRange -> ExprNode -> TokParser s StatNode
    invocation pra expr = do
        return $ StatNode (pra, StatInvoke expr)


gramExprList :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
gramExprList = do
    init@(ExprNode(pra, _)) <- gramExpr
    choice [
        do
            tokSymbol ","
            (prb, rest, mlast) <- gramExprList
            return $ (pra <> prb, init:rest, mlast),
        if isMultretExpr init
            then return $ (pra, [], Just init)
            else return $ (pra, [init], Nothing)]


gramExpr :: TokParser s ExprNode
gramExpr = do
    readOr

    where

    readPower :: TokParser s ExprNode
    readPower = do
        base@(ExprNode (pra, _)) <- gramTermExpr
        choice [
            do
                tokSymbol "^"
                exp@(ExprNode (prb, _)) <- readPrefix
                return $
                    ExprNode (pra <> prb, ExprBinary BinaryPower base exp),
            return $ base]

    readPrefix :: TokParser s ExprNode
    readPrefix = do
        choice [
            unaryReader (tokKeyword "not") UnaryNot readPrefix,
            unaryReader (tokSymbol "#") UnaryLength readPrefix,
            unaryReader (tokSymbol "-") UnaryMinus readPrefix,
            unaryReader (tokSymbol "~") UnaryBNot readPrefix,
            readPower]

    readProduct :: TokParser s ExprNode
    readProduct = do
        chainl1 readPrefix $
            choice [
                binaryOp (tokSymbol "*") (ExprBinary BinaryTimes),
                binaryOp (tokSymbol "/") (ExprBinary BinaryDivide),
                binaryOp (tokSymbol "//") (ExprBinary BinaryFloorDiv),
                binaryOp (tokSymbol "%") (ExprBinary BinaryModulo)]

    readSum :: TokParser s ExprNode
    readSum = do
        chainl1 readProduct $
            choice [
                binaryOp (tokSymbol "+") (ExprBinary BinaryPlus),
                binaryOp (tokSymbol "-") (ExprBinary BinaryMinus)]

    readJoin :: TokParser s ExprNode
    readJoin = do
        chainr1 readSum $
            binaryOp (tokSymbol "..") (ExprBinary BinaryConcat)

    readShift :: TokParser s ExprNode
    readShift = do
        chainr1 readJoin $
            binaryOp (tokSymbol "..") (ExprBinary BinaryConcat)

    readBitAnd :: TokParser s ExprNode
    readBitAnd = do
        chainl1 readShift $
            binaryOp (tokSymbol "&") (ExprBinary BinaryBAnd)

    readBitXor :: TokParser s ExprNode
    readBitXor = do
        chainl1 readBitAnd $
            binaryOp (tokSymbol "~") (ExprBinary BinaryBXor)

    readBitOr :: TokParser s ExprNode
    readBitOr = do
        chainl1 readBitXor $
            binaryOp (tokSymbol "|") (ExprBinary BinaryBOr)

    readCompare :: TokParser s ExprNode
    readCompare = do
        chainl1 readBitOr $
            choice [
                binaryOp (tokSymbol "<") (ExprBinary BinaryLess),
                binaryOp (tokSymbol ">") (ExprBinary BinaryGreater),
                binaryOp (tokSymbol "<=") (ExprBinary BinaryLessEqual),
                binaryOp (tokSymbol ">=") (ExprBinary BinaryGreaterEqual),
                binaryOp (tokSymbol "~=") (ExprBinary BinaryNotEqual),
                binaryOp (tokSymbol "==") (ExprBinary BinaryEqual)]

    readAnd :: TokParser s ExprNode
    readAnd = do
        chainl1 readCompare $
            binaryOp (tokKeyword "and") ExprAnd

    readOr :: TokParser s ExprNode
    readOr = do
        chainl1 readAnd $
            binaryOp (tokKeyword "or") ExprOr

    unaryReader
        :: TokParser s SourceRange
        -> UnaryOp
        -> TokParser s ExprNode
        -> TokParser s ExprNode
    unaryReader prefixParser op elemParser = do
        pra <- prefixParser
        arg@(ExprNode (prb, _)) <- elemParser
        return $ ExprNode (pra <> prb, ExprUnary op arg)

    binaryOp
        :: TokParser s SourceRange
        -> (ExprNode -> ExprNode -> Expression)
        -> TokParser s (ExprNode -> ExprNode -> ExprNode)
    binaryOp parser builder = do
        parser
        return $ (\left@(ExprNode (pra, _)) right@(ExprNode (prb, _)) ->
            ExprNode (pra <> prb, builder left right))


gramTermExpr :: TokParser s ExprNode
gramTermExpr = do
    choice [
        atom "nil" ExprNil,
        atom "false" (ExprBool False),
        atom "true" (ExprBool True),
        literal tokInt ExprInt,
        literal tokReal ExprReal,
        literal tokString ExprString,
        do
            pr <- tokSymbol "..."
            return $ ExprNode (pr, ExprEllipsis),
        do
            pr <- tokKeyword "function"
            gramFunctionBody,
        gramTableExpr,
        gramSuffixExpr]

    where

    atom :: BSt.ByteString -> Expression -> TokParser s ExprNode
    atom kw nd = do
        pr <- tokKeyword kw
        return $ ExprNode (pr, nd)

    literal
        :: TokParser s (SourceRange, a)
        -> (a -> Expression)
        -> TokParser s ExprNode
    literal parse construct = do
        (pr, v) <- parse
        return $ ExprNode (pr, construct v)


gramFunctionBody :: TokParser s ExprNode
gramFunctionBody = do
    gramMethodFunctionBody False


gramMethodFunctionBody :: Bool -> TokParser s ExprNode
gramMethodFunctionBody isMethod = do
    pra <- tokSymbol "("
    (params, vararg) <- gramParamList
    tokSymbol ")"
    let params' =
            if isMethod
                then (NameNode (pra, "self")):params
                else params
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ ExprNode (pra <> prb, ExprFunction params' vararg body)


gramParamList :: TokParser s ([NameNode], Maybe VarargNode)
gramParamList = do
    option ([], Nothing) readParams

    where

    readParams :: TokParser s ([NameNode], Maybe VarargNode)
    readParams = paramVararg <|> paramName

    paramVararg :: TokParser s ([NameNode], Maybe VarargNode)
    paramVararg = do
        pr <- tokSymbol "..."
        return $ ([], Just (VarargNode pr))

    paramName :: TokParser s ([NameNode], Maybe VarargNode)
    paramName = do
        name <- gramName
        choice [
            do
                tokSymbol ","
                (rest, vararg) <- readParams
                return $ (name:rest, vararg),
            return $ ([name], Nothing)]


gramTableExpr :: TokParser s ExprNode
gramTableExpr = do
    pra <- tokSymbol "{"
    (elems, mlast) <- readFields
    prb <- tokSymbol "}"
    return $ ExprNode (pra <> prb, ExprTable elems mlast)

    where

    readFields :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    readFields = do
        choice [
            fieldIndexed,
            fieldNamed,
            fieldPos,
            return $ ([], Nothing)]

    fieldIndexed :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldIndexed = do
        tokSymbol "["
        key <- gramExpr
        tokSymbol "]"
        tokSymbol "="
        value <- gramExpr
        (rest, mlast) <- readTableRest
        return $ ((Just key, value):rest, mlast)

    fieldNamed :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldNamed = do
        try (lookAhead (do
            tokIdent
            tokSymbol "="))
        (pr, nameid) <- tokIdent
        let key = ExprNode (pr, ExprString nameid)
        tokSymbol "="
        value <- gramExpr
        (rest, mlast) <- readTableRest
        return $ ((Just key, value):rest, mlast)

    fieldPos :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldPos = do
        value <- gramExpr
        (rest, mlast) <- readTableRest
        case (rest, mlast) of
            ([], Nothing) ->
                if isMultretExpr value
                    then return $ ([], Just value)
                    else return $ ((Nothing, value):rest, mlast)
            _ -> return $ ((Nothing, value):rest, mlast)

    readTableRest :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    readTableRest = do
        choice [
            do
                tokSymbol "," <|> tokSymbol ";"
                readFields,
            return $ ([], Nothing)]


gramVarExpr :: TokParser s ExprNode
gramVarExpr = do
    name@(NameNode (pr, _)) <- gramName
    return $ ExprNode (pr, ExprVar name)


gramSuffixExpr :: TokParser s ExprNode
gramSuffixExpr = do
    start <- readGroup <|> gramVarExpr
    suffixes $ start

    where

    readGroup :: TokParser s ExprNode
    readGroup = do
        pra <- tokSymbol "("
        expr <- gramExpr
        prb <- tokSymbol ")"
        return $ expr

    suffixes :: ExprNode -> TokParser s ExprNode
    suffixes base = do
        choice [
            suffixExprIndex base,
            suffixNameIndex base,
            suffixMethod base,
            suffixApply base,
            return $ base]

    suffixExprIndex :: ExprNode -> TokParser s ExprNode
    suffixExprIndex base@(ExprNode (pra, _)) = do
        tokSymbol "["
        index <- gramExpr
        prb <- tokSymbol "]"
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixNameIndex :: ExprNode -> TokParser s ExprNode
    suffixNameIndex base@(ExprNode (pra, _)) = do
        tokSymbol "."
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixMethod :: ExprNode -> TokParser s ExprNode
    suffixMethod base@(ExprNode (pra, _)) = do
        tokSymbol ":"
        (_, name) <- tokIdent
        (prb, args, mlast) <- arguments
        let nstr = name
        suffixes $ ExprNode (pra <> prb, ExprMethodCall base nstr args mlast)

    suffixApply :: ExprNode -> TokParser s ExprNode
    suffixApply base@(ExprNode (pra, _)) = do
        (prb, args, mlast) <- arguments
        suffixes $ ExprNode (pra <> prb, ExprCall base args mlast)

    arguments :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    arguments = argList <|> argString <|> argTable

    argList :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    argList = do
        pra <- tokSymbol "("
        (args, mlast) <- option ([], Nothing) argListRest
        prb <- tokSymbol ")"
        return $ (pra <> prb, args, mlast)

    argListRest :: TokParser s ([ExprNode], Maybe ExprNode)
    argListRest = do
        head <- gramExpr
        choice [
            do
                tokSymbol ","
                (tail, mlast) <- argListRest
                return $ ((head:tail), mlast),
            if isMultretExpr head
                then return $ ([], Just head)
                else return $ ([head], Nothing)]

    argString :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    argString = do
        (pr, str) <- tokString
        return $ (pr, [ExprNode (pr, ExprString str)], Nothing)

    argTable :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    argTable = do
        arg@(ExprNode (pr, _)) <- gramTableExpr
        return $ (pr, [arg], Nothing)


data IrSlot
    = ISLocal Int
    | ISConst Int
    | ISGuard Int
    deriving (Eq)


instance DefString IrSlot where
    defString d (ISLocal i) rest = "@" $ shows i $ rest
    defString d (ISConst i) rest = "%" $ shows i $ rest
    defString d (ISGuard i) rest = "&" $ shows i $ rest


data IrValue
    = IANil
    | IABool Bool
    | IAInteger Integer
    | IARational Rational
    | IAString BSt.ByteString
    | IATable IrList [(IrValue, IrValue)]
    | IASlot IrSlot
    | IAUpvalue Int
    | IAUpconst Int
    | IAIndex IrValue IrValue
    | IAUnaryUnm IrValue
    | IAUnaryLen IrValue
    | IAUnaryBNot IrValue
    | IABinaryPow IrValue IrValue
    | IABinaryMul IrValue IrValue
    | IABinaryDiv IrValue IrValue
    | IABinaryIDiv IrValue IrValue
    | IABinaryMod IrValue IrValue
    | IABinaryAdd IrValue IrValue
    | IABinarySub IrValue IrValue
    | IABinaryConcat IrValue IrValue
    | IABinaryShl IrValue IrValue
    | IABinaryShr IrValue IrValue
    | IABinaryBAnd IrValue IrValue
    | IABinaryBXor IrValue IrValue
    | IABinaryBOr IrValue IrValue
    | IABinaryLt IrValue IrValue
    | IABinaryGt IrValue IrValue
    | IABinaryLe IrValue IrValue
    | IABinaryGe IrValue IrValue
    | IABinaryEq IrValue IrValue
    | IABinaryNeq IrValue IrValue
    | IACar IrList
    | IALNot IrValue
    | IALAnd IrValue IrValue
    | IALOr IrValue IrValue
    | IAFunction
        (Maybe (SourceRange, BSt.ByteString)) -- def location, source name
        [(
            Either Int Int, -- Left outerupvalue | Right outerlocal
            Maybe (SourceRange, BSt.ByteString))] -- captured upvalues
        [(
            Either Int IrSlot, -- Left outerupconst | Right outerslot
            Maybe (SourceRange, BSt.ByteString))] -- captured upconstants
        [(Int, Maybe (SourceRange, BSt.ByteString))] -- named arguments
        Int Int Int -- max count of locals|consts|guards
        IrBody -- function body


data IrList
    = IAArguments
    | IACall IrValue IrList
    | IACallMethod IrValue IrValue IrList
    | IARange IrValue IrValue IrValue
    | IAEmpty
    | IACons IrValue IrList


data IrSink
    = IASetLocal Int
    | IASetUpvalue Int
    | IASetIndex IrValue IrValue


data IrAction
    = IAAssign IrList [IrSink] IrAction
    | IAOpen IrList [(Maybe (SourceRange, BSt.ByteString), IrSlot)] IrAction
    | IASequence IrList IrAction
    | IADrop [IrSlot] IrAction
    | IAReturn IrList
    | IATailCall IrValue IrList
    | IATailCallMethod IrValue IrValue IrList
    | IABranch IrValue IrAction IrAction
    | IABlock Int
    | IAMark SourceRange IrAction


type IrBody = [(Int, IrAction)]


instance DefString (IrBody) where
    defString d [] rest = rest
    defString d [(id, act)] rest
        = "def _" $ shows id $ ":" $ defBr (d+1) $ defString (d+1) act $ rest
    defString d ((id, act):x:xs) rest
        = "def _" $ shows id $ ":" $ defBr (d+1) $ defString (d+1) act
            $ defBr d $ defString d (x:xs) $ rest


instance DefString IrValue where
    defString d IANil rest
        = "nil" $ rest
    defString d (IABool False) rest
        = "false" $ rest
    defString d (IABool True) rest
        = "true" $ rest
    defString d (IAInteger i) rest
        = shows i $ rest
    defString d (IARational r) rest
        = shows (numerator r) $ "/" $ shows (denominator r) $ rest
    defString d (IAString s) rest
        = shows s $ rest
    defString d (IATable xs kvs) rest
        = "(table {" $ defString d xs $ "} [" $ defKVs (d+1) kvs $ "])" $ rest
        where
        defKVs d [] rest = rest
        defKVs d [(k, v)] rest
            = defBr d $ defString d k $ " <- " $ defString d v
                $ defBr d $ rest
        defKVs d ((k, v):xs) rest
            = defBr d $ defString d k $ " <- " $ defString d v $ ","
                $ defKVs d xs $ rest
    defString d (IASlot slot) rest
        = defString d slot $ rest
    defString d (IAUpvalue id) rest
        = "$" $ shows id $ rest
    defString d (IAUpconst id) rest
        = "^" $ shows id $ rest
    defString d (IAIndex table index) rest
        = "(index " $ defString d table $ " " $ defString d index $ ")" $ rest
    defString d (IAUnaryUnm a) rest
        = "(munm " $ defString d a $ ")" $ rest
    defString d (IAUnaryLen a) rest
        = "(mlen " $ defString d a $ ")" $ rest
    defString d (IAUnaryBNot a) rest
        = "(mbnot " $ defString d a $ ")" $ rest
    defString d (IABinaryPow a b) rest
        = "(mpow " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryMul a b) rest
        = "(mmul " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryDiv a b) rest
        = "(mdiv " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryIDiv a b) rest
        = "(midiv " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryMod a b) rest
        = "(mmod " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryAdd a b) rest
        = "(madd " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinarySub a b) rest
        = "(msub " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryConcat a b) rest
        = "(mconcat " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryShl a b) rest
        = "(mshl " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryShr a b) rest
        = "(mshr " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryBAnd a b) rest
        = "(mband " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryBXor a b) rest
        = "(mbxor " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryBOr a b) rest
        = "(mor " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryLt a b) rest
        = "(mlt " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryGt a b) rest
        = "(mgt " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryLe a b) rest
        = "(mle " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryGe a b) rest
        = "(mge " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryEq a b) rest
        = "(meq " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IABinaryNeq a b) rest
        = "(mneq " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IACar xs) rest
        = "(car {" $ defString d xs $ "})" $ rest
    defString d (IALNot a) rest
        = "(lnot " $ defString d a $ ")" $ rest
    defString d (IALAnd a b) rest
        = "(land " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IALOr a b) rest
        = "(lor " $ defString d a $ " " $ defString d b $ ")" $ rest
    defString d (IAFunction
            mlocation upvalues upconsts params
            maxlocals maxconsts maxguards body) rest
        = "(" $ defBr (d+1) $ "function" $ defLocation d mlocation
            $ defUpvalues (d+1) upvalues 0
            $ defUpconsts(d+1) upconsts 0
            $ defParameters (d+1) params
            $ defBr (d+1) $ shows maxlocals $ "@ "
            $ shows maxconsts $ "% " $ shows maxguards $ "&"
            $ defBr (d+1) $ defString (d+1) body
            $ defBr (d+1) $ ")" $ rest
        where
        defLocation d Nothing rest = rest
        defLocation d (Just (pr, name)) rest
            = " -- " $ unpackSt name $ " " $ shows pr $ rest
        defUpvalues d [] id rest = rest
        defUpvalues d ((Left oid, mloc):xs) id rest
            = defBr d $ "*$" $ shows id $ " <- $" $ shows oid
                $ defLocation d mloc $ defUpvalues d xs (id+1) $ rest
        defUpvalues d ((Right lid, mloc):xs) id rest
            = defBr d $ "*$" $ shows id $ " <- @" $ shows lid
                $ defLocation d mloc $ defUpvalues d xs (id+1) $ rest
        defUpconsts d [] id rest = rest
        defUpconsts d ((Left oid, mloc):xs) id rest
            = defBr d $ "*^" $ shows id $ " <- ^" $ shows oid
                $ defLocation d mloc $ defUpconsts d xs (id+1) $ rest
        defUpconsts d ((Right slot, mloc):xs) id rest
            = defBr d $ "*^" $ shows id $ " <- " $ defString d slot
                $ defLocation d mloc $ defUpconsts d xs (id+1) $ rest
        defParameters d [] rest = rest
        defParameters d ((id, mloc):xs) rest
            = defBr d $ "*@" $ shows id
                $ defLocation d mloc $ defParameters d xs $ rest


instance DefString IrList where
    defString d IAArguments rest
        = "arguments" $ rest
    defString d (IACall func args) rest
        = "call " $ defString d func $ " {" $ defString d args $ "}" $ rest
    defString d (IACallMethod func index args) rest
        = "callmethod " $ defString d func $ " "
            $ defString d index $ " {" $ defString d args $ "}" $ rest
    defString d (IARange init limit step) rest
        = "range " $ defString d init $ " " $ defString d limit
            $ " " $ defString d step $ rest
    defString d IAEmpty rest
        = "empty" $ rest
    defString d (IACons x xs) rest
        = defString d x $ ", " $ defString d xs $ rest


instance DefString IrSink where
    defString d (IASetLocal id) rest
        = "=@" $ shows id $ rest
    defString d (IASetUpvalue id) rest
        = "=$" $ shows id $ rest
    defString d (IASetIndex table index) rest
        = "(setindex " $ defString d table $ " "
            $ defString d index $ ")" $ rest


instance DefString IrAction where
    defString d (IAAssign source targets next) rest
        = "assign {" $ defString d source $ "} ["
            $ defList ", " d targets $ "];"
            $ defBr d $ defString d next $ rest
    defString d (IAOpen source targets next) rest
        = "open {" $ defString d source $ "} ["
            $ defTargets (d+1) targets $ "];"
            $ defBr d $ defString d next $ rest
        where
        defTargets d [] rest = rest
        defTargets d ((Nothing, slot):xs) rest
            = defBr d $ "*" $ defString d slot $ defTargets d xs $ rest
        defTargets d ((Just (pr, name), slot):xs) rest
            = defBr d $ "*" $ defString d slot $ " --[[ " $ unpackSt name
                $ " " $ shows pr $ " ]]" $ defTargets d xs $ rest
    defString d (IASequence sa sb) rest
        = defString d sa $ ";" $ defBr d $ defString d sb $ rest
    defString d (IADrop slots x) rest
        = "drop [" $ defList ", " d slots $ "];" $ defBr d
            $ defString d x $ rest
    defString d (IAReturn x) rest
        = "return {" $ defString d x $ "}" $ rest
    defString d (IATailCall func args) rest
        = "tailcall " $ defString d func $ " {" $ defString d args $ "}" $ rest
    defString d (IATailCallMethod func index args) rest
        = "tailcallmethod " $ defString d func $ " " $ defString d index $ " {"
            $ defString d args $ "}" $ rest
    defString d (IABranch cond ba bb) rest
        = "if " $ defString d cond $ ":"
            $ defBr (d+1) $ defString (d+1) ba
            $ defBr d $ "else:"
            $ defBr (d+1) $ defString (d+1) bb $ rest
    defString d (IABlock id) rest
        = "block _" $ shows id $ rest
    defString d (IAMark pr next) rest
        = "mark " $ shows pr $ ";" $ defBr d $ defString d next $ rest


data CompileError = CompileError SourceRange String


instance Show CompileError where
    show (CompileError range msg) = show range ++ "\n" ++ msg


type BlockTable = [(Int, Maybe BSt.ByteString, [Int])]


data LexicalContext = LexicalContext {
    lecxOuter :: (Maybe LexicalContext),
    lecxNextIndex :: Int,
    lecxMaxLocals :: Int,
    lecxMaxConsts :: Int,
    lecxMaxGuards :: Int,
    lecxSlots :: [(Int, BSt.ByteString, SourceRange, IrSlot)],
    lecxUpvalues :: [(BSt.ByteString, SourceRange, Int, Either Int Int)],
    lecxUpconsts :: [(BSt.ByteString, SourceRange, Int, Either Int IrSlot)],
    lecxVararg :: Bool,
    lecxBlocks :: BlockTable}


lecxGetSlots
    :: (Monad m)
    => StateT LexicalContext m [(Int, BSt.ByteString, SourceRange, IrSlot)]
lecxGetSlots = do
    (LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va blocks) <- get
    return $ slots


lecxSetSlots
    :: (Monad m)
    => [(Int, BSt.ByteString, SourceRange, IrSlot)]
    -> StateT LexicalContext m ()
lecxSetSlots slots = do
    (LexicalContext
        mouter iname  maxl maxc maxg
        _ upvalues upconsts va blocks) <- get
    put $ LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va blocks


lecxCreateSlot
    :: (Monad m)
    => (Int -> IrSlot)
    -> (IrSlot -> Int -> Int)
    -> ((Int, Int, Int) -> Int -> (Int, Int, Int))
    -> BSt.ByteString
    -> SourceRange
    -> StateT LexicalContext m Int
lecxCreateSlot genf idmatch maxupdate name pr = do
    (LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va blocks) <- get
    let id = nextId slots
    let entry = (iname, name, pr, genf id)
    let (maxl', maxc', maxg') = maxupdate (maxl, maxc, maxg) (id + 1)
    put $ LexicalContext
        mouter (iname+1) maxl' maxc' maxg'
        (entry:slots) upvalues upconsts va blocks
    return $ id
    where
    nextId [] = 0
    nextId ((_, _, _, slot):xs) = idmatch slot (nextId xs)


lecxCreateLocal name pr = do
    lecxCreateSlot
        ISLocal
        (\slot rest -> case slot of ISLocal id -> id + 1; _ -> rest)
        (\(maxl, maxc, maxg) id -> (max id maxl, maxc, maxg))
        name pr
lecxCreateConst name pr = do
    lecxCreateSlot
        ISConst
        (\slot rest -> case slot of ISConst id -> id + 1; _ -> rest)
        (\(maxl, maxc, maxg) id -> (maxl, max id maxc, maxg))
        name pr
lecxCreateGuard name pr = do
    lecxCreateSlot
        ISGuard
        (\slot rest -> case slot of ISGuard id -> id + 1; _ -> rest)
        (\(maxl, maxc, maxg) id -> (maxl, maxc, max id maxg))
        name pr


lecxAccessVariable
    :: BSt.ByteString
    -> StateT LexicalContext Maybe (
        SourceRange,
        (IrSlot -> a) -> (Int -> a) -> (Int -> a) -> a)
lecxAccessVariable name = do
    lecx@(LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va blocks) <- get
    msum [
        searchSlots slots,
        searchUpvalues upvalues,
        searchUpconsts upconsts,
        do
            ((pr, disp), outer') <- lift (do
                outer <- mouter
                runStateT (lecxAccessVariable name) outer)
            let pullUpvalue source = (do
                let id = case upvalues of
                        (_, _, uid, _):_ -> uid+1
                        _ -> 0
                let entry = (name, pr, id, source)
                put $ LexicalContext
                    (Just outer') iname maxl maxc maxg
                    slots (entry:upvalues) upconsts va blocks
                return $ (pr, (\onSlot onUpvalue onUpconst -> onUpvalue id)))
            let pullUpconst source = (do
                let id = case upconsts of
                        (_, _, uid, _):_ -> uid+1
                        _ -> 0
                let entry = (name, pr, id, source)
                put $ LexicalContext
                    (Just outer') iname maxl maxc maxg
                    slots upvalues (entry:upconsts) va blocks
                return $ (pr, (\onSlot onUpvalue onUpconst -> onUpconst id)))
            disp
                (\slot -> do
                    case slot of
                        ISLocal lid -> pullUpvalue $ Right lid
                        _ -> pullUpconst $ Right slot)
                (\upvalueid -> do
                    pullUpvalue $ Left upvalueid)
                (\upconstid -> do
                    pullUpconst $ Left upconstid)]
    where
    searchSlots [] = do
        lift $ Nothing
    searchSlots ((index, defname, pr, slot):rest) = do
        if defname == name
            then return $ (pr, (\onSlot onUpvalue onUpconst -> onSlot slot))
            else searchSlots rest
    searchUpvalues [] = do
        lift $ Nothing
    searchUpvalues ((defname, pr, id, _):rest) = do
        if defname == name
            then return $ (pr, (\onSlot onUpvalue onUpconst -> onUpvalue id))
            else searchUpvalues rest
    searchUpconsts [] = do
        lift $ Nothing
    searchUpconsts ((defname, pr, id, _):rest) = do
        if defname == name
            then return $ (pr, (\onSlot onUpvalue onUpconst -> onUpconst id))
            else searchUpconsts rest


lecxCreateBlock
    :: Maybe BSt.ByteString
    -> StateT LexicalContext Maybe Int
lecxCreateBlock mname = do
    (LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va blocks) <- get
    case mname of
        Just name -> lift $ checkDup name blocks
        Nothing -> return ()
    let id = case blocks of
            (last, _, _):_ -> last+1
    let locstack = reverse $ map (\(index,_,_,_) -> index) slots
    put $ LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va ((id, mname, locstack):blocks)
    return $ id
    where
    checkDup name [] = Just ()
    checkDup name ((_, Nothing, _):xs) = do
        checkDup name xs
    checkDup name ((_, Just name', _):xs) = do
        if name == name'
            then Nothing
            else checkDup name xs


lecxNewBlock
    :: (Monad m) => StateT LexicalContext m Int
lecxNewBlock = do
    mapStateT (\(Just x) -> return x) $ lecxCreateBlock Nothing


lecxGetLocalStack
    :: (Monad m) => StateT LexicalContext m [(Int, IrSlot)]
lecxGetLocalStack = do
    (LexicalContext
        mouter iname maxl maxc maxg
        slots upvalues upconsts va blocks) <- get
    return $ map (\(index, _, _, slot) -> (index, slot)) slots


type Compile t = StateT LexicalContext (Either CompileError) t


compileVariable
    :: NameNode
    -> Compile
        (  (IrSlot -> a)
        -> (Int -> a)
        -> (Int -> a)
        -> (IrValue -> IrValue -> a)
        -> a)
compileVariable (NameNode (pr, name)) = do
    mapStateT (maybe err Right) $
        tryAccess name `mplus` tryEnv name
    where
    err = Left $ CompileError pr "Cannot access a variable"
    tryAccess name = do
        (_, disp) <- lecxAccessVariable name
        disp
            (\slot -> do
                return $ (\onSlot onUpvalue onUpconst onIndex -> do
                    onSlot slot))
            (\upvalue -> do
                return $ (\onSlot onUpvalue onUpconst onIndex -> do
                    onUpvalue upvalue))
            (\upconst -> do
                return $ (\onSlot onUpvalue onUpconst onIndex -> do
                    onUpconst upconst))
    tryEnv name = do
        (_, disp) <- lecxAccessVariable "_ENV"
        disp
            (\slot -> do
                return $ (\onSlot onUpvalue onUpconst onIndex -> do
                    onIndex (IASlot slot) (IAString name)))
            (\upvalue -> do
                return $ (\onSlot onUpvalue onUpconst onIndex -> do
                    onIndex (IAUpvalue upvalue) (IAString name)))
            (\upconst -> do
                return $ (\onSlot onUpvalue onUpconst onIndex -> do
                    onIndex (IAUpconst upconst) (IAString name)))


compileExpressionRead
    :: ShowS
    -> ExprNode
    -> StateT LexicalContext (Either CompileError) IrValue
compileExpressionRead target (ExprNode (pr, ExprNil)) = do
    return $ IANil
compileExpressionRead target (ExprNode (pr, ExprBool b)) = do
    return $ IABool b
compileExpressionRead target (ExprNode (pr, ExprInt i)) = do
    return $ IAInteger i
compileExpressionRead target (ExprNode (pr, ExprReal r)) = do
    return $ IARational r
compileExpressionRead target (ExprNode (pr, ExprString s)) = do
    return $ IAString s
compileExpressionRead target value@(ExprNode (pr,
        ExprFunction params mvararg body)) = do
    let name = BSt.pack $ map byte $ target ""
    functionira <- compileFunction pr name value
    return $ functionira
compileExpressionRead target (ExprNode (pr, ExprTable items mlast)) = do
    let (posits, indits) = foldr
            (\(mindex, value) (posits, indits) -> do
                case mindex of
                    Nothing -> (value:posits, indits)
                    Just index -> (posits, (index, value):indits))
            ([], [])
            items
    let positname n = target . "[" . shows n . "]"
    let posittargets = map positname [1..]
    posira <- compileExpressionList posittargets posits mlast
    inditiras <- forM indits (\(index, value) -> do
        let indexstr = case index of
                ExprNode (_, ExprString str) ->
                    if isValidIdent str
                        then "." . unpackSt str
                        else "[" . defString 0 index . "]"
                _ -> "[" . defString 0 index . "]"
        let valuetarget = target . indexstr
        indexira <- compileExpressionRead "(index)" index
        valueira <- compileExpressionRead valuetarget value
        return $ (indexira, valueira))
    return $ IATable
        posira
        inditiras
compileExpressionRead target (ExprNode (pr, ExprVar namenode)) = do
    disp <- compileVariable namenode
    disp
        (\slot -> do
            return $ IASlot slot)
        (\upvalue -> do
            return $ IAUpvalue upvalue)
        (\upconst -> do
            return $ IAUpconst upconst)
        (\table index -> do
            return $ IAIndex table index)
compileExpressionRead target (ExprNode (pr, ExprIndex table index)) = do
    IAIndex
        <$> compileExpressionRead target table
        <*> compileExpressionRead target index
compileExpressionRead target (ExprNode (pr, ExprUnary op a)) = do
    selectunary op
        <$> compileExpressionRead target a
    where
    selectunary UnaryNot = IALNot
    selectunary UnaryLength = IAUnaryLen
    selectunary UnaryMinus = IAUnaryUnm
    selectunary UnaryBNot = IAUnaryBNot
compileExpressionRead target (ExprNode (pr, ExprBinary op a b)) = do
    selectbinary op
        <$> compileExpressionRead target a
        <*> compileExpressionRead target b
    where
    selectbinary BinaryPower = IABinaryPow
    selectbinary BinaryTimes = IABinaryMul
    selectbinary BinaryDivide = IABinaryDiv
    selectbinary BinaryFloorDiv = IABinaryIDiv
    selectbinary BinaryModulo = IABinaryMod
    selectbinary BinaryPlus = IABinaryAdd
    selectbinary BinaryMinus = IABinarySub
    selectbinary BinaryConcat = IABinaryConcat
    selectbinary BinaryLShift = IABinaryShl
    selectbinary BinaryRShift = IABinaryShr
    selectbinary BinaryBAnd = IABinaryBAnd
    selectbinary BinaryBXor = IABinaryBXor
    selectbinary BinaryBOr = IABinaryBOr
    selectbinary BinaryLess = IABinaryLt
    selectbinary BinaryGreater = IABinaryGt
    selectbinary BinaryLessEqual = IABinaryLe
    selectbinary BinaryGreaterEqual = IABinaryGe
    selectbinary BinaryNotEqual = IABinaryNeq
    selectbinary BinaryEqual = IABinaryEq
compileExpressionRead target (ExprNode (pr, ExprAnd a b)) = do
    IALAnd
        <$> compileExpressionRead target a
        <*> compileExpressionRead target b
compileExpressionRead target (ExprNode (pr, ExprOr a b)) = do
    IALOr
        <$> compileExpressionRead target a
        <*> compileExpressionRead target b
{-ExprEllipsis, ExprCall, ExprMethodCall-}
compileExpressionRead target expr = do
    listira <- compileExpressionReadLast target expr
    return $ IACar listira


compileExpressionReadLast
    :: ShowS
    -> ExprNode
    -> StateT LexicalContext (Either CompileError) IrList
compileExpressionReadLast target (ExprNode (pr, ExprEllipsis)) = do
    vararg <- lecxVararg <$> get
    if vararg
        then return $ IAArguments
        else lift $ Left $
            CompileError pr "Ellipsis must appear inside a vararg function"
compileExpressionReadLast target (ExprNode (pr, ExprCall func args mlast)) = do
    let argname n = "(argument " . shows n . " of " . defString 0 func . ")"
    let argtargets = map argname [1..]
    IACall
        <$> compileExpressionRead target func
        <*> compileExpressionList argtargets args mlast
compileExpressionReadLast target
        (ExprNode (pr, ExprMethodCall obj name args mlast)) = do
    let argname n = "(argument " . shows n . " of " . defString 0 obj
            . ":" . unpackSt name . ")"
    let argtargets = map argname [1..]
    IACallMethod
        <$> compileExpressionRead target obj
        <*> (return $ IAString name)
        <*> compileExpressionList argtargets args mlast
compileExpressionReadLast target (ExprNode (pr, _)) = do
    error "the parser shouldn't produce that"


compileExpressionWrite
    :: ExprNode
    -> StateT LexicalContext (Either CompileError) IrSink
compileExpressionWrite (ExprNode (pr, ExprVar namenode)) = do
    disp <- compileVariable namenode
    disp
        (\slot -> do
            case slot of
                ISLocal id -> return $ IASetLocal id
                _ -> lift $ Left $ errConst)
        (\upvalue -> do
            return $ IASetUpvalue upvalue)
        (\upconst -> do
            lift $ Left $ errConst)
        (\table index -> do
            return $ IASetIndex table index)
    where
    errConst = CompileError pr
        ("Cannot assign to an immutable variable " $ defString 0 namenode $ "")
compileExpressionWrite (ExprNode (pr, ExprIndex table index)) = do
    IASetIndex
        <$> compileExpressionRead "(table)" table
        <*> compileExpressionRead "(index)" index
compileExpressionWrite (ExprNode (pr, _)) = do
    lift $ Left $ CompileError pr "A variable or table field expected"


compileExpressionList
    :: [ShowS]
    -> [ExprNode]
    -> Maybe ExprNode
    -> StateT LexicalContext (Either CompileError) IrList
compileExpressionList _ [] Nothing = do
    return $ IAEmpty
compileExpressionList (t:targets) [] (Just last) = do
    compileExpressionReadLast t last
compileExpressionList (t:targets) (x:xs) mlast = do
    IACons
        <$> compileExpressionRead t x
        <*> compileExpressionList targets xs mlast


type Link t = ReaderT (BlockTable, Maybe Int) (Either CompileError) t


type BodyS m a
    =  IrAction
    -> IrBody
    -> (IrAction, IrBody)


pfindBlock
    :: Int
    -> Link [Int]
pfindBlock idref = do
    (blocks, _) <- ask
    search blocks
    where
    search [] = error "this shouldn't happen"
    search ((id, _, stack):rest) = do
        if id == idref
            then return $ stack
            else search rest


pfindBreak
    :: SourceRange
    -> Link (Int, [Int])
pfindBreak pr = do
    (_, mbreak) <- ask
    case mbreak of
        Just breakid -> do
            stack <- pfindBlock breakid
            return $ (breakid, stack)
        Nothing -> lift $ Left $ CompileError pr "Invalid break"


pfindLabel
    :: SourceRange
    -> BSt.ByteString
    -> Link (Int, [Int])
pfindLabel pr nameref = do
    (blocks, _) <- ask
    search blocks
    where
    search [] = lift $ Left $
        CompileError pr ("Invalid label " $ unpackSt nameref $ "")
    search ((id, Nothing, stack):rest) = search rest
    search ((id, Just name, stack):rest) = do
        if name == nameref
            then return $ (id, stack)
            else search rest


pstackDiff
    :: SourceRange
    -> [(Int, IrSlot)]
    -> [Int]
    -> Link [IrSlot]
pstackDiff pr from to = do
    case to of
        tindex:trest -> do
            case from of
                (findex, _):frest -> do
                    if tindex == findex
                        then pstackDiff pr frest trest
                        else err
                [] -> err
        [] -> return $ reverse $ map snd $ from
    where
    err = do
        lift $ Left $
            CompileError pr "Jump lands inside a variable's scope"


plocalBreak
    :: Int
    -> Link a
    -> Link a
plocalBreak breakid link = local (\(blocks, _) -> (blocks, Just breakid)) link


makeDropBefore
    :: [IrSlot]
    -> IrAction
    -> IrAction
makeDropBefore slots (IADrop slots' after)
    = IADrop (slots ++ slots') after
makeDropBefore slots after
    = IADrop slots after


compileBody
    :: [StatNode]
    -> Link (BodyS m a)
    -> StateT LexicalContext (Either CompileError) (Link (BodyS m a))

compileBody [] prev = do
    return $ prev

compileBody (StatNode (pr, StatNull):others) prev = do
    compileBody others prev

compileBody (StatNode (pr, StatAssign lhs rhs mlast):others) prev = do
    let targets = map (defString 0) lhs ++ repeat id
    sourceira <- compileExpressionList targets rhs mlast
    targets <- forM lhs compileExpressionWrite
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            cprev
                (IAMark pr
                    (IAAssign
                        sourceira
                        targets
                        after))
                bbs))

compileBody (StatNode (pr, StatInvoke expr):others) prev = do
    case expr of
        ExprNode (_, ExprCall _ _ _) -> return ()
        ExprNode (_, ExprMethodCall _ _ _ _) -> return ()
        _ -> lift $ Left $
            CompileError pr "Function call expected"
    exprira <- compileExpressionReadLast "(invoke)" expr
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            cprev
                (IAMark pr
                    (IASequence
                        exprira
                        after))
                bbs))

compileBody (StatNode (pr, StatLabel label):others) prev = do
    let (NameNode (_, name)) = label
    nextid <- mapStateT (maybe (err name) Right) $
        lecxCreateBlock (Just name)
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            let nextbb = (nextid, IAMark pr after)
            cprev
                (IABlock nextid)
                (nextbb:bbs)))
    where
    err name = Left $ CompileError pr ("Duplicate label " $ unpackSt name $ "")

compileBody (StatNode (pr, StatBreak):others) prev = do
    currentstack <- lecxGetLocalStack
    compileBody others (do
        cprev <- prev
        (breakid, targetstack) <- pfindBreak pr
        stackdiff <- pstackDiff pr (reverse currentstack) targetstack
        return (\after bbs -> do
            case stackdiff of
                [] -> cprev
                    (IAMark pr $
                        IABlock breakid)
                    bbs
                _ -> cprev
                    (IAMark pr $
                        IADrop stackdiff (IABlock breakid))
                    bbs))

compileBody (StatNode (pr, StatGoto label):others) prev = do
    let (NameNode (_, name)) = label
    currentstack <- lecxGetLocalStack
    compileBody others (do
        cprev <- prev
        (targetid, targetstack) <- pfindLabel pr name
        stackdiff <- pstackDiff pr (reverse currentstack) targetstack
        return (\after bbs -> do
            case stackdiff of
                [] -> cprev
                    (IAMark pr $
                        IABlock targetid)
                    bbs
                _ -> cprev
                    (IAMark pr $
                        IADrop stackdiff (IABlock targetid))
                    bbs))

compileBody (StatNode (pr, StatDo body):others) prev = do
    pbody <- compileBody body prev
    compileBody others pbody

compileBody (StatNode (pr, StatWhile cond body):others) prev = do
    let (ExprNode (condpr, _)) = cond
    loopid <- lecxNewBlock
    condira <- compileExpressionRead "(while condition)" cond
    pbody <- compileBody body (return (,))
    nextid <- lecxNewBlock
    compileBody others (do
        cprev <- prev
        cbody <- plocalBreak nextid pbody
        return (\after bbs -> do
            let nextbb = (nextid, after)
            let (bodyira, bodybbs) = cbody (IABlock loopid) (nextbb:bbs)
            let loopira = IABranch condira bodyira (IABlock nextid)
            let loopbb = (loopid, IAMark condpr loopira)
            cprev (IABlock loopid) (loopbb:bodybbs)))

compileBody (StatNode (pr, StatRepeat body cond):others) prev = do
    let (ExprNode (condpr, _)) = cond
    loopid <- lecxNewBlock
    pbody <- compileBody body (return (,))
    condira <- compileExpressionRead "(repeat condifion)" cond
    nextid <- lecxNewBlock
    compileBody others (do
        cprev <- prev
        cbody <- plocalBreak nextid pbody
        return (\after bbs -> do
            let nextbb = (nextid, after)
            let checkira = IABranch condira (IABlock nextid) (IABlock loopid)
            let (bodyira, bodybbs) = cbody
                    (IAMark condpr checkira) (nextbb:bbs)
            let loopbb = (loopid, bodyira)
            cprev (IABlock loopid) (loopbb:bodybbs)))

compileBody (StatNode (pr, StatIf cond tbody estat):others) prev = do
    condira <- compileExpressionRead "(if condition)" cond
    ptbody <- compileBody tbody (return (,))
    pestat <- compileBody [estat] (return (,))
    nextid <- lecxNewBlock
    compileBody others (do
        cprev <- prev
        ctbody <- ptbody
        cestat <- pestat
        return (\after bbs -> do
            let nextbb = (nextid, after)
            let mergeira = IABlock nextid
            let (estatira, estatbbs) = cestat mergeira (nextbb:bbs)
            let (tbodyira, tbodybbs) = ctbody mergeira estatbbs
            let branchira = IABranch condira tbodyira estatira
            cprev
                (IAMark pr branchira)
                tbodybbs))

compileBody (StatNode (pr,
        StatForNum param start limit mdelta body):others) prev = do
    let (NameNode (parampr, paramname)) = param
    startira <- compileExpressionRead "(range start)" start
    limitira <- compileExpressionRead "(range limit)" limit
    deltaira <- case mdelta of
            Just delta -> compileExpressionRead "(range delta)" delta
            Nothing -> return $ IAInteger 1
    oldstack <- lecxGetSlots
    fiterid <- lecxCreateConst "(range iterator)" pr
    paramid <- lecxCreateLocal paramname parampr
    loopid <- lecxNewBlock
    pbody <- compileBody body (return (,))
    lecxSetSlots oldstack
    nextid <- lecxNewBlock
    compileBody others (do
        cprev <- prev
        cbody <- plocalBreak nextid pbody
        return (\after bbs -> do
            let (bodyira, bodybbs) = cbody
                    (IABlock loopid)
                    ((nextid, after):bbs)
            let exitira = IADrop
                    [
                        ISLocal paramid,
                        ISConst fiterid]
                    (IABlock nextid)
            let branchira = IABranch
                    (IASlot (ISLocal paramid))
                    bodyira
                    exitira
            let stepira = IAAssign
                    (IACall
                        (IASlot (ISConst fiterid))
                        IAEmpty)
                    [IASetLocal paramid]
                    branchira
            let loopira = IABlock loopid
            let loopbbs = (loopid, IAMark parampr stepira):bodybbs
            let initira = IAOpen
                    (IARange startira limitira deltaira)
                    [
                        (Just (pr, "(range iterator)"), ISConst fiterid),
                        (Just (parampr, paramname), ISLocal paramid)]
                    loopira
            cprev
                (IAMark pr initira)
                loopbbs))

compileBody (StatNode (pr, StatForEach lhs rhs mlast body):others) prev = do
    let lhspr = foldl' (<>) (collapseRangeNull pr) $
            map (\(NameNode (pr, _)) -> pr) lhs
    initlistira <- compileExpressionList
        ([
            "(for iterator)",
            "(for context)",
            "(for index)",
            "(for guard)"] ++ repeat id)
        rhs mlast
    oldstack <- lecxGetSlots
    fiterid <- lecxCreateConst "(for iterator)" pr
    fstateid <- lecxCreateConst "(for context)" pr
    findexid <- lecxCreateLocal "(for index)" pr
    fguardid <- lecxCreateGuard "(for guard)" pr
    locals <- forM lhs (\(NameNode (pr, name)) -> do
        lid <- lecxCreateLocal name pr
        return $ (Just (pr, name), lid))
    let (_, firstlocalid):_ = locals
    loopid <- lecxNewBlock
    pbody <- compileBody body (return (,))
    lecxSetSlots oldstack
    nextid <- lecxNewBlock
    compileBody others (do
        cprev <- prev
        cbody <- plocalBreak nextid pbody
        return (\after bbs -> do
            let (bodyira, bodybbs) = cbody
                    (IABlock loopid)
                    ((nextid, after):bbs)
            let droplist = reverse $ (map (ISLocal . snd) locals)
            let exitira = IADrop
                    (droplist ++ [
                        ISGuard fguardid,
                        ISLocal findexid,
                        ISConst fstateid,
                        ISConst fiterid])
                    (IABlock nextid)
            let branchira = IABranch
                    (IASlot (ISLocal findexid))
                    bodyira
                    exitira
            let stepira = IAAssign
                    (IACall
                        (IASlot (ISConst fiterid))
                        (IACons
                            (IASlot (ISConst fstateid))
                            (IACons
                                (IASlot (ISLocal findexid))
                                IAEmpty)))
                    (map (IASetLocal . snd) locals)
                    (IAAssign
                        (IACons (IASlot (ISLocal firstlocalid)) IAEmpty)
                        [IASetLocal findexid]
                        branchira)
            let loopira = IABlock loopid
            let loopbbs = (loopid, IAMark lhspr stepira):bodybbs
            let initira = IAOpen
                    initlistira
                    [
                        (Just (pr, "(for iterator)"), ISConst fiterid),
                        (Just (pr, "(for context)"), ISConst fstateid),
                        (Just (pr, "(for index)"), ISLocal findexid),
                        (Just (pr, "(for guard)"), ISGuard fguardid)]
                    (IAOpen
                        IAEmpty
                        (map (\(def, lid) -> (def, ISLocal lid)) locals)
                        loopira)
            cprev
                (IAMark pr initira)
                loopbbs))

compileBody (StatNode (pr, StatFunction target value):others) prev = do
    let name = BSt.pack $ map byte $ defString 0 target ""
    functionira <- compileFunction pr name value
    targetira <- compileExpressionWrite target
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            let assignira = IAAssign
                    (IACons functionira IAEmpty)
                    [targetira]
                    after
            cprev
                (IAMark pr assignira)
                bbs))

compileBody (StatNode (pr,
        StatLocalFunction namenode value scope):others) prev = do
    let (NameNode (namepr, name)) = namenode
    oldscope <- lecxGetSlots
    lid <- lecxCreateLocal name namepr
    functionira <- compileFunction pr name value
    pscope <- compileBody scope (return (,))
    lecxSetSlots oldscope
    compileBody others (do
        cprev <- prev
        cscope <- pscope
        return (\after bbs -> do
            let dropira = makeDropBefore [ISLocal lid] after
            let (scopeira, scopebbs) = cscope dropira bbs
            let initira = IAAssign
                    (IACons functionira IAEmpty)
                    [IASetLocal lid]
                    scopeira
            let openira = IAOpen
                    IAEmpty
                    [(Just (namepr, name), ISLocal lid)]
                    initira
            cprev
                (IAMark pr openira)
                scopebbs))

compileBody (StatNode (pr, StatLocalDef lhs rhs mlast scope):others) prev = do
    let targets = map (\(name, _) -> defString 0 name) lhs ++ repeat id
    sourcesira <- compileExpressionList targets rhs mlast
    oldscope <- lecxGetSlots
    locals <- forM lhs makeLocal
    pscope <- compileBody scope (return (,))
    lecxSetSlots oldscope
    compileBody others (do
        cprev <- prev
        cscope <- pscope
        return (\after bbs -> do
            let droplist = reverse $ map snd locals
            let dropira = makeDropBefore droplist after
            let (scopeira, scopebbs) = cscope dropira bbs
            let initira = IAOpen
                    sourcesira
                    locals
                    scopeira
            cprev
                (IAMark pr initira)
                scopebbs))

    where
    makeLocal (NameNode (pr, name), mattr) = do
        case mattr of
            Nothing -> do
                id <- lecxCreateLocal name pr
                return $ (Just (pr, name), ISLocal id)
            Just "const" -> do
                id <- lecxCreateConst name pr
                return $ (Just (pr, name), ISConst id)
            Just "close" -> do
                id <- lecxCreateGuard name pr
                return $ (Just (pr, name), ISGuard id)
            _ -> lift $ Left $ CompileError pr "Invalid variable attribute"

compileBody (StatNode (pr,
        StatReturn [] (Just (ExprNode (_,
            ExprCall func args mlast)))):others) prev = do
    let argname n = "(argument " . shows n . " of " . defString 0 func . ")"
    let argtargets = map argname [1..]
    funcira <- compileExpressionRead "(return)" func
    argira <- compileExpressionList argtargets args mlast
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            cprev
                (IAMark pr
                    (IATailCall
                        funcira
                        argira))
                bbs))

compileBody (StatNode (pr,
        StatReturn [] (Just (ExprNode (_,
            ExprMethodCall obj name args mlast)))):others) prev = do
    let argname n = "(argument " . shows n . " of " . defString 0 obj
            . ":" . unpackSt name . ")"
    let argtargets = map argname [1..]
    objira <- compileExpressionRead "(return)" obj
    let nameira = IAString name
    argira <- compileExpressionList argtargets args mlast
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            cprev
                (IAMark pr
                    (IATailCallMethod
                        objira
                        nameira
                        argira))
                bbs))

compileBody (StatNode (pr, StatReturn rhs mlast):others) prev = do
    let targets = map (\n -> "(return " . shows n . ")") [1..]
    valueira <- compileExpressionList targets rhs mlast
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            cprev
                (IAMark pr
                    (IAReturn valueira))
                bbs))


compileFunction
    :: SourceRange
    -> BSt.ByteString
    -> ExprNode
    -> StateT LexicalContext (Either CompileError) IrValue
compileFunction pr name (ExprNode (_,
        ExprFunction paramnodes mvarargnode stats)) = do
    let isvararg = isJust mvarargnode
    outer <- get
    let (paramdefs, paramdecls, paramcount) = foldr
            (\(NameNode (pr, name)) (defs, decls, count) ->
                (
                    (count, name, pr, ISLocal count):defs,
                    (count, Just (pr, name)):decls,
                    count+1))
            ([], [], 0)
            (reverse paramnodes)
    let innerContext = LexicalContext {
        lecxOuter = Just outer,
        lecxNextIndex = paramcount,
        lecxMaxLocals = paramcount,
        lecxMaxConsts = 0,
        lecxMaxGuards = 0,
        lecxSlots = paramdefs,
        lecxUpvalues = [],
        lecxUpconsts = [],
        lecxVararg = isvararg,
        lecxBlocks = [(0, Nothing, [])]}
    (pbody, context) <- lift $
        runStateT (compileBody stats (return (,))) innerContext
    put $ fromJust $ lecxOuter context
    cbody <- lift $ runReaderT pbody ((lecxBlocks context), Nothing)
    let (main, bbs) = cbody (IAReturn IAEmpty) []
    let upvaluedecls = reverse $ map
            (\(name, pr, id, source) -> (source, Just (pr, name)))
            (lecxUpvalues context)
    let upconstdecls = reverse $ map
            (\(name, pr, id, source) -> (source, Just (pr, name)))
            (lecxUpconsts context)
    return $ IAFunction
        (Just (pr, name))
        upvaluedecls
        upconstdecls
        (reverse paramdecls)
        (lecxMaxLocals context)
        (lecxMaxConsts context)
        (lecxMaxGuards context)
        ((0, main):bbs)


compileChunk :: FilePath -> [StatNode] -> Either CompileError IrValue
compileChunk filename stats = do
    let baseContext = LexicalContext {
        lecxOuter = Nothing,
        lecxNextIndex = 0,
        lecxMaxLocals = 0,
        lecxMaxConsts = 0,
        lecxMaxGuards = 0,
        lecxSlots = [],
        lecxUpvalues = [("_ENV", nullRange filename, 0, Right 0)],
        lecxUpconsts = [],
        lecxVararg = True,
        lecxBlocks = [(0, Nothing, [])]}
    (pbody, context) <- runStateT (compileBody stats (return (,))) baseContext
    cbody <- runReaderT pbody ((lecxBlocks context), Nothing)
    let (main, bbs) = cbody (IAReturn IAEmpty) []
    let chunk = IAFunction
            Nothing
            [(Right 0, Just (nullRange filename, "_ENV"))]
            []
            []
            (lecxMaxLocals context)
            (lecxMaxConsts context)
            (lecxMaxGuards context)
            ((0, main):bbs)
    let outerFunction = IAFunction
            Nothing
            []
            []
            [(0, Just (nullRange filename, "_ENV"))]
            1
            0
            0
            [(0, IAReturn (IACons chunk IAEmpty))]
    return $ outerFunction


translateLua :: FilePath -> B.ByteString -> Either String IrValue
translateLua filename source = do
    parse <- errToStr $ parseGrammar gramChunk filename source
    errToStr $ compileChunk filename parse
    where
    errToStr :: (Show a) => Either a b -> Either String b
    errToStr (Left x) = Left $ show x
    errToStr (Right y) = Right y


test :: IO ()
test = do
    let path = "..\\test.lua"
    input <- B.readFile path
    case parseGrammar gramChunk path input of
        Left err -> putStrLn $ show err
        Right x -> do
            let str = defChunkString x ""
            writeFile "..\\testout.lua" str
            case compileChunk path x of
                Left err -> putStrLn $ show err
                Right irbody -> do
                    let str = defString 0 irbody ""
                    writeFile "..\\testir.lua" str

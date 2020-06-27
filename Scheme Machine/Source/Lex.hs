{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Lex (
    TokenData(..),
    Token,
    TokParser,
    tokWordSatisfy,
    tokWord,
    tokKeyword,
    tokBool,
    tokNumber,
    tokChar,
    tokString,
    tokOpen,
    tokClose,
    tokOpenVec,
    tokQuote,
    tokBackquote,
    tokComma,
    tokSplice,
    tokDot,
    tokEof,
    parseGrammar,
    parseGrammarS,
) where

import Data.Char
import Data.Function
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
import qualified Number as N
import SourceRange

data TokenData
    = TokWord BSt.ByteString
    | TokBool Bool
    | TokNumber N.Number
    | TokChar Char
    | TokString BSt.ByteString
    | TokOpen
    | TokClose
    | TokOpenVec
    | TokQuote
    | TokBackquote
    | TokComma
    | TokSplice
    | TokDot
    | TokWordInternal [Char]
    | TokStringInternal [Char]
    | TokNumberInternal [Char]
    | TokEof
    deriving (Eq)
newtype Token = Token (SourceRange, TokenData) deriving (Show)

instance Show TokenData where
    show (TokWord s) = BSt.unpack s
    show (TokBool False) = "#f"
    show (TokBool True) = "#t"
    show (TokNumber num) = show num
    show (TokChar '\n') = "#\\newline"
    show (TokChar ' ') = "#\\space"
    show (TokChar c) = ['#', '\\', c]
    show (TokString s) = show s
    show TokOpen = "("
    show TokClose = ")"
    show TokOpenVec = "#("
    show TokQuote = "'"
    show TokBackquote = "`"
    show TokComma = ","
    show TokSplice = ",@"
    show TokDot = "."
    show (TokWordInternal s) = "?w " ++ show s
    show (TokStringInternal s) = "?s " ++ show s
    show (TokNumberInternal s) = "?n " ++ show s
    show (TokEof) = "<eof>"

showTokenType :: TokenData -> String
showTokenType (TokWord _) = "word"
showTokenType (TokBool _) = "boolean literal"
showTokenType (TokNumber _) = "number literal"
showTokenType (TokChar _) = "character literal"
showTokenType (TokString _) = "string literal"
showTokenType TokOpen = "("
showTokenType TokClose = ")"
showTokenType TokOpenVec = "#("
showTokenType TokQuote = "'"
showTokenType TokBackquote = "`"
showTokenType TokComma = ","
showTokenType TokSplice = ",@"
showTokenType TokDot = "."
showTokenType (TokWordInternal s) = "word"
showTokenType (TokStringInternal s) = "string literal"
showTokenType (TokNumberInternal s) = "number literal"
showTokenType (TokEof) = "end of file"

within :: (Ord a) => a -> a -> a -> Bool
within a b x = a <= x && x <= b

newtype Cell s = Cell (STRef s (Maybe (Token, Cell s)))
type LParser s = ParsecT B.ByteString () (ST s)

failInvalid :: Char -> LParser s a
failInvalid ch = fail ("Invalid character: " ++ [ch])

coerceLetter :: Char -> Char
coerceLetter x
    | within 'A' 'Z' x = chr (ord x + (ord 'a' - ord 'A'))
    | otherwise = x

lexWhitespace :: LParser s ()
lexWhitespace = do
    satisfy (within '\0' '\32')
    return ()

lexComment :: LParser s ()
lexComment = do
    char ';' <?> ""
    many $ satisfy (\x -> x /= '\n' && x /= '\r')
    return ()

lexSpace :: LParser s ()
lexSpace = do
    many (lexWhitespace <|> lexComment)
    return ()

lexWordInitial :: LParser s Char
lexWordInitial = do
    ch <- satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || x `elem` "!$%&*/:<=>?^_~") <?> "letter"
    return $ coerceLetter ch

lexWordSubsequent :: LParser s Char
lexWordSubsequent = do
    ch <- satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || within '0' '9' x
        || x `elem` "!$%&*/:<=>?^_~+-.@") <?> "letter"
    return $ coerceLetter ch

lexNumberInitial :: LParser s Char
lexNumberInitial = do
    ch <- satisfy (\x -> within '0' '9' x) <?> "digit"
    return $ coerceLetter ch

lexNumberSubsequent :: LParser s Char
lexNumberSubsequent = do
    ch <- satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || within '0' '9' x
        || x `elem` "-+.#@") <?> "digit"
    return $ coerceLetter ch

lexPoundedT :: LParser s TokenData
lexPoundedT = char 't' >> return (TokBool True)

lexPoundedF :: LParser s TokenData
lexPoundedF = char 'f' >> return (TokBool False)

lexPoundedOpenVec :: LParser s TokenData
lexPoundedOpenVec = char '(' >> return TokOpenVec

lexCharnameSubsequent :: LParser s Char
lexCharnameSubsequent = do
    ch <- satisfy (\x -> False
        || within 'a' 'z' x
        || within 'A' 'Z' x
        || within '0' '9' x)
    return $ coerceLetter ch

lexPoundedChar :: LParser s TokenData
lexPoundedChar = do
    char '\\'
    cname <- many lexCharnameSubsequent
    case cname of
        "" -> do
            ch <- anyChar
            return $ TokChar ch
        "space" -> return $ TokChar ' '
        "newline" -> return $ TokChar '\n'
        "n" -> return $ TokChar '\n'
        "r" -> return $ TokChar '\n'
        "t" -> return $ TokChar '\n'
        _ -> unexpected ("character name " ++ show cname)

lexPoundedNumber :: LParser s TokenData
lexPoundedNumber = do
    init <- oneOf "eibodx"
    rest <- many1 lexNumberSubsequent
    return $ TokNumberInternal ('#':init:rest)

lexPounded :: LParser s TokenData
lexPounded = do
    char '#'
    choice [
        lexPoundedT,
        lexPoundedF,
        lexPoundedOpenVec,
        lexPoundedChar,
        lexPoundedNumber] <?> "valid #-token"

lexStringEscape :: LParser s [Char]
lexStringEscape = do
    char '\\' <?> ""
    choice [
        char 'n' >> return "\n",
        char 'r' >> return "\r",
        char 't' >> return "\t",
        do
            char '\n'
            optional (char '\r')
            return "\n",
        do
            char '\r'
            optional (char '\n')
            return "\n",
        do
            char 'z'
            lexSpace
            return "",
        do
            a <- satisfy (within '0' '9')
            mb <- optionMaybe (satisfy (within '0' '9'))
            mc <- optionMaybe (satisfy (within '0' '9'))
            n <- case (mb, mc) of
                (Nothing, _) -> return $ c2d a
                (Just b, Nothing) -> return $ c2d a * 10 + c2d b
                (Just b, Just c) -> return $ c2d a * 100 + c2d b * 10 + c2d c
            return $ [chr n]] <?> "escape sequence"
    where
    c2d :: Char -> Int
    c2d c = ord c - ord '0'

lexStringElement :: LParser s [Char]
lexStringElement = do
    ch <- satisfy (`notElem` "\"\n\r")
    return [ch]

lexString :: LParser s TokenData
lexString = do
    char '"' <?> "string"
    cont <- many (lexStringEscape <|> lexStringElement)
    char '"'
    return $ TokStringInternal (concat cont)

lexNumber :: LParser s TokenData
lexNumber = do
    init <- lexNumberInitial
    rest <- many lexNumberSubsequent
    return $ TokNumberInternal (init:rest)

lexWord :: LParser s TokenData
lexWord = do
    init <- lexWordInitial
    rest <- many lexWordSubsequent
    return $ TokWordInternal (init:rest)

lexSign :: LParser s TokenData
lexSign = do
    init <- oneOf "-+" <?> ""
    rest <- many lexNumberSubsequent
    if null rest
        then return $ TokWordInternal [init]
        else return $ TokNumberInternal (init:rest)

lexDot :: LParser s TokenData
lexDot = do
    init <- char '.' <?> ""
    maybenext <- optionMaybe (char '.')
    case maybenext of
        Just next -> do
            rest <- many lexWordSubsequent
            return $ TokWordInternal (init:next:rest)
        Nothing -> do
            rest <- many lexNumberSubsequent
            if null rest
                then return $ TokWordInternal [init]
                else return $ TokNumberInternal (init:rest)

lexComma :: LParser s TokenData
lexComma = do
    init <- char ',' <?> ""
    maybenext <- optionMaybe (char '@')
    case maybenext of
        Just next -> return $ TokSplice
        Nothing -> return $ TokComma

packTokenData :: TokenData -> LParser s TokenData
packTokenData (TokWordInternal s) = return $ TokWord (BSt.pack s)
packTokenData (TokStringInternal s) = return $ TokString (BSt.pack s)
packTokenData (TokNumberInternal s) = do
    case N.parseNumber s of
        Nothing -> fail ("Invalid number: " ++ s)
        Just num -> return $ TokNumber num
packTokenData t = return $ t

lexToken :: LParser s Token
lexToken = do
    spos <- getPosition
    tokdata <- choice [
        lexPounded,
        lexString,
        lexNumber,
        lexWord,
        lexSign,
        lexDot,
        lexComma,
        char '(' >> return TokOpen,
        char ')' >> return TokClose,
        -- char '\'' >> return TokQuote,
        char '\'' >> return undefined,
        eof >> return TokEof]
    epos <- getPosition
    packed <- packTokenData tokdata
    return $ Token (range spos epos, packed)

newtype TokenStream s = TokenStream (Cell s, SourcePos)

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

instance Stream (TokenStream s) (LParser s) Token where
    uncons (TokenStream (cell, _)) = do
        (tok, next) <- fetch cell
        (Token(pr, _), _) <- fetch next
        return $ Just (tok, TokenStream (next, rangeStart pr))

type TokParser s = ParsecT (TokenStream s) () (LParser s)

tokPrim :: (Token -> Maybe a) -> TokParser s a
tokPrim f = do
    tokenPrim
        (\(Token (_, td)) -> showTokenType td)
        (\_ _ (TokenStream (_, npos)) -> npos)
        f
    where

lexGrammar :: TokParser s a -> SourceName -> LParser s (Either ParseError a)
lexGrammar gram name = do
    init <- lift (newSTRef Nothing)
    ipos <- getPosition
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

parseGrammarS :: (forall s . TokParser s a) -> String -> Either ParseError a
parseGrammarS gram input = parseGrammar gram "<string>" (B.pack input)

tokWordSatisfy
    :: (BSt.ByteString -> Bool)
    -> TokParser s (SourceRange, BSt.ByteString)
tokWordSatisfy test = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokWord w ->
                    if test w
                        then Just (pr, w)
                        else Nothing
                _ -> Nothing)

tokWord :: TokParser s (SourceRange, BSt.ByteString)
tokWord = tokWordSatisfy (const True) <?> "word"

tokKeyword :: String -> TokParser s SourceRange
tokKeyword ref =  do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokWord w ->
                    if w == ref'
                        then Just pr
                        else Nothing
                _ -> Nothing)
    <?> ("'" ++ ref ++ "'")
    where
    ref' = BSt.pack ref

tokBool :: TokParser s (SourceRange, Bool)
tokBool = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokBool b -> Just (pr, b)
                _ -> Nothing)
    <?> "boolean"

tokNumber :: TokParser s (SourceRange, N.Number)
tokNumber = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokNumber n -> Just (pr, n)
                _ -> Nothing)
    <?> "number"

tokChar :: TokParser s (SourceRange, Char)
tokChar = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokChar c -> Just (pr, c)
                _ -> Nothing)
    <?> "character"

tokString :: TokParser s (SourceRange, BSt.ByteString)
tokString = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokString s -> Just (pr, s)
                _ -> Nothing)
    <?> "string"

tokEqualTest :: TokenData -> TokParser s SourceRange
tokEqualTest ref = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            if td == ref
                then Just pr
                else Nothing)
    <?> (show ref)

tokOpen = tokEqualTest TokOpen
tokClose = tokEqualTest TokClose
tokOpenVec = tokEqualTest TokOpenVec
tokQuote = tokEqualTest TokQuote
tokBackquote = tokEqualTest TokBackquote
tokComma = tokEqualTest TokComma
tokSplice = tokEqualTest TokSplice
tokDot = tokEqualTest TokDot

tokEof :: TokParser s ()
tokEof = do
    tokPrim
        (\tok@(Token (pr, td)) ->
            case td of
                TokEof -> Just ()
                _ -> Nothing)

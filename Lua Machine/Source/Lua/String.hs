{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.String (
    luaBinpackRead,
    luaBinpackSize,
    luaBinpackWrite,
    luaBreakString,
    luaFormat,
    luaLexInteger,
) where


import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.RWS.CPS
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Maybe
import Data.Monoid (Endo(..), appEndo)
import Data.Ratio
import GHC.Float
import Text.Parsec.Char hiding (digit)
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (uncons)
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua.Common


data FieldPad
    = PTLeft
    | PTRight
    | PTZero
    deriving (Show)
data PrintField = PrintField {
    pfSign :: Maybe Char,
    pfAlt :: Bool,
    pfPad :: FieldPad,
    pfWidth :: Maybe Int,
    pfPrec :: Maybe Int,
    pfKind :: Char}
    deriving (Show)


errWrongFormat :: LuaValue q s
errWrongFormat = LString $ "Invalid format string"

errWrongSlot :: Char -> LuaValue q s
errWrongSlot ch = LString $ "Unknown format specifier %" <> BSt.pack [ch]

errWrongType :: BSt.ByteString -> LuaValue q s -> LuaValue q s
errWrongType fmt a = LString $
    "Cannot apply " <> fmt <> " to a " <> luaTypename a


permutationOf
    :: [Char]
    -> Parsec BSt.ByteString () [Char]
permutationOf set = do
    option [] (do
        ch <- oneOf set
        rest <- permutationOf (delete ch set)
        return $ ch:rest)


readFormatSlot :: Parsec BSt.ByteString () PrintField
readFormatSlot = do
    try $ notFollowedBy $ string "%%"
    _ <- char '%'
    flags <- permutationOf "+-# 0"
    mwidth <- optionMaybe $ readNum
    mprec <- optionMaybe $ char '.' >> readNum
    kind <- satisfy (\x -> False
        || 'a' <= x && x <= 'z'
        || 'A' <= x && x <= 'Z')
    return $ PrintField {
        pfSign = selectSign flags,
        pfPad = selectPad flags,
        pfAlt = '#' `elem` flags,
        pfWidth = mwidth,
        pfPrec = mprec,
        pfKind = kind}
    where
    selectSign flags
        | '+' `elem` flags = Just '+'
        | ' ' `elem` flags = Just ' '
        | otherwise = Nothing
    selectPad flags
        | '-' `elem` flags = PTRight
        | '0' `elem` flags = PTZero
        | otherwise = PTLeft
    readNum = do
        first <- satisfy (\x -> '0' <= x && x <= '9')
        if first == '0'
            then return 0
            else do
                rest <- many $ satisfy (\x -> '0' <= x && x <= '9')
                return $ itos $ first:rest
    itos digits
        | x > toInteger (maxBound :: Int) = maxBound :: Int
        | x < toInteger (minBound :: Int) = minBound :: Int
        | otherwise = fromInteger x :: Int
        where
        x = read digits :: Integer



readFormatLiteral :: Parsec BSt.ByteString () B.ByteString
readFormatLiteral = do
    chars <- many1 $ choice [
        satisfy (\x -> x /= '%'),
        '%' <$ try (string "%%")]
    return $ B.pack chars


readFormat :: Parsec BSt.ByteString () [Either B.ByteString PrintField]
readFormat = do
    fields <- many $
        (Right <$> readFormatSlot) <|> (Left <$> readFormatLiteral)
    eof
    return $ fields


parseFormat
    :: BSt.ByteString
    -> LuaState q s [Either B.ByteString PrintField]
parseFormat fmt = do
    case parse readFormat "" fmt of
        Left _ -> luaError $ errWrongFormat
        Right fields -> return $ fields


data NEnc = NEnc Integer (Integer -> Char)


generalItos
    :: NEnc
    -> Integer
    -> Int
    -> Int
    -> ShowS
generalItos nb@(NEnc base itoc) i digits decpt rest
    | i == 0 && digits <= 0 && decpt < 0 = rest
    | decpt == 0 = generalItos nb i digits (-1) ('.':rest)
    | i >= 0 = do
        let (i', d) = i `divMod` base
        generalItos nb i' (digits-1) (decpt-1) (itoc d:rest)
    | otherwise = undefined


nenc8 :: NEnc
nenc8 = NEnc 8 (\d -> toEnum (fromEnum d + fromEnum '0'))

nenc10 :: NEnc
nenc10 = NEnc 10 (\d -> toEnum (fromEnum d + fromEnum '0'))

nenc16L :: NEnc
nenc16L = NEnc 16 (\d -> do
    let di = fromEnum d
    let csh = if di < 10 then fromEnum '0' else fromEnum 'a' - 10
    toEnum (di + csh))

nenc16B :: NEnc
nenc16B = NEnc 16 (\d -> do
    let di = fromEnum d
    let csh = if di < 10 then fromEnum '0' else fromEnum 'A' - 10
    toEnum (di + csh))


normalized
    :: Rational
    -> Rational
    -> (Int -> Rational -> a)
    -> a
normalized base q onNorm
    | q <= 0 = do
        onNorm 0 0
    | q < 1 = do
        normalized base (q * base) (\e qn ->
            onNorm (e-1) qn)
    | q < base = do
        onNorm 0 q
    | otherwise = do
        normalized base (q / base) (\e qn ->
            onNorm (e+1) qn)


exponentItos :: Int -> ShowS
exponentItos e rest
    | e < 0 = '-':generalItos nenc10 (-toInteger e) 1 (-1) rest
    | otherwise = '+':generalItos nenc10 (toInteger e) 1 (-1) rest


formatNumberFixed
    :: NEnc
    -> Int
    -> Int
    -> Rational
    -> ShowS
formatNumberFixed nenc@(NEnc dbase _) ilen flen q = do
    let prec = max flen 0
    generalItos
        nenc
        (round (q * (toRational dbase)^prec))
        (prec + ilen)
        flen


formatNumberNormalized
    :: Rational
    -> Char
    -> NEnc
    -> Int
    -> Int
    -> Rational
    -> ShowS
formatNumberNormalized ebase expch nenc ilen flen q = do
    normalized ebase q (\e qn rest -> do
        formatNumberFixed nenc ilen flen qn $
            expch:exponentItos e rest)


formatNumberAdaptive
    :: Bool
    -> Rational
    -> Char
    -> NEnc
    -> Int
    -> Rational
    -> ShowS
formatNumberAdaptive stripneeded ebase expch nenc@(NEnc dbase _) prec q = do
    normalized ebase q (\e qn -> do
        if -4 <= e && e < prec
            then do
                let flen = prec - 1 - e
                let i = round (q * toRational dbase^flen)
                itos (stripr flen i)
            else do
                let flen = prec - 1
                let i = round (qn * toRational dbase^flen)
                (\rest -> do
                    itos (stripr flen i) $
                        expch:exponentItos e rest))
    where
    itos (flen, x) = do
        if flen == 0
            then if stripneeded
                then generalItos nenc x 1 (-1)
                else generalItos nenc x 1 0
            else generalItos nenc x (flen+1) flen
    stripr flen i
        | stripneeded && i `mod` dbase == 0 && flen > 0 = do
            stripr (flen-1) (i `div` dbase)
        | otherwise = (flen, i)


discrimRational
    :: Rational
    -> (Rational -> a)
    -> (Rational -> a)
    -> a
discrimRational q onPos onNeg
    | q < 0 = onNeg (-q)
    | otherwise = onPos q


discrimDouble
    :: Double
    -> a
    -> a
    -> a
    -> (Rational -> a)
    -> (Rational -> a)
    -> a
discrimDouble d onNan onPosInf onNegInf onPosFinite onNegFinite
    | isNaN d = onNan
    | isInfinite d && d < 0 = onNegInf
    | isInfinite d = onPosInf
    | isNegativeZero d = onNegFinite 0
    | d < 0 = onNegFinite (-toRational d)
    | otherwise = onPosFinite (toRational d)


discrimLuaNumber
    :: LuaValue q s
    -> a
    -> a
    -> a
    -> (Rational -> a)
    -> (Rational -> a)
    -> a
    -> a
discrimLuaNumber
        value onNan onPosInf onNegInf onPosFinite onNegFinite onOther = do
    case value of
        LInteger x ->
            discrimRational (toRational x) onPosFinite onNegFinite
        LRational q ->
            discrimRational q onPosFinite onNegFinite
        LDouble d ->
            discrimDouble d onNan onPosInf onNegInf onPosFinite onNegFinite
        _ -> onOther


formatLuaNumber
    :: ShowS
    -> ShowS
    -> ShowS
    -> (Rational -> ShowS)
    -> LuaValue q s
    -> Maybe (ShowS -> ShowS)
formatLuaNumber nans infs pluss numberfmt value = do
    discrimLuaNumber value
        (Just $ (\zp -> zp . nans))
        (Just $ (\zp -> zp . pluss . infs))
        (Just $ (\zp -> zp . ('-':) . infs))
        (\q -> Just (\zp -> pluss . zp . numberfmt q))
        (\q -> Just (\zp -> ('-':) . zp . numberfmt q))
        Nothing


roundLuaNumber
    :: LuaValue q s
    -> Maybe Integer
roundLuaNumber value = do
    discrimLuaNumber value
        (Just $ 0)
        (Just $ 0)
        (Just $ 0)
        (\q -> Just $ round q)
        (\q -> Just $ - round q)
        Nothing


formatStringLiteral
    :: BSt.ByteString
    -> ShowS
formatStringLiteral s = ('"':) . qstr (BSt.unpack s) . ('"':)
    where
    qstr "" rest = rest
    qstr (c:cs) rest
        | c == '\n' =
            '\\':'\n'
            :qstr cs rest
        | c < '\10' && not (dstart cs) =
            '\\'
            :d cn
            :qstr cs rest
        | c < '\32' && not (dstart cs) =
            '\\'
            :d (cn `div` 10)
            :d (cn `mod` 10)
            :qstr cs rest
        | c < '\32' =
            '\\':'0'
            :d (cn `div` 10)
            :d (cn `mod` 10)
            :qstr cs rest
        | c == '\127' =
            '\\':'1':'2':'7'
            :qstr cs rest
        | c == '\\' = '\\':'\\':qstr cs rest
        | c == '"' = '\\':'"':qstr cs rest
        | otherwise = c:qstr cs rest
        where
        cn = fromEnum c
        d n = toEnum (n + fromEnum '0')
    dstart "" = False
    dstart (c:_) = '0' <= c && c <= '9'


formatPrintField
    :: PrintField
    -> LuaValue q s
    -> LuaState q s (ShowS -> ShowS -> ShowS -> ShowS)
formatPrintField pf value = do
    -- luaLiftIO $ print pf
    case pfWidth pf of
        Just width | width > 1000000 -> luaError $ errWrongFormat
        _ -> return ()
    case pfPrec pf of
        Just prec | prec > 1000000 -> luaError $ errWrongFormat
        _ -> return ()
    case (kind, lorb False True) of
        ('a', _) -> do
            let aprec = do
                case pfPrec pf of
                    Just 0
                        | pfAlt pf -> 0
                        | otherwise -> -1
                    Just prec -> prec
                    Nothing -> 13
            nss <- assertType $ formatLuaNumberLorB pf
                (formatNumberNormalized
                    2 (lorb 'p' 'P') (lorb nenc16L nenc16B) 1 aprec)
                value
            let nss' zp = if isFinite
                    then nss (('0':) . (lorb 'x' 'X':) . zp)
                    else nss zp
            return (\lp zp rp -> lp . nss' zp . rp)
        ('c', False) -> do
            mchar <- discrimLuaNumber value
                (return $ Nothing)
                (return $ Nothing)
                (return $ Nothing)
                (\q -> return $ do
                    guard $ denominator q == 1
                    let n = numerator q
                    guard $ 0 <= n && n < 256
                    Just $ toEnum (fromEnum n))
                (\_ -> return $ Nothing)
                raiseWrongType
            case mchar of
                Nothing -> return (\lp zp rp -> lp . zp . rp)
                Just ch -> return (\lp zp rp -> lp . zp . (ch:) . rp)
        ('d', False) -> do
            vali <- assertType $ roundLuaNumber value
            let nss zp = if vali >= 0
                    then pluss . zp . generalItos
                        nenc10 vali integerprec (-1)
                    else ('-':) . zp . generalItos
                        nenc10 (-vali) integerprec (-1)
            return $ integerpad (\lp zp rp -> lp . nss zp . rp)
        ('e', _) -> do
            nss <- assertType $ formatLuaNumberLorB pf
                (formatNumberNormalized 10 (lorb 'e' 'E') nenc10 1 floatprec)
                value
            return (\lp zp rp -> lp . nss zp . rp)
        ('f', _) -> do
            nss <- assertType $ formatLuaNumberLorB pf
                (formatNumberFixed nenc10 1 floatprec)
                value
            return (\lp zp rp -> lp . nss zp . rp)
        ('g', _) -> do
            let prec = maybe 6 (max 1) $ pfPrec pf
            nss <- assertType $ formatLuaNumberLorB pf
                (formatNumberAdaptive
                    (not $ pfAlt pf) 10 (lorb 'e' 'E') nenc10 prec)
                value
            return (\lp zp rp -> lp . nss zp . rp)
        ('i', False) -> do
            vali <- assertType $ roundLuaNumber value
            let nss zp = if vali >= 0
                    then pluss . zp . generalItos
                        nenc10 vali integerprec (-1)
                    else ('-':) . zp . generalItos
                        nenc10 (-vali) integerprec (-1)
            return $ integerpad (\lp zp rp -> lp . nss zp . rp)
        ('o', False) -> do
            vali <- assertType $ roundLuaNumber value
            let abss = generalItos nenc8 (abs vali) integerprec (-1)
            let abss' = case abss "" of
                    '0':_ -> abss
                    _ -> if pfAlt pf
                        then ('0':) . abss
                        else abss
            let nss zp = if vali >= 0
                    then pluss . zp . abss'
                    else ('-':) . zp . abss'
            return $ integerpad (\lp zp rp -> lp . nss zp . rp)
        ('q', _) -> do
            let tellq = return (\lp zp rp -> lp . zp . shows value . rp)
            let tells s = return (\lp zp rp -> lp . zp . s . rp)
            case value of
                LNil -> tellq
                LBool _ -> tellq
                LInteger _ -> tellq
                LRational _ -> tellq
                LDouble d
                    | isNaN d -> tells ("(0/0)"++)
                    | isInfinite d && d > 0 -> tells ("(1/0)"++)
                    | isInfinite d -> tells ("(-1/0)"++)
                    | otherwise -> tellq
                LString str -> tells $ formatStringLiteral str
                _ -> raiseWrongType
        ('s', _) -> do
            str <- luaAsString value
            let ss = case pfPrec pf of
                    Nothing -> (BSt.unpack str ++)
                    Just prec -> (take prec (BSt.unpack str) ++)
            return (\lp zp rp -> lp . zp . ss . rp)
        ('u', False) -> do
            vali <- assertType $ roundLuaNumber value
            let nss zp = if vali >= 0
                    then pluss . zp . generalItos
                        nenc10 vali integerprec (-1)
                    else ('-':) . zp . generalItos
                        nenc10 (-vali) integerprec (-1)
            return $ integerpad (\lp zp rp -> lp . nss zp . rp)
        ('x', _) -> do
            vali <- assertType $ roundLuaNumber value
            let prefixs = if pfAlt pf then ('0':) . (lorb 'x' 'X':) else id
            let nss zp = case vali `compare` 0 of
                    GT -> pluss . prefixs . zp . generalItos
                        (lorb nenc16L nenc16B) vali integerprec (-1)
                    EQ -> pluss . zp . if integerprec > 0 then ('0':) else id
                    LT -> ('-':) . prefixs . zp . generalItos
                        (lorb nenc16L nenc16B) (-vali) integerprec (-1)
            return $ integerpad (\lp zp rp -> lp . nss zp . rp)
        _ -> luaError $ errWrongSlot $ pfKind pf
        where
        raiseWrongType = luaError $
            errWrongType (BSt.pack ['%', pfKind pf]) value
        assertType = maybe raiseWrongType return
        integerpad ff lp zp rp
            | isJust (pfPrec pf) = do
                let zp' = map (\_ -> ' ') $ zp ""
                ff (lp . (zp' ++)) id rp
            | otherwise = ff lp zp rp
        integerprec = fromMaybe 1 $ pfPrec pf
        floatprec = do
            case pfPrec pf of
                Just 0 -> if pfAlt pf
                    then 0
                    else -1
                Just prec -> prec
                Nothing -> 6
        isFinite = discrimLuaNumber value
            False
            False
            False
            (\_ -> True)
            (\_ -> True)
            False
        formatLuaNumberLorB _ = lorb
            (formatLuaNumber ("nan" ++) ("inf" ++) pluss)
            (formatLuaNumber ("NAN" ++) ("INF" ++) pluss)
        pluss = (maybe id (:) $ pfSign pf)
        kind = lorb
            (pfKind pf)
            (toEnum (fromEnum (pfKind pf) + (fromEnum 'a' - fromEnum 'A')))
        lorb l b
            | 'A' <= pfKind pf && pfKind pf <= 'Z' = b
            | otherwise = l


formatFields
    :: [Either B.ByteString PrintField]
    -> [LuaValue q s]
    -> (B.ByteString -> B.ByteString)
    -> LuaState q s (B.ByteString -> B.ByteString)
formatFields fields args cont = do
    case fields of
        [] -> return $ cont
        Left bstr:fs -> do
            let cont' = seq bstr $ cont . (bstr <>)
            formatFields fs args cont'
        Right pf:fs -> do
            let (a, as) = fromMaybe (LNil, []) $ uncons args
            field <- formatPrintField pf a
            let fieldmin = field id id id ""
            let mpadlen = do
                width <- pfWidth pf
                let flen = length $ fieldmin
                guard $ width > flen
                Just $ width - flen
            let fieldstr = case mpadlen of
                    Nothing -> fieldmin
                    Just padlen -> do
                        case pfPad pf of
                            PTLeft -> field (replicate padlen ' ' ++) id id ""
                            PTZero -> field id (replicate padlen '0' ++) id ""
                            PTRight -> field id id (replicate padlen ' ' ++) ""
            let bstr = B.pack fieldstr
            let cont' = seq bstr $ cont . (bstr <>)
            formatFields fs as cont'


luaFormat
    :: BSt.ByteString
    -> [LuaValue q s]
    -> LuaState q s BSt.ByteString
luaFormat fmt args = do
    fields <- parseFormat fmt
    bstrs <- formatFields fields args id
    return $ B.toStrict $ bstrs ""


type Matcher q s
    =  BSt.ByteString
    -> Int
    -> ([Int] -> [LuaValue q s] -> [LuaValue q s])
    -> Maybe (Int, [LuaValue q s])


matchFinalAny :: Matcher q s
matchFinalAny _ pos caps = do
    Just $ (pos, caps [] [])


matchFinalEnd :: Matcher q s
matchFinalEnd source pos caps = do
    if pos == BSt.length source
        then Just $ (pos, caps [] [])
        else Nothing


matchStart :: Matcher q s -> Matcher q s
matchStart next source pos caps = do
    guard $ pos == 0
    next source pos caps


matchClose :: Matcher q s -> Matcher q s
matchClose next source pos caps = do
    let caps' ends = caps (pos:ends)
    next source pos caps'


matchOpen :: Matcher q s -> Matcher q s
matchOpen next source pos caps = do
    let caps' ends after = do
        let (mycap, endrest) = case uncons ends of
                Just (epos, endrest') -> do
                    let str = BSt.drop pos $ BSt.take epos $ source
                    (LString str, endrest')
                Nothing -> (LNil, ends)
        caps endrest (mycap:after)
    next source pos caps'


matchMarker :: Matcher q s -> Matcher q s
matchMarker next source pos caps = do
    let caps' ends after = do
        let mycap = LInteger $ toInteger pos + 1
        caps ends (mycap:after)
    next source pos caps'


matchOne
    :: (Char -> Bool)
    -> Matcher q s -> Matcher q s
matchOne test next source pos caps = do
    let stail = BSt.drop pos $ source
    (ch, _) <- BSt.uncons $ stail
    guard $ test ch
    next source (pos+1) caps


matchZeroOrOne
    :: (Char -> Bool)
    -> Matcher q s -> Matcher q s
matchZeroOrOne test next source pos caps = do
            matchOne test next source pos caps
    `mplus` next source pos caps


matchZeroOrMore
    :: (Char -> Bool)
    -> Matcher q s -> Matcher q s
matchZeroOrMore test next source pos caps = do
    let stail = BSt.drop pos $ source
    let maxprefix = BSt.length $ BSt.takeWhile test $ stail
    tryMatch $ pos + maxprefix
    where
    tryMatch n
        | n < pos = Nothing
        | otherwise = do
                    next source n caps
            `mplus` tryMatch (n-1)


matchOneOrMore
    :: (Char -> Bool)
    -> Matcher q s -> Matcher q s
matchOneOrMore test next source pos caps = do
    let stail = BSt.drop pos $ source
    let maxprefix = BSt.length $ BSt.takeWhile test $ stail
    tryMatch $ pos + maxprefix
    where
    tryMatch n
        | n <= pos = Nothing
        | otherwise = do
                    next source n caps
            `mplus` tryMatch (n-1)


matchMinimal
    :: (Char -> Bool)
    -> Matcher q s -> Matcher q s
matchMinimal test next source pos caps = do
            next source pos caps
    `mplus` (do
        let stail = BSt.drop pos $ source
        (ch, _) <- BSt.uncons $ stail
        guard $ test ch
        matchMinimal test next source (pos+1) caps)


matchLiteral
    :: BSt.ByteString
    -> Matcher q s -> Matcher q s
matchLiteral lit next source pos caps = do
    let stail = BSt.drop pos $ source
    _ <- BSt.stripPrefix lit $ stail
    next source (pos + BSt.length lit) caps


matchCapture
    :: Int
    -> Matcher q s -> Matcher q s
matchCapture capindex next source pos caps = do
    (LString lit, _) <- uncons $ drop capindex $ caps [] []
    matchLiteral lit next source pos caps


matchBracket
    :: Char
    -> Char
    -> Matcher q s -> Matcher q s
matchBracket upc downc next source pos caps = do
    (ch, stail) <- BSt.uncons $ BSt.drop pos $ source
    guard $ ch == upc
    walk (1::Int) stail (pos + 1)
    where
    walk 0 _ pos' = next source pos' caps
    walk n stail pos' = do
        case BSt.uncons stail of
            Just (ch, stail')
                | ch == downc -> walk (n-1) stail' (pos'+1)
                | ch == upc -> walk (n+1) stail' (pos'+1)
                | otherwise -> walk n stail' (pos'+1)
            Nothing -> Nothing


matchFrontier
    :: (Char -> Bool)
    -> Matcher q s -> Matcher q s
matchFrontier test next source pos caps = do
    let (left, rightstr) = case pos of
            0 -> ('\0', source)
            _ -> fromJust $ BSt.uncons $ BSt.drop (pos-1) $ source
    let right = case BSt.uncons $ rightstr of
            Just (h, _) -> h
            Nothing -> '\0'
    guard $ not (test left) && test right
    next source pos caps


data PatternParserState q s = PatternParserState {
    ppsCaptures :: Int -> [Maybe Bool] -> [Maybe Bool],
    ppsChars :: Maybe (String -> String),
    ppsElems :: Matcher q s -> Matcher q s,
    ppsComplexity :: !Int}


type PatternParser q s = Parsec BSt.ByteString (PatternParserState q s)


readPatternCheckComplexity :: PatternParser q s ()
readPatternCheckComplexity = do
    pps <- getState
    if ppsComplexity pps > 1000
        then fail "Pattern is too complex"
        else return ()


readPatternPushChar :: Char -> PatternParser q s ()
readPatternPushChar ch = do
    modifyState (\pps -> do
        let left = fromMaybe id $ ppsChars pps
        pps {
            ppsChars = Just $ left . (ch:)})


readPatternPushElement :: (Matcher q s -> Matcher q s) -> PatternParser q s ()
readPatternPushElement el = do
    modifyState (\pps -> do
        case ppsChars pps of
            Nothing -> do
                pps {
                    ppsElems = ppsElems pps . el,
                    ppsComplexity = ppsComplexity pps + 1}
            Just buf -> do
                let charElem = matchLiteral (BSt.pack $ buf "")
                pps {
                    ppsChars = Nothing,
                    ppsElems = ppsElems pps . charElem . el,
                    ppsComplexity = ppsComplexity pps + 1})
    readPatternCheckComplexity


readPatternCategory
    :: PatternParser q s (Either (Char -> Bool) Char)
readPatternCategory = do
    n <- anyChar
    case splitCase n of
        ('a', df) -> return $ Left $ df testLetter
        ('c', df) -> return $ Left $ df testControl
        ('d', df) -> return $ Left $ df testDigit
        ('g', df) -> return $ Left $ df testPrintable
        ('l', df) -> return $ Left $ df testLowercase
        ('p', df) -> return $ Left $ df testPunctuation
        ('s', df) -> return $ Left $ df testSpace
        ('u', df) -> return $ Left $ df testUppercase
        ('w', df) -> return $ Left $ df testAlphanum
        ('x', df) -> return $ Left $ df testHex
        _ -> case n of
            'z' -> return $ Right $ '\0'
            'Z' -> return $ Left $ (/='\0')
            _ -> return $ Right $ n
    where
    splitCase n
        | 'A' <= n && n <= 'Z' = do
            (toEnum (fromEnum n + (fromEnum 'a' - fromEnum 'A')), (not .))
        | otherwise = (n, id)
    testLetter c = within 'a' 'z' c || within 'A' 'Z' c
    testControl c = c <= '\31' || c == '\127'
    testDigit c = within '0' '9' c
    testPrintable c = within '\33' '\126' c
    testLowercase c = within 'a' 'z' c
    testPunctuation c =
           within '\33' '\47' c
        || within '\58' '\64' c
        || within '\91' '\96' c
        || within '\123' '\126' c
    testSpace c = within '\9' '\13' c || c == ' '
    testUppercase c = within 'A' 'Z' c
    testAlphanum c = within 'a' 'z' c || within 'A' 'Z' c || within '0' '9' c
    testHex c = within 'a' 'f' c || within 'A' 'F' c || within '0' '9' c
    within a b c = a <= c && c <= b


readPatternClass
    :: PatternParser q s (Either (Char -> Bool) Char)
readPatternClass = do
    _ <- char '['
    inverse <- option False (True <$ char '^')
    set <- readElems Nothing
    case (inverse, set) of
        (_, Nothing) -> do
            return $ Left $ const inverse
        (False, Just (Right ch)) -> do
            return $ Right $ ch
        (False, Just (Left func)) -> do
            let list = listArray ('\0', '\255') $
                    map func ['\0' .. '\255']
            let func' x = (list :: UArray Char Bool) ! x
            return $ seq list $ Left $ func'
        (True, Just (Right ch)) -> do
            return $ Left $ (/=ch)
        (True, Just (Left func)) -> do
            let list = listArray ('\0', '\255') $
                    map (not . func) ['\0' .. '\255']
            let func' x = (list :: UArray Char Bool) ! x
            return $ seq list $ Left $ func'
    where
    readElems set = do
        el <- readCategory <|> readRange <|> readSingle
        let set' = combine set el
        (char ']' >> return set') <|> readElems set'
    readCategory = do
        _ <- char '%'
        cat <- readPatternCategory
        return $ Just cat
    readRange = try (do
        first <- anyChar
        _ <- char '-'
        final <- noneOf "]%"
        case first `compare` final of
            LT -> return $ Just $ Left (\x -> first <= x && x <= final)
            EQ -> return $ Just $ Right first
            GT -> return $ Nothing)
    readSingle = do
        ch <- anyChar
        return $ Just $ Right ch
    combine set Nothing = set
    combine Nothing (Just el) = Just el
    combine (Just el1) (Just el2) = do
        case (el1, el2) of
            (Right c1, Right c2)
                | c1 == c2 -> Just (Right c1)
                | otherwise -> Just (Left (\x -> x==c1 || x==c2))
            (Right c1, Left f2) -> do
                Just (Left (\x -> x == c1 || f2 x))
            (Left f1, Right c2) -> do
                Just (Left (\x -> f1 x || x == c2))
            (Left f1, Left f2) -> do
                Just (Left (\x -> f1 x || f2 x))


readPatternSpan :: PatternParser q s ()
readPatternSpan = do
    unit <- choice [
        readPatternClass,
        char '%' >> readPatternCategory,
        Left (const True) <$ char '.',
        Right <$> anyChar]
    factor <- optionMaybe $ choice [
        matchZeroOrMore <$ char '*',
        matchOneOrMore <$ char '+',
        matchMinimal <$ char '-',
        matchZeroOrOne <$ char '?']
    case (unit, factor) of
        (Right ch, Nothing) -> readPatternPushChar ch
        (Right ch, Just mf) -> readPatternPushElement $ mf (==ch)
        (Left test, Nothing) -> readPatternPushElement $ matchOne test
        (Left test, Just mf) -> readPatternPushElement $ mf test


readPatternFrontier :: PatternParser q s ()
readPatternFrontier = do
    _ <- try $ string "%f"
    unit <- readPatternClass
    case unit of
        Right ch -> readPatternPushElement $ matchFrontier (==ch)
        Left test -> readPatternPushElement $ matchFrontier test


readPatternBracket :: PatternParser q s ()
readPatternBracket = do
    _ <- try $ string "%b"
    upchar <- anyChar
    downchar <- anyChar
    readPatternPushElement $ matchBracket upchar downchar


readPatternGroupOpen :: PatternParser q s ()
readPatternGroupOpen = do
    _ <- char '('
    isMarker <- option False $ True <$ char ')'
    if isMarker
        then do
            modifyState (\pps -> do
                let caps' level after = do
                    ppsCaptures pps level (Just False:after)
                pps {
                    ppsCaptures = caps'})
            readPatternPushElement $ matchMarker
        else do
            modifyState (\pps -> do
                let caps' level after = do
                    if level > 0
                        then ppsCaptures pps (level-1) (Just True:after)
                        else ppsCaptures pps level (Nothing:after)
                pps {
                    ppsCaptures = caps'})
            readPatternPushElement $ matchOpen


readPatternGroupClose :: PatternParser q s ()
readPatternGroupClose = do
    _ <- char ')'
    pps <- getState
    let isValid = Nothing `elem` ppsCaptures pps 0 []
    unless isValid $ fail "Invalid pattern capture"
    putState (do
        let caps' level after = do
            ppsCaptures pps (level+1) after
        pps {
            ppsCaptures = caps'})
    readPatternPushElement $ matchClose


readPatternCapture :: PatternParser q s ()
readPatternCapture = do
    ich <- try (char '%' >> satisfy (\x -> '0' <= x && x <= '9'))
    let capindex = fromEnum ich - fromEnum '1'
    caps <- ppsCaptures <$> getState
    let mcap = do
        guard $ capindex >= 0
        (Just True, _) <- uncons $ drop capindex $ caps 0 []
        return ()
    case mcap of
        Nothing -> fail $ "Invalid capture index %" ++ [ich]
        Just () -> readPatternPushElement $ matchCapture capindex


readPatternEnd :: PatternParser q s (Matcher q s)
readPatternEnd = do
    final <-     (matchFinalAny <$ eof)
             <|> try (matchFinalEnd <$ (char '$' >> eof))
    caps <- ppsCaptures <$> getState
    when (Nothing `elem` caps 0 []) $
        fail $ "Unfinished capture"
    readPatternPushElement $ id
    els <- ppsElems <$> getState
    return $ els final


readPattern :: PatternParser q s (Matcher q s)
readPattern = do
    choice [
        readPatternEnd,
        readPatternFrontier >> readPattern,
        readPatternBracket >> readPattern,
        readPatternGroupOpen >> readPattern,
        readPatternGroupClose >> readPattern,
        readPatternCapture >> readPattern,
        readPatternSpan >> readPattern] <?> ""


readPatternStart :: PatternParser q s (Matcher q s)
readPatternStart = do
    optional $ do
        _ <- char '^'
        readPatternPushElement $ matchStart
    readPattern


parsePattern
    :: Bool
    -> BSt.ByteString
    -> Either
        BSt.ByteString
        (BSt.ByteString -> Int -> Maybe (Int, [LuaValue q s]))
parsePattern skipStart source = do
    let readf = do
        if skipStart
            then readPattern
            else readPatternStart
    let initstate = PatternParserState {
        ppsCaptures = (\_ -> id),
        ppsChars = Nothing,
        ppsElems = id,
        ppsComplexity = 0}
    case runParser readf initstate "" source of
        Left err -> Left $ BSt.pack $ show err
        Right matchers -> Right $ (\buf spos -> matchers buf spos (const id))


luaBreakString
    :: Bool
    -> BSt.ByteString
    -> BSt.ByteString
    -> Int
    -> LuaState q s [Either BSt.ByteString [LuaValue q s]]
luaBreakString skipStart source pattern pos = do
    case parsePattern skipStart pattern of
        Left err -> luaError $ LString err
        Right matcher
            | pos <= BSt.length source -> return $ walk matcher 0 pos
            | otherwise -> return $ [Left source]

    where

    walk matcher prevpos curpos = do
        case matcher source curpos of
            Nothing
                | prevpos == BSt.length source -> []
                | curpos == BSt.length source -> do
                    [Left $ BSt.drop prevpos $ source]
                | otherwise -> walk matcher prevpos (curpos+1)
            Just (epos, capsinter)
                | epos == prevpos -> do
                    let next1 = walk matcher prevpos (curpos+1)
                    if epos == 0
                        then Right caps:next1
                        else next1
                | curpos == prevpos -> Right caps:next
                | otherwise -> Left leftstr:Right caps:next

                where

                leftstr = BSt.drop prevpos $ BSt.take curpos $ source

                caps = LString capstr:capsinter

                capstr = BSt.drop curpos $ BSt.take epos $ source

                next
                    | epos == BSt.length source = []
                    | otherwise = walk matcher epos epos


luaLexInteger :: BSt.ByteString -> Integer -> LuaValue q s
luaLexInteger buf base
    | 2 <= base && base <= 36 = readStart $ BSt.unpack buf
    | otherwise = LNil

    where

    readStart str = do
        case str of
            c:rest | '\9' <= c && c <= '\13' -> readStart rest
            ' ':rest-> readStart rest
            '+':rest -> readFirst 1 rest
            '-':rest -> readFirst (-1) rest
            rest -> readFirst 1 rest

    readFirst sign str = do
        case str of
            c:rest
                | let d = digit c, d < base ->
                    readDigits sign d rest
            _ -> LNil

    readDigits sign num str = do
        case str of
            c:rest
                | let d = digit c, d < base ->
                    readDigits sign (num*base + d) rest
            rest -> readEnd (sign*num) rest

    readEnd x str = do
        case str of
            "" -> LInteger x
            c:rest | '\9' <= c && c <= '\13' -> readEnd x rest
            ' ':rest -> readEnd x rest
            _ -> LNil

    digit c
        | '0' <= c && c <= '9' = toInteger $ fromEnum c - fromEnum '0'
        | 'a' <= c && c <= 'z' = toInteger $ fromEnum c - (fromEnum 'a' - 10)
        | 'A' <= c && c <= 'Z' = toInteger $ fromEnum c - (fromEnum 'A' - 10)
        | otherwise = 100


binpackParse
    :: (MonadTrans mt, Monad (mt (Either BSt.ByteString)))
    => BSt.ByteString
    -> s
    -> (Bool -> Int -> Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (               Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (               Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (        Int -> Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (        Int -> Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (               Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (        Int -> Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> (               Int -> Bool -> s -> mt (Either BSt.ByteString) s)
    -> mt (Either BSt.ByteString) s
binpackParse pattern initstate
        onInteger onFloat onDouble
        onLengthString onFixedString onZeroString
        onAlignPad onBytePad = do
    process 1 False initstate $ BSt.unpack pattern

    where

    process !align !bigendian !s buf = do
        case buf of
            '<':rest -> process align False s rest
            '>':rest -> process align True s rest
            '=':rest -> process align False s rest
            '!':rest -> do
                sizenum 4 rest (\val rest' -> do
                    process val bigendian s rest')
            'b':rest -> do
                send align bigendian s rest $
                    onInteger True 1
            'B':rest -> do
                send align bigendian s rest $
                    onInteger False 1
            'h':rest -> do
                send align bigendian s rest $
                    onInteger True 2
            'H':rest -> do
                send align bigendian s rest $
                    onInteger False 2
            'l':rest -> do
                send align bigendian s rest $
                    onInteger True 8
            'L':rest -> do
                send align bigendian s rest $
                    onInteger False 8
            'j':rest -> do
                send align bigendian s rest $
                    onInteger True 8
            'J':rest -> do
                send align bigendian s rest $
                    onInteger False 8
            'T':rest -> do
                send align bigendian s rest $
                    onInteger False 4
            'i':rest -> do
                sizenum 4 rest (\val rest' -> do
                    send align bigendian s rest' $ onInteger True val)
            'I':rest -> do
                sizenum 4 rest (\val rest' -> do
                    send align bigendian s rest' $ onInteger False val)
            'f':rest -> do
                send align bigendian s rest $
                    onFloat
            'd':rest -> do
                send align bigendian s rest $
                    onDouble
            'n':rest -> do
                send align bigendian s rest $
                    onDouble
            'c':rest -> do
                lengthnum rest (\val rest' -> do
                    send align bigendian s rest' $ onFixedString val)
            'z':rest -> do
                send align bigendian s rest $
                    onZeroString
            's':rest -> do
                sizenum 4 rest (\val rest' -> do
                    send align bigendian s rest' $ onLengthString val)
            'x':rest -> send align bigendian s rest $ onBytePad
            'X':rest -> sendAlignPad align bigendian s rest
            ' ':rest -> process align bigendian s rest
            [] -> return $ s
            _ -> lift $ Left "Invalid format option"

    sendAlignPad align bigendian s buf = do
        case buf of
            'b':rest -> send align bigendian s rest $ onAlignPad 1
            'B':rest -> send align bigendian s rest $ onAlignPad 1
            'h':rest -> send align bigendian s rest $ onAlignPad 2
            'H':rest -> send align bigendian s rest $ onAlignPad 2
            'l':rest -> send align bigendian s rest $ onAlignPad 8
            'L':rest -> send align bigendian s rest $ onAlignPad 8
            'j':rest -> send align bigendian s rest $ onAlignPad 8
            'J':rest -> send align bigendian s rest $ onAlignPad 8
            'T':rest -> send align bigendian s rest $ onAlignPad 4
            'i':rest -> do
                sizenum 4 rest (\val rest' -> do
                    send align bigendian s rest' $ onAlignPad val)
            'I':rest -> do
                sizenum 4 rest (\val rest' -> do
                    send align bigendian s rest' $ onAlignPad val)
            'f':rest -> send align bigendian s rest $ onAlignPad 4
            'd':rest -> send align bigendian s rest $ onAlignPad 8
            'n':rest -> send align bigendian s rest $ onAlignPad 8
            's':rest -> do
                lengthnum rest (\val rest' -> do
                    send align bigendian s rest' $ onAlignPad val)
            'x':rest -> send align bigendian s rest $ onAlignPad 1
            _ -> lift $ Left "Invalid next option for X"

    send align bigendian s buf act = do
        s' <- act align bigendian s
        process align bigendian s' buf

    sizenum _ (c:rest) after | Just d <- ctod c = do
        numnext d rest (\r rest' -> do
            case r of
                _
                    | 1 <= r && r <= 16 -> after r rest'
                    | otherwise -> lift $ Left $
                        BSt.pack "Integral size out of limits")
    sizenum def buf after = after def buf

    lengthnum (c:rest) after | Just d <- ctod c = do
        numnext d rest after
    lengthnum _ _ = lift $ Left "Missing length option"

    numnext !n (c:rest) after
        | Just d <- ctod c = do
            if n < 100000000
                then numnext (n*10 + d) rest after
                else lift $ Left "Given length is too large"
    numnext !n buf after = after n buf

    ctod c
        | '0' <= c && c <= '9' = Just $ fromEnum c - fromEnum '0'
        | otherwise = Nothing


binpackAlignDelta
    :: (MonadTrans mt, Monad (mt (Either BSt.ByteString)))
    => Int
    -> Int
    -> Int
    -> mt (Either BSt.ByteString) Int
binpackAlignDelta isize align total
    | let step = min isize align
    , let mask = step - 1
    , step .&. mask == 0 = return $ (- total) .&. (step-1)
    | otherwise = lift $ Left $ "Alignment is not a power of 2"


luaBinpackSize
    :: BSt.ByteString
    -> Either BSt.ByteString Int
luaBinpackSize pattern = do
    runIdentityT $ binpackParse pattern
        0
        (\_ isize align _ total -> do
            ceiln isize align total >>= checkmax . (+isize))
        (\align _ total -> ceiln 4 align total >>= checkmax . (+4))
        (\align _ total -> ceiln 8 align total >>= checkmax . (+8))
        (\_ _ _ _ -> invalidVarlen)
        (\strlen _ _ total -> checkmax $ total + strlen)
        (\_ _ _ -> invalidVarlen)
        (\isize align _ total -> ceiln isize align total >>= checkmax)
        (\_ _ total -> checkmax $ total + 1)

    where

    ceiln isize align total = do
        ad <- binpackAlignDelta isize align total
        return $ total + ad

    checkmax total
        | total > 0x7fffffff = lift $ Left "Format result is too large"
        | otherwise = return $ total

    invalidVarlen = lift $ Left "Variable-length format inside packsize"


luaBinpackWrite
    :: BSt.ByteString
    -> [LuaValue q s]
    -> Either BSt.ByteString BSt.ByteString
luaBinpackWrite pattern initargs = do
    (_, charbuf) <- execRWST
        (binpackParse pattern
            ()
            onInteger onFloat onDouble
            onLengthString onFixedString onZeroString
            onAlignPad onBytePad)
        () (0, zip (initargs ++ repeat LNil) [(2::Int)..])
    Right $ BSt.pack $ appEndo charbuf []

    where

    onInteger signed isize align bigendian _ = do
        (vali, n) <- poparg "integer" luaToInteger
        case encode signed isize vali of
            Nothing -> invalidArgVal n "integer overflow"
            Just chars -> do
                padUntil isize align
                write $ endian bigendian $ chars

    onFloat align bigendian _ = do
        (vald, _) <- poparg "number" luaToDouble
        let valf = double2Float vald
        let vali = castFloatToWord32 valf
        let Just chars = encode False 4 vali
        padUntil 4 align
        write $ endian bigendian $ chars

    onDouble align bigendian _ = do
        (vald, _) <- poparg "number" luaToDouble
        let vali = castDoubleToWord64 vald
        let Just chars = encode False 8 vali
        padUntil 8 align
        write $ endian bigendian $ chars

    onLengthString isize align bigendian _ = do
        (str, n) <- poparg "string" luaToString
        let vali = BSt.length str
        case encode False isize vali of
            Nothing -> invalidArgVal n
                "string length does not fit in given size"
            Just chars -> do
                padUntil isize align
                write $ endian bigendian $ chars
                write $ BSt.unpack str

    onFixedString strlen _ _ _ = do
        (str, n) <- poparg "string" luaToString
        if BSt.length str > strlen
            then invalidArgVal n "string longer than given size"
            else write $
                BSt.unpack str ++ replicate (strlen - BSt.length str) '\0'

    onZeroString _ _ _ = do
        (str, n) <- poparg "string" luaToString
        if BSt.elem '\0' str
            then invalidArgVal n "string contains zeroes"
            else write $ BSt.unpack str ++ "\0"

    onAlignPad isize align _ _ = do
        padUntil isize align

    onBytePad _ _ _ = do
        write "\0"

    padUntil isize align = do
        (total, _) <- get
        ad <- binpackAlignDelta isize align total
        case ad of
            0 -> return ()
            _ -> write $ replicate ad '\0'

    write chars = do
        (total, args) <- get
        put $ (total + length chars, args)
        tell $ Endo (chars ++)

    poparg expected parser = do
        (total, args) <- get
        let (arg, n):rest = args
        case parser arg of
            Just result -> do
                put (total, rest)
                return $ (result, n)
            Nothing -> invalidArgType n expected arg

    endian False xs = xs
    endian True xs = reverse xs

    encode :: (Bits a, Enum a, Num a, Ord a) => Bool -> Int -> a -> Maybe [Char]
    encode signed isize b
        | signed && isize == 1 && (-128 <= b && b <= 127) = Just [c]
        | not signed && isize == 1 && (0 <= b && b <= 255) = Just [c]
        | isize > 1 = (c:) <$> encode signed (isize-1) (shiftR b 8)
        | otherwise = Nothing
        where
        c = (toEnum . fromEnum) (b .&. 0xff) :: Char

    invalidArgType n expected got = lift $ Left $ BSt.concat [
        "Expected a ", expected, " for packed value #",
        BSt.pack (show n), ", got a ", luaTypename got, " instead"]

    invalidArgVal n what = lift $ Left $ BSt.concat [
        "Invalid packed value #", BSt.pack (show n), ", ", what]


luaBinpackRead
    :: BSt.ByteString
    -> BSt.ByteString
    -> Int
    -> Either BSt.ByteString ([LuaValue q s] -> [LuaValue q s], Int)
luaBinpackRead pattern source offset = do
    (offset', valuebuf) <- execRWST
        (binpackParse pattern
            ()
            onInteger onFloat onDouble
            onLengthString onFixedString onZeroString
            onAlignPad onBytePad)
        () (max offset 0)
    Right $ (appEndo valuebuf, offset')

    where

    onInteger signed isize align bigendian _ = do
        padUntil isize align
        ibytes <- consume isize
        let vali = decode signed $ endian bigendian $ BSt.unpack ibytes
        tell $ Endo (LInteger vali:)

    onFloat align bigendian _ = do
        padUntil 4 align
        ibytes <- consume 4
        let vali = decode False $ endian bigendian $ BSt.unpack ibytes
        let valf = castWord32ToFloat vali
        let vald = float2Double valf
        tell $ Endo (LDouble vald:)

    onDouble align bigendian _ = do
        padUntil 8 align
        ibytes <- consume 8
        let vali = decode False $ endian bigendian $ BSt.unpack ibytes
        let vald = castWord64ToDouble vali
        tell $ Endo (LDouble vald:)

    onLengthString isize align bigendian _ = do
        padUntil isize align
        ibytes <- consume isize
        let strlen = decode False $ endian bigendian $ BSt.unpack ibytes
        str <- consume strlen
        tell $ Endo (LString str:)

    onFixedString strlen _ _ _ = do
        str <- consume strlen
        tell $ Endo (LString str:)

    onZeroString _ _ _ = do
        total <- get
        case BSt.break (=='\0') $ BSt.drop total $ source of
            (str, rest)
                | BSt.isPrefixOf "\0" rest -> do
                    put $ total + BSt.length str + 1
                    tell $ Endo (LString str:)
                | otherwise -> invalidZeroString

    onAlignPad isize align _ _ = do
        padUntil isize align

    onBytePad _ _ _ = do
        _ <- consume 1
        return ()

    padUntil isize align = do
        total <- get
        ad <- binpackAlignDelta isize align total
        _ <- consume $ ad
        return ()

    consume n = do
        total <- get
        if total + n <= BSt.length source
            then do
                put $ total + n
                return $ BSt.take n $ BSt.drop total $ source
            else invalidSource

    endian False xs = xs
    endian True xs = reverse xs

    decode :: (Bits a, Enum a, Num a, Ord a) => Bool -> [Char] -> a
    decode signed (ch:rest)
        | null rest && signed && d >= 128 = d - 256
        | null rest = d
        | otherwise = d + shiftL (decode signed rest) 8
        where
        d = (toEnum . fromEnum) ch
    decode _ [] = 0

    invalidZeroString = lift $ Left "Unfinished zero-terminated string"

    invalidSource = lift $ Left "Data string is too short"


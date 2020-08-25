{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Interpret (
    luaDo,
    luaDo_,
    luaLoad,
    luaLoadFile,
) where


import Control.Exception
import Control.Monad.Reader
import Data.List
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Array.ST as V
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua.Common
import Lua.Debug
import Lua.SourceRange
import Lua.Translate


data LirContext q s = LirContext {
    lirxBody :: !IrBody,
    lirxVararg :: ![LuaValue q s],
    lirxUpvalues :: !(A.Array Int (LuaRef q s (LuaValue q s))),
    lirxLocals :: !(V.STArray s Int (LuaRef q s (LuaValue q s)))}


lirValue
    :: LirContext q s
    -> IrValue
    -> LuaState q s (LuaValue q s)
lirValue !ctx ira = do
    case ira of
        IANil -> return $ LNil
        IABool b -> return $ LBool b
        IAInteger i -> return $ LInteger i
        IADouble r -> return $ LDouble r
        IAString s -> return $ LString s
        IATable listira kviras -> do
            ivs <- lirList ctx listira
            kvs <- forM kviras $ \(kira, vira) -> do
                k <- lirValue ctx kira
                v <- lirValue ctx vira
                return $ (k, v)
            let ikvs = zip (map LInteger [1..]) ivs ++ kvs
            luaCreateTable ikvs
        IALocal ix -> do
            ref <- luaLiftST $ V.readArray (lirxLocals ctx) ix
            luaRead $ ref
        IAUpvalue ix -> luaRead $ lirxUpvalues ctx A.! ix
        IAIndex tableira indexira -> binary luaGet tableira indexira
        IAUnaryUnm aira -> unary luaArithUnm aira
        IAUnaryLen aira -> unary luaLen aira
        IAUnaryBNot aira -> unary luaArithBNot aira
        IABinaryPow aira bira -> binary luaArithPow aira bira
        IABinaryMul aira bira -> binary luaArithMul aira bira
        IABinaryDiv aira bira -> binary luaArithDiv aira bira
        IABinaryIDiv aira bira -> binary luaArithIDiv aira bira
        IABinaryMod aira bira -> binary luaArithMod aira bira
        IABinaryAdd aira bira -> binary luaArithAdd aira bira
        IABinarySub aira bira -> binary luaArithSub aira bira
        IABinaryConcat aira bira -> binary luaConcat aira bira
        IABinaryShl aira bira -> binary luaArithShl aira bira
        IABinaryShr aira bira -> binary luaArithShr aira bira
        IABinaryBAnd aira bira -> binary luaArithBAnd aira bira
        IABinaryBXor aira bira -> binary luaArithBXor aira bira
        IABinaryBOr aira bira -> binary luaArithBOr aira bira
        IABinaryLt aira bira -> do
            LBool <$!> binary luaCompareLt aira bira
        IABinaryGt aira bira -> do
            LBool <$> binary (flip luaCompareLt) aira bira
        IABinaryLe aira bira -> do
            LBool <$> binary luaCompareLe aira bira
        IABinaryGe aira bira -> do
            LBool <$> binary (flip luaCompareLe) aira bira
        IABinaryEq aira bira -> do
            LBool <$> binary luaCompareEq aira bira
        IABinaryNeq aira bira -> do
            (LBool . not) <$> binary luaCompareEq aira bira
        IACar listira -> do
            list <- lirList ctx listira
            case list of
                x:_ -> return $ x
                _ -> return $ LNil
        IALNot aira -> do
            (LBool . not . luaToBoolean) <$> lirValue ctx aira
        IALAnd aira bira -> do
            a <- lirValue ctx aira
            if luaToBoolean a
                then lirValue ctx bira
                else return $ a
        IALOr aira bira -> do
            a <- lirValue ctx aira
            if luaToBoolean a
                then return $ a
                else lirValue ctx bira
        IAFunction
                mlocation upvaluedefs paramdefs
                maxindex funcbody -> do
            upvalues <- forM upvaluedefs $ \(source, mdef) -> do
                ref <- case source of
                    Left ix -> return $! lirxUpvalues ctx A.! ix
                    Right ix -> luaLiftST $ V.readArray (lirxLocals ctx) ix
                return $ (mdef, ref)
            luaCreateFunction $
                lirFunction
                    mlocation upvalues paramdefs
                    maxindex funcbody
    where
    unary !lfunc !aira = do
        a <- lirValue ctx aira
        lfunc a
    binary !lfunc !aira !bira = do
        a <- lirValue ctx aira
        b <- lirValue ctx bira
        lfunc a b


lirList
    :: LirContext q s
    -> IrList
    -> LuaState q s [LuaValue q s]
lirList !ctx ira = do
    case ira of
        IAArguments -> do
            return $ lirxVararg ctx
        IACall funcira argsira -> do
            func <- lirValue ctx funcira
            args <- lirList ctx argsira
            luaCall func args
        IACallMethod objira indexira argsira -> do
            obj <- lirValue ctx objira
            index <- lirValue ctx indexira
            args <- lirList ctx argsira
            func <- luaGet obj index
            luaCall func (obj:args)
        IARange firstira limitira stepira -> do
            first <- lirValue ctx firstira
            limit <- lirValue ctx limitira
            step <- lirValue ctx stepira
            func <- luaRangeIter first limit step
            return $ [func]
        IAEmpty -> do
            return $ []
        IACons firstira restira -> do
            first <- lirValue ctx firstira
            rest <- lirList ctx restira
            return $ first:rest


lirSinkPrepare
    :: LirContext q s
    -> IrSink
    -> LuaState q s (LuaValue q s -> LuaState q s ())
lirSinkPrepare !ctx ira = do
    case ira of
        IASetLocal ix -> do
            ref <- luaLiftST $ V.readArray (lirxLocals ctx) ix
            return $ luaWrite ref
        IASetUpvalue ix -> do
            return $ luaWrite (lirxUpvalues ctx A.! ix)
        IASetIndex tableira indexira -> do
            table <- lirValue ctx tableira
            index <- lirValue ctx indexira
            return $ luaSet table index


lirLocal
    :: LirContext q s
    -> Int
    -> LuaValue q s
    -> (Maybe (SourceRange, BSt.ByteString), Bool, Int)
    -> (Int -> LuaState q s (Either (LuaFrameReturn q s) IrBlock))
    -> LuaState q s (Either (LuaFrameReturn q s) IrBlock)
lirLocal !ctx !depth !value (mdef, toclose, ix) act = do
    assert (ix == depth) (return ())
    ref <- luaAlloc $ value
    luaLiftST $ V.writeArray (lirxLocals ctx) ix $ ref
    r <- if toclose
        then do
            luaCloseAfter value $
                luadWithinLocal mdef ref $
                    act $! depth + 1
        else do
            luadWithinLocal mdef ref $
                act $! depth + 1
    luaLiftST $ V.writeArray (lirxLocals ctx) ix $ undefined
    return $ r


lirContinue
    :: LirContext q s
    -> Int
    -> Either (LuaFrameReturn q s) IrBlock
    -> LuaState q s (Either (LuaFrameReturn q s) IrBlock)
lirContinue !ctx !depth result = do
    case result of
        Right (IrBlock _ targetdepth ira) -> do
            case compare targetdepth depth of
                LT -> return $ result
                EQ -> lirAction ctx depth ira
                GT -> undefined
        _ -> return $ result


lirOpen
    :: LirContext q s
    -> Int
    -> [LuaValue q s]
    -> [(Maybe (SourceRange, BSt.ByteString), Bool, Int)]
    -> IrAction
    -> LuaState q s (Either (LuaFrameReturn q s) IrBlock)
lirOpen !ctx !depth values targets next = do
    case (values, targets) of
        (v:vs, s:s':ss) -> do
            lirLocal ctx depth v s $ \ !depth2 -> do
                lirOpen ctx depth2 vs (s':ss) next >>= lirContinue ctx depth2
        ([], s:s':ss) -> do
            lirLocal ctx depth LNil s $ \ !depth2 -> do
                lirOpen ctx depth2 [] (s':ss) next >>= lirContinue ctx depth2
        (v:_, [s]) -> do
            lirLocal ctx depth v s $ \ !depth2 -> do
                lirAction ctx depth2 next
        ([], [s]) -> do
            lirLocal ctx depth LNil s $ \ !depth2 -> do
                lirAction ctx depth2 next
        _ -> undefined


lirAction
    :: LirContext q s
    -> Int
    -> IrAction
    -> LuaState q s (Either (LuaFrameReturn q s) IrBlock)
lirAction !ctx !depth ira = do
    case ira of
        IAMark pr next -> do
            luadSetLocation $ Just pr
            lirAction ctx depth next
        IAAssign sourceira sinks next -> do
            sources <- lirList ctx sourceira
            preps <- mapM (lirSinkPrepare ctx) sinks
            zipWithM_ ($) preps (sources ++ repeat LNil)
            lirAction ctx depth next
        IAOpen sourceira targets next -> do
            sources <- lirList ctx sourceira
            lirOpen ctx depth sources targets next >>= lirContinue ctx depth
        IASequence list next -> do
            _ <- lirList ctx list
            lirAction ctx depth next
        IATest condira target next -> do
            cond <- lirValue ctx condira
            if luaToBoolean cond
                then lirAction ctx depth next
                else do
                    let Just block = find
                            (\(IrBlock ix _ _) -> ix == target)
                            (lirxBody ctx)
                    lirContinue ctx depth $ Right block
        IAJump target -> do
            let Just block = find
                    (\(IrBlock ix _ _) -> ix == target)
                    (lirxBody ctx)
            lirContinue ctx depth $ Right block
        IAReturn listira -> do
            result <- lirList ctx listira
            return $ Left $ Right $ result
        IATailCall funcira argsira -> do
            func <- lirValue ctx funcira
            args <- lirList ctx argsira
            return $ Left $ Left $ (luaCall func, args)
        IATailCallMethod objira indexira argsira -> do
            obj <- lirValue ctx objira
            index <- lirValue ctx indexira
            args <- lirList ctx argsira
            func <- luaGet obj index
            return $ Left $ Left $ (luaCall func, obj:args)


lirExecute
    :: LirContext q s
    -> LuaState q s (LuaFrameReturn q s)
lirExecute !ctx = do
    let IrBlock _ !depth !main:_ = lirxBody ctx
    result <- lirAction ctx depth main
    case result of
        Left fret -> return $ fret
        Right _ -> undefined


lirFunction
    :: Maybe (SourceRange, BSt.ByteString)
    -> [(Maybe (SourceRange, BSt.ByteString), LuaRef q s (LuaValue q s))]
    -> [Maybe (SourceRange, BSt.ByteString)]
    -> Int
    -> IrBody
    -> LuaFunction q s
lirFunction mlocation upvalues paramdefs maxindex funcbody = do
    let !upvalueArray = A.listArray
            (0, length upvalues - 1)
            (map snd upvalues)
    \args -> do
        localArray <- luaLiftST $ V.newArray (0, maxindex) undefined
        (vararg, locals) <- parseArgs 0 localArray paramdefs args
        let !lirContext = LirContext {
            lirxBody = funcbody,
            lirxVararg = vararg,
            lirxUpvalues = upvalueArray,
            lirxLocals = localArray}
        luadRunFunction mlocation upvalues locals $
            lirExecute lirContext
    where
    parseArgs _ _ [] args = do
        return $ (args, [])
    parseArgs ix localArray (mdef:ps) args = do
        let (arg, as) = fromMaybe (LNil, []) (uncons args)
        ref <- luaAlloc arg
        luaLiftST $ V.writeArray localArray ix $ ref
        (vararg, locals) <- parseArgs (ix + 1) localArray ps as
        return $ (vararg, (mdef, ref):locals)


luaLoad
    :: FilePath
    -> B.ByteString
    -> LuaValue q s
    -> LuaState q s (Either (LuaValue q s) (LuaValue q s))
luaLoad filename source fenv = do
    case translateLua filename source of
        Left err -> return $ Left $ luaPush err
        Right (maxindex, funcbody) -> do
            envref <- luaAlloc $ fenv
            let mlocation = Just (nullRange filename, "(chunk)")
            let upvalues = [(Just (nullRange filename, "_ENV"), envref)]
            let paramdefs = []
            func <- luaCreateFunction $
                lirFunction
                    mlocation upvalues paramdefs
                    maxindex funcbody
            return $ Right $ func


luaLoadFile
    :: FilePath
    -> LuaValue q s
    -> LuaState q s (Either (LuaValue q s) (LuaValue q s))
luaLoadFile filename fenv = do
    ioret <- luaTry $ luaLiftIO $ B.readFile filename
    case ioret of
        Left err -> return $ Left err
        Right source -> do
            let source' = do
                case B.uncons source of
                    Just ('#', rest) -> do
                        case B.findIndex (\x -> x=='\n' || x=='\r') rest of
                            Nothing -> B.empty
                            Just pos -> B.drop pos $ rest
                    _ -> source
            luaLoad filename source' fenv


luaDo
    :: B.ByteString
    -> LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s [LuaValue q s]
luaDo source fenv args = do
    lret <- luaLoad "" source fenv
    case lret of
        Left err -> luaError err
        Right fn -> luaCall fn args


luaDo_
    :: B.ByteString
    -> LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s ()
luaDo_ source fenv args = () <$ luaDo source fenv args

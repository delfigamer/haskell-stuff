{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Interpret (
    luaDo,
    luaDo_,
    luaLoad,
    luaLoadFile,
) where


import System.CPUTime
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
    lirxUpconsts :: !(A.Array Int (LuaValue q s)),
    lirxLocals :: !(A.Array Int (LuaRef q s (LuaValue q s))),
    lirxConsts :: !(V.STArray s Int (LuaValue q s)),
    lirxGuards :: !(V.STArray s Int (LuaValue q s))}


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
        IASlot (ISLocal ix) -> do
            luaRead $ lirxLocals ctx A.! ix
        IASlot (ISConst ix) -> do
            luaLiftST $ V.readArray (lirxConsts ctx) ix
        IASlot (ISGuard ix) -> do
            luaLiftST $ V.readArray (lirxGuards ctx) ix
        IAUpvalue ix -> luaRead $ lirxUpvalues ctx A.! ix
        IAUpconst ix -> return $ lirxUpconsts ctx A.! ix
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
                mlocation upvaluedefs upconstdefs paramdefs
                maxl maxc maxg funcbody -> do
            upvalues <- forM upvaluedefs (\(source, mdef) -> do
                let !ref = case source of
                        Left ix -> lirxUpvalues ctx A.! ix
                        Right ix -> lirxLocals ctx A.! ix
                return $ (mdef, ref))
            upconsts <- forM upconstdefs (\(source, mdef) -> do
                value <- case source of
                        Left ix -> return $! lirxUpconsts ctx A.! ix
                        Right (ISLocal ix) -> do
                            luaRead $ lirxLocals ctx A.! ix
                        Right (ISConst ix) -> do
                            luaLiftST $ V.readArray (lirxConsts ctx) ix
                        Right (ISGuard ix) -> do
                            luaLiftST $ V.readArray (lirxGuards ctx) ix
                return $ (mdef, value))
            luaCreateFunction $
                lirFunction
                    mlocation upvalues upconsts paramdefs
                    maxl maxc maxg funcbody
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


lirSink
    :: LirContext q s
    -> IrSink
    -> LuaValue q s
    -> LuaState q s ()
lirSink !ctx ira value = do
    case ira of
        IASetLocal ix -> luaWrite (lirxLocals ctx A.! ix) $ value
        IASetUpvalue ix -> luaWrite (lirxUpvalues ctx A.! ix) $ value
        IASetIndex tableira indexira -> do
            table <- lirValue ctx tableira
            index <- lirValue ctx indexira
            luaSet table index value


lirLocal
    :: LirContext q s
    -> LuaValue q s
    -> (Maybe (SourceRange, BSt.ByteString), IrSlot)
    -> LuaState q s (Either (LuaFrameReturn q s) IrAction)
    -> LuaState q s (Either (LuaFrameReturn q s) IrAction)
lirLocal !ctx !value (mdef, slot) act = do
    case slot of
        ISLocal ix -> do
            luaWrite (lirxLocals ctx A.! ix) $ value
            luadWithinLocal mdef (Left value) $ act
        ISConst ix -> do
            luaLiftST $ V.writeArray (lirxConsts ctx) ix $ value
            luadWithinLocal mdef (Left value) $ act
        ISGuard ix -> do
            luaLiftST $ V.writeArray (lirxGuards ctx) ix $ value
            luaCloseAfter value $
                luadWithinLocal mdef (Left value) $ act


lirContinue
    :: LirContext q s
    -> Either (LuaFrameReturn q s) IrAction
    -> LuaState q s (Either (LuaFrameReturn q s) IrAction)
lirContinue !ctx result = do
    case result of
        Left r -> return $ Left r
        Right ira -> lirAction ctx ira


lirOpen
    :: LirContext q s
    -> [LuaValue q s]
    -> [(Maybe (SourceRange, BSt.ByteString), IrSlot)]
    -> IrAction
    -> LuaState q s (Either (LuaFrameReturn q s) IrAction)
lirOpen !ctx values targets next = do
    case (values, targets) of
        (v:vs, s:s':ss) -> do
            lirLocal ctx v s $ do
                lirOpen ctx vs (s':ss) next >>= lirContinue ctx
        ([], s:s':ss) -> do
            lirLocal ctx LNil s $ do
                lirOpen ctx [] (s':ss) next >>= lirContinue ctx
        (v:_, [s]) -> do
            lirLocal ctx v s $ do
                lirAction ctx next
        ([], [s]) -> do
            lirLocal ctx LNil s $ do
                lirAction ctx next
        _ -> undefined


lirAction
    :: LirContext q s
    -> IrAction
    -> LuaState q s (Either (LuaFrameReturn q s) IrAction)
lirAction !ctx ira = do
    case ira of
        IAAssign sourceira sinks next -> do
            sources <- lirList ctx sourceira
            zipWithM_
                (\sink value -> do
                    lirSink ctx sink value)
                sinks
                (sources ++ repeat LNil)
            lirAction ctx next
        IAOpen sourceira targets next -> do
            sources <- lirList ctx sourceira
            lirOpen ctx sources targets next >>= lirContinue ctx
        IASequence list next -> do
            _ <- lirList ctx list
            lirAction ctx next
        IADrop [] _ -> undefined
        IADrop (s:ss) next -> do
            case ss of
                [] -> return $ Right $ next
                _ -> return $ Right $ IADrop ss next
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
        IABranch condira pos neg -> do
            cond <- lirValue ctx condira
            if luaToBoolean cond
                then lirAction ctx pos
                else lirAction ctx neg
        IABlock bix -> do
            case lookup bix $ lirxBody ctx of
                Just ira -> lirAction ctx ira
                Nothing -> lirInvalid
        IAMark pr next -> do
            luadSetLocation $ Just pr
            lirAction ctx next


lirExecute
    :: LirContext q s
    -> LuaState q s (LuaFrameReturn q s)
lirExecute !ctx = do
    let (_, !main):_ = lirxBody ctx
    result <- lirAction ctx main
    case result of
        Left fret -> return $ fret
        Right _ -> lirInvalid


lirInvalid :: LuaState q s a
lirInvalid = luaError $ LString "Invalid IR"


lirFunction
    :: Maybe (SourceRange, BSt.ByteString)
    -> [(Maybe (SourceRange, BSt.ByteString), LuaRef q s (LuaValue q s))]
    -> [(Maybe (SourceRange, BSt.ByteString), LuaValue q s)]
    -> [(Int, Maybe (SourceRange, BSt.ByteString))]
    -> Int -> Int -> Int
    -> IrBody
    -> LuaFunction q s
lirFunction mlocation upvalues upconsts paramdefs maxl maxc maxg funcbody = do
    let !upvalueArray = A.listArray
            (0, length upvalues - 1)
            (map snd upvalues)
    let !upconstArray = A.listArray
            (0, length upconsts - 1)
            (map snd upconsts)
    let !upvalueList =
            map (\(mdef, ref) -> (mdef, Right ref)) upvalues
            ++ map (\(mdef, value) -> (mdef, Left value)) upconsts
    \args -> do
        locals <- replicateM maxl $ luaAlloc LNil
        let !localArray = A.listArray
                (0, maxl - 1)
                locals
        constArray <- luaLiftST $ V.newArray (0, maxc - 1) LNil
        guardArray <- luaLiftST $ V.newArray (0, maxg - 1) LNil
        (vararg, argvarlist) <- parseArgs localArray paramdefs args
        let !lirContext = LirContext {
            lirxBody = funcbody,
            lirxVararg = vararg,
            lirxUpvalues = upvalueArray,
            lirxUpconsts = upconstArray,
            lirxLocals = localArray,
            lirxConsts = constArray,
            lirxGuards = guardArray}
        luadRunFunction mlocation upvalueList argvarlist $
            lirExecute lirContext
    where
    parseArgs _ [] args = do
        return $ (args, [])
    parseArgs localArray ((ix, mdef):ps) args = do
        let (arg, as) = fromMaybe (LNil, []) (uncons args)
        let ref = localArray A.! ix
        luaWrite ref arg
        (vararg, argvarlist) <- parseArgs localArray ps as
        return $ (vararg, (mdef, Right ref):argvarlist)


luaLoad
    :: FilePath
    -> B.ByteString
    -> LuaValue q s
    -> LuaState q s (Either (LuaValue q s) (LuaValue q s))
luaLoad filename source fenv = do
    case translateLua filename source of
        Left err -> return $ Left $ luaPush err
        Right (maxl, maxc, maxg, funcbody) -> do
            envref <- luaAlloc $ fenv
            let mlocation = Just (nullRange filename, "(chunk)")
            let upvalues = [(Just (nullRange filename, "_ENV"), envref)]
            let upconsts = []
            let paramdefs = []
            func <- luaCreateFunction $
                lirFunction
                    mlocation upvalues upconsts paramdefs
                    maxl maxc maxg funcbody
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

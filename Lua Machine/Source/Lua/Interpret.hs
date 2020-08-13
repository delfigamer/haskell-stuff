{-# LANGUAGE OverloadedStrings #-}


module Lua.Interpret (
    luaDo,
    luaDo_,
    luaLoad,
    luaLoadFile,
) where


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
    lirxBody :: IrBody,
    lirxVararg :: [LuaValue q s],
    lirxUpvalues :: A.Array Int (LuaRef q s (LuaValue q s)),
    lirxUpconsts :: A.Array Int (LuaValue q s),
    lirxLocals :: A.Array Int (LuaRef q s (LuaValue q s)),
    lirxConsts :: V.STArray s Int (LuaValue q s),
    lirxGuards :: V.STArray s Int (LuaValue q s),
    lirxStack :: [IrSlot]}


lirxStackPush :: IrSlot -> LirContext q s -> LirContext q s
lirxStackPush slot (LirContext b va uv uc ls cs gs st) = do
    LirContext b va uv uc ls cs gs (slot:st)


type LirState q s = ReaderT (LirContext q s) (LuaState q s)


lirValue
    :: IrValue
    -> LirState q s (LuaValue q s)
lirValue IANil = do
    return $ LNil
lirValue (IABool b) = do
    return $ LBool b
lirValue (IAInteger i) = do
    return $ LInteger i
lirValue (IADouble r) = do
    return $ LDouble r
lirValue (IAString s) = do
    return $ LString s
lirValue (IATable listira kviras) = do
    ivs <- lirList listira
    kvs <- forM kviras (\(kira, vira) -> do
        k <- lirValue kira
        v <- lirValue vira
        return $ (k, v))
    let ikvs = kvs ++ zip (map LInteger [1..]) ivs
    lift $ luaCreateTable ikvs
lirValue (IASlot slot) = do
    ctx <- ask
    case slot of
        ISLocal ix -> do
            let ref = lirxLocals ctx A.! ix
            lift $ luaRead ref
        ISConst ix -> do
            lift $ luaLiftST $ V.readArray (lirxConsts ctx) ix
        ISGuard ix -> do
            lift $ luaLiftST $ V.readArray (lirxGuards ctx) ix
lirValue (IAUpvalue ix) = do
    upvalues <- lirxUpvalues <$> ask
    let ref = upvalues A.! ix
    lift $ luaRead ref
lirValue (IAUpconst ix) = do
    upconsts <- lirxUpconsts <$> ask
    return $ upconsts A.! ix
lirValue (IAIndex tableira indexira) = do
    lirValueBinary luaGet tableira indexira
lirValue (IAUnaryUnm aira) = lirValueUnary luaArithUnm aira
lirValue (IAUnaryLen aira) = lirValueUnary luaLen aira
lirValue (IAUnaryBNot aira) = lirValueUnary luaArithBNot aira
lirValue (IABinaryPow aira bira) = lirValueBinary luaArithPow aira bira
lirValue (IABinaryMul aira bira) = lirValueBinary luaArithMul aira bira
lirValue (IABinaryDiv aira bira) = lirValueBinary luaArithDiv aira bira
lirValue (IABinaryIDiv aira bira) = lirValueBinary luaArithIDiv aira bira
lirValue (IABinaryMod aira bira) = lirValueBinary luaArithMod aira bira
lirValue (IABinaryAdd aira bira) = lirValueBinary luaArithAdd aira bira
lirValue (IABinarySub aira bira) = lirValueBinary luaArithSub aira bira
lirValue (IABinaryConcat aira bira) = lirValueBinary luaConcat aira bira
lirValue (IABinaryShl aira bira) = lirValueBinary luaArithShl aira bira
lirValue (IABinaryShr aira bira) = lirValueBinary luaArithShr aira bira
lirValue (IABinaryBAnd aira bira) = lirValueBinary luaArithBAnd aira bira
lirValue (IABinaryBXor aira bira) = lirValueBinary luaArithBXor aira bira
lirValue (IABinaryBOr aira bira) = lirValueBinary luaArithBOr aira bira
lirValue (IABinaryLt aira bira) = do
    LBool <$> lirValueBinary luaCompareLt aira bira
lirValue (IABinaryGt aira bira) = do
    LBool <$> lirValueBinary (flip luaCompareLt) aira bira
lirValue (IABinaryLe aira bira) = do
    LBool <$> lirValueBinary luaCompareLe aira bira
lirValue (IABinaryGe aira bira) = do
    LBool <$> lirValueBinary (flip luaCompareLe) aira bira
lirValue (IABinaryEq aira bira) = do
    LBool <$> lirValueBinary luaCompareEq aira bira
lirValue (IABinaryNeq aira bira) = do
    (LBool . not) <$> lirValueBinary luaCompareEq aira bira
lirValue (IACar listira) = do
    list <- lirList listira
    case list of
        x:_ -> return $ x
        _ -> return $ LNil
lirValue (IALNot aira) = do
    (LBool . not . luaToBoolean) <$> lirValue aira
lirValue (IALAnd aira bira) = do
    a <- lirValue aira
    if luaToBoolean a
        then lirValue bira
        else return $ a
lirValue (IALOr aira bira) = do
    a <- lirValue aira
    if luaToBoolean a
        then return $ a
        else lirValue bira
lirValue (IAFunction
        mlocation upvaluedefs upconstdefs paramdefs
        maxl maxc maxg funcbody) = do
    ctx <- ask
    upvalues <- forM upvaluedefs (\(source, mdef) -> do
        let ref = case source of
                Left ix -> lirxUpvalues ctx A.! ix
                Right ix -> lirxLocals ctx A.! ix
        return $ (mdef, ref))
    upconsts <- forM upconstdefs (\(source, mdef) -> do
        value <- case source of
                Left ix -> return $ lirxUpconsts ctx A.! ix
                Right (ISLocal ix) -> do
                    let ref = lirxLocals ctx A.! ix
                    lift $ luaRead ref
                Right (ISConst ix) -> do
                    lift $ luaLiftST $ V.readArray (lirxConsts ctx) ix
                Right (ISGuard ix) -> do
                    lift $ luaLiftST $ V.readArray (lirxGuards ctx) ix
        return $ (mdef, value))
    lift $
        luaCreateFunction $
            lirFunction
                mlocation upvalues upconsts paramdefs
                maxl maxc maxg funcbody


lirValueUnary
    :: (LuaValue q s -> LuaState q s (LuaValue q s))
    -> IrValue
    -> LirState q s (LuaValue q s)
lirValueUnary lfunc aira = do
    a <- lirValue aira
    lift $ lfunc a


lirValueBinary
    :: (LuaValue q s -> LuaValue q s -> LuaState q s t)
    -> IrValue
    -> IrValue
    -> LirState q s t
lirValueBinary lfunc aira bira = do
    a <- lirValue aira
    b <- lirValue bira
    lift $ lfunc a b


lirList :: IrList -> LirState q s [LuaValue q s]
lirList IAArguments = do
    lirxVararg <$> ask
lirList (IACall funcira argsira) = do
    func <- lirValue funcira
    args <- lirList argsira
    lift $ luaCall func args
lirList (IACallMethod objira indexira argsira) = do
    obj <- lirValue objira
    index <- lirValue indexira
    args <- lirList argsira
    func <- lift $ luaGet obj index
    lift $ luaCall func (obj:args)
lirList (IARange firstira limitira stepira) = do
    first <- lirValue firstira
    limit <- lirValue limitira
    step <- lirValue stepira
    func <- lift $ luaRangeIter first limit step
    return $ [func]
lirList IAEmpty = do
    return $ []
lirList (IACons firstira restira) = do
    first <- lirValue firstira
    rest <- lirList restira
    return $ first:rest


lirSink :: IrSink -> LuaValue q s -> LirState q s ()
lirSink (IASetLocal ix) value = do
    locals <- lirxLocals <$> ask
    let ref = locals A.! ix
    lift $ luaWrite ref value
lirSink (IASetUpvalue ix) value = do
    upvalues <- lirxUpvalues <$> ask
    let ref = upvalues A.! ix
    lift $ luaWrite ref value
lirSink (IASetIndex tableira indexira) value = do
    table <- lirValue tableira
    index <- lirValue indexira
    lift $ luaSet table index value


lirLocal
    :: LuaValue q s
    -> (Maybe (SourceRange, BSt.ByteString), IrSlot)
    -> LirState q s (Either (LuaFrameReturn q s) IrAction)
    -> LirState q s (Either (LuaFrameReturn q s) IrAction)
lirLocal value (mdef, slot) act = do
    ctx <- ask
    case slot of
        ISLocal ix -> do
            let ref = lirxLocals ctx A.! ix
            lift $ luaWrite ref $ value
            lift $
                luadWithinLocal mdef (Left value) $
                    (runReaderT act (lirxStackPush slot ctx))
        ISConst ix -> do
            let consts = lirxConsts ctx
            lift $ luaLiftST $ V.writeArray consts ix $ value
            lift $
                luadWithinLocal mdef (Left value) $
                    (runReaderT act (lirxStackPush slot ctx))
        ISGuard ix -> do
            let guards = lirxGuards ctx
            lift $ luaLiftST $ V.writeArray guards ix $ value
            lift $
                luaCloseAfter value $
                    luadWithinLocal mdef (Left value) $
                        runReaderT act (lirxStackPush slot ctx)


lirContinue
    :: Either (LuaFrameReturn q s) IrAction
    -> LirState q s (Either (LuaFrameReturn q s) IrAction)
lirContinue (Left r) = return $ Left r
lirContinue (Right ira) = lirAction ira


lirOpen
    :: [LuaValue q s]
    -> [(Maybe (SourceRange, BSt.ByteString), IrSlot)]
    -> IrAction
    -> LirState q s (Either (LuaFrameReturn q s) IrAction)
lirOpen _ [] _ = undefined
lirOpen [] [s] next = do
    lirLocal LNil s $ lirAction next
lirOpen (v:_) [s] next = do
    lirLocal v s $ lirAction next
lirOpen [] (s:s':ss) next = do
    lirLocal LNil s $ (lirOpen [] (s':ss) next >>= lirContinue)
lirOpen (v:vs) (s:s':ss) next = do
    lirLocal v s $ (lirOpen vs (s':ss) next >>= lirContinue)


lirAction
    :: IrAction
    -> LirState q s (Either (LuaFrameReturn q s) IrAction)
lirAction (IAAssign sourceira sinks next) = do
    sources <- lirList sourceira
    zipWithM_
        (\sink value -> do
            lirSink sink value)
        sinks
        (sources ++ repeat LNil)
    lirAction next
lirAction (IAOpen sourceira targets next) = do
    sources <- lirList sourceira
    lirOpen sources targets next >>= lirContinue
lirAction (IASequence list next) = do
    _ <- lirList list
    lirAction next
lirAction (IADrop [] _) = undefined
lirAction (IADrop (s:ss) next) = do
    stack <- lirxStack <$> ask
    let top = fst <$> uncons stack
    if top == Just s
        then case ss of
            [] -> return $ Right $ next
            _ -> return $ Right $ IADrop ss next
        else lift $ luaError $ LString "Invalid IR"
lirAction (IAReturn listira) = do
    result <- lirList listira
    return $ Left $ Right $ result
lirAction (IATailCall funcira argsira) = do
    func <- lirValue funcira
    args <- lirList argsira
    return $ Left $ Left $ (luaCall func, args)
lirAction (IATailCallMethod objira indexira argsira) = do
    obj <- lirValue objira
    index <- lirValue indexira
    args <- lirList argsira
    func <- lift $ luaGet obj index
    return $ Left $ Left $ (luaCall func, obj:args)
lirAction (IABranch condira pos neg) = do
    cond <- lirValue condira
    if luaToBoolean cond
        then lirAction pos
        else lirAction neg
lirAction (IABlock bix) = do
    blocks <- lirxBody <$> ask
    case lookup bix blocks of
        Just ira -> lirAction ira
        Nothing -> lirInvalid
lirAction (IAMark pr next) = do
    lift $ luadSetLocation (Just pr)
    lirAction next


lirExecute
    :: LirContext q s
    -> LuaState q s (LuaFrameReturn q s)
lirExecute ctx = do
    runReaderT
        (do
            let (_, main):_ = lirxBody ctx
            result <- lirAction main
            case result of
                Left fret -> return $ fret
                Right _ -> lirInvalid)
        ctx


lirInvalid :: LirState q s a
lirInvalid = lift $ luaError $ LString "Invalid IR"


lirFunction
    :: Maybe (SourceRange, BSt.ByteString)
    -> [(Maybe (SourceRange, BSt.ByteString), LuaRef q s (LuaValue q s))]
    -> [(Maybe (SourceRange, BSt.ByteString), LuaValue q s)]
    -> [(Int, Maybe (SourceRange, BSt.ByteString))]
    -> Int -> Int -> Int
    -> IrBody
    -> LuaFunction q s
lirFunction mlocation upvalues upconsts paramdefs maxl maxc maxg funcbody = do
    let upvalueArray = A.listArray
            (0, length upvalues - 1)
            (map snd upvalues)
    let upconstArray = A.listArray
            (0, length upconsts - 1)
            (map snd upconsts)
    let upvalueList =
            map (\(mdef, ref) -> (mdef, Right ref)) upvalues
            ++ map (\(mdef, value) -> (mdef, Left value)) upconsts
    let function args = (do
        locals <- replicateM maxl $ luaAlloc LNil
        let localArray = A.listArray
                (0, maxl - 1)
                locals
        constArray <- luaLiftST $ V.newArray (0, maxc - 1) LNil
        guardArray <- luaLiftST $ V.newArray (0, maxg - 1) LNil
        (vararg, argvarlist, stack) <- parseArgs localArray paramdefs args
        let lirContext = LirContext {
            lirxBody = funcbody,
            lirxVararg = vararg,
            lirxUpvalues = upvalueArray,
            lirxUpconsts = upconstArray,
            lirxLocals = localArray,
            lirxConsts = constArray,
            lirxGuards = guardArray,
            lirxStack = stack}
        luadRunFunction mlocation upvalueList argvarlist $
            lirExecute lirContext)
    function
    where
    parseArgs _ [] args = do
        return $ (args, [], [])
    parseArgs localArray ((ix, mdef):ps) args = do
        let (arg, as) = fromMaybe (LNil, []) (uncons args)
        let ref = localArray A.! ix
        luaWrite ref arg
        (vararg, argvarlist, stack) <- parseArgs localArray ps as
        return $ (vararg, (mdef, Right ref):argvarlist, (ISLocal ix):stack)


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

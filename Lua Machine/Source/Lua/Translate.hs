{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Translate (
    IrSlot(..),
    IrValue(..),
    IrList(..),
    IrSink(..),
    IrAction(..),
    IrBody,
    translateLua,
) where


import Control.DeepSeq
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua.DefString
import Lua.Parse
import Lua.SourceRange


data IrSlot
    = ISLocal !Int
    | ISConst !Int
    | ISGuard !Int
    deriving (Show, Eq, Generic, NFData)


instance DefString IrSlot where
    defString _ (ISLocal i) rest = "@" $ shows i $ rest
    defString _ (ISConst i) rest = "%" $ shows i $ rest
    defString _ (ISGuard i) rest = "&" $ shows i $ rest


data IrValue
    = IANil
    | IABool Bool
    | IAInteger Integer
    | IADouble Double
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
    deriving (Show, Generic, NFData)


data IrList
    = IAArguments
    | IACall IrValue IrList
    | IACallMethod IrValue IrValue IrList
    | IARange IrValue IrValue IrValue
    | IAEmpty
    | IACons IrValue IrList
    deriving (Show, Generic, NFData)


data IrSink
    = IASetLocal Int
    | IASetUpvalue Int
    | IASetIndex IrValue IrValue
    deriving (Show, Generic, NFData)


data IrAction
    = IAAssign IrList [IrSink] IrAction
    | IAOpen IrList [(Maybe (SourceRange, BSt.ByteString), IrSlot)] IrAction
    | IASequence IrList IrAction
    | IADrop IrSlot IrAction
    | IAReturn IrList
    | IATailCall IrValue IrList
    | IATailCallMethod IrValue IrValue IrList
    | IABranch IrValue IrAction IrAction
    | IABlock Int
    | IAMark SourceRange IrAction
    deriving (Show, Generic, NFData)


type IrBody = [(Int, IrAction)]


instance DefString (IrBody) where
    defString _ [] rest = rest
    defString d [(ix, act)] rest
        = "def _" $ shows ix $ ":" $ defBr (d+1) $ defString (d+1) act $ rest
    defString d ((ix, act):x:xs) rest
        = "def _" $ shows ix $ ":" $ defBr (d+1) $ defString (d+1) act
            $ defBr d $ defString d (x:xs) $ rest


instance DefString IrValue where
    defString _ IANil rest
        = "nil" $ rest
    defString _ (IABool False) rest
        = "false" $ rest
    defString _ (IABool True) rest
        = "true" $ rest
    defString _ (IAInteger i) rest
        = shows i $ rest
    defString _ (IADouble x) rest
        = shows x $ rest
    defString _ (IAString s) rest
        = shows s $ rest
    defString d (IATable ivs kvs) rest
        = "(table {" $ defString d ivs $ "} [" $ defKVs kvs $ "])" $ rest
        where
        defKVs [] rest' = rest'
        defKVs [(k, v)] rest'
            = defBr (d+1) $ defString (d+1) k $ " <- " $ defString (d+1) v
                $ defBr (d+1) $ rest'
        defKVs ((k, v):xs) rest'
            = defBr (d+1) $ defString (d+1) k $ " <- " $ defString (d+1) v
                $ "," $ defKVs xs $ rest'
    defString d (IASlot slot) rest
        = defString d slot $ rest
    defString _ (IAUpvalue ix) rest
        = "$" $ shows ix $ rest
    defString _ (IAUpconst ix) rest
        = "^" $ shows ix $ rest
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
        = "(" $ defBr (d+1) $ "function" $ defLocation mlocation
            $ defUpvalues upvalues (0 :: Int)
            $ defUpconsts upconsts (0 :: Int)
            $ defParameters params
            $ defBr (d+1) $ shows maxlocals $ "@ "
            $ shows maxconsts $ "% " $ shows maxguards $ "&"
            $ defBr (d+1) $ defString (d+1) body
            $ defBr (d+1) $ ")" $ rest
        where
        defLocation Nothing rest' = rest'
        defLocation (Just (pr, name)) rest'
            = " -- " $ unpackSt name $ " " $ shows pr $ rest'
        defUpvalues [] _ rest' = rest'
        defUpvalues ((Left oix, mloc):xs) ix rest'
            = defBr (d+1) $ "*$" $ shows ix $ " <- $" $ shows oix
                $ defLocation mloc $ defUpvalues xs (ix+1) $ rest'
        defUpvalues ((Right lix, mloc):xs) ix rest'
            = defBr (d+1) $ "*$" $ shows ix $ " <- @" $ shows lix
                $ defLocation mloc $ defUpvalues xs (ix+1) $ rest'
        defUpconsts [] _ rest' = rest'
        defUpconsts ((Left oix, mloc):xs) ix rest'
            = defBr (d+1) $ "*^" $ shows ix $ " <- ^" $ shows oix
                $ defLocation mloc $ defUpconsts xs (ix+1) $ rest'
        defUpconsts ((Right slot, mloc):xs) ix rest'
            = defBr (d+1) $ "*^" $ shows ix $ " <- " $ defString (d+1) slot
                $ defLocation mloc $ defUpconsts xs (ix+1) $ rest'
        defParameters [] rest' = rest'
        defParameters ((ix, mloc):xs) rest'
            = defBr (d+1) $ "*@" $ shows ix
                $ defLocation mloc $ defParameters xs $ rest'


instance DefString IrList where
    defString _ IAArguments rest
        = "arguments" $ rest
    defString d (IACall func args) rest
        = "call " $ defString d func $ " {" $ defString d args $ "}" $ rest
    defString d (IACallMethod func index args) rest
        = "callmethod " $ defString d func $ " "
            $ defString d index $ " {" $ defString d args $ "}" $ rest
    defString d (IARange first limit step) rest
        = "range " $ defString d first $ " " $ defString d limit
            $ " " $ defString d step $ rest
    defString _ IAEmpty rest
        = "empty" $ rest
    defString d (IACons x xs) rest
        = defString d x $ ", " $ defString d xs $ rest


instance DefString IrSink where
    defString _ (IASetLocal ix) rest
        = "=@" $ shows ix $ rest
    defString _ (IASetUpvalue ix) rest
        = "=$" $ shows ix $ rest
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
            $ defTargets targets $ "];"
            $ defBr d $ defString d next $ rest
        where
        defTargets [] rest' = rest'
        defTargets ((Nothing, slot):xs) rest'
            = defBr (d+1) $ "*" $ defString (d+1) slot $ defTargets xs $ rest'
        defTargets ((Just (pr, name), slot):xs) rest'
            = defBr (d+1) $ "*" $ defString (d+1) slot
                $ " --[[ " $ unpackSt name $ " " $ shows pr $ " ]]"
                $ defTargets xs $ rest'
    defString d (IASequence sa sb) rest
        = defString d sa $ ";" $ defBr d $ defString d sb $ rest
    defString d (IADrop slot x) rest
        = "drop " $ defString d slot $ ";" $ defBr d
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
    defString _ (IABlock ix) rest
        = "block _" $ shows ix $ rest
    defString d (IAMark pr next) rest
        = "mark " $ shows pr $ ";" $ defBr d $ defString d next $ rest


data CompileError = CompileError SourceRange String


instance Show CompileError where
    show (CompileError range msg) = show range ++ "\n" ++ msg


type BlockTable = [(Int, Maybe BSt.ByteString, [Int])]


data LexicalContext = LexicalContext {
    lecxOuter :: Maybe LexicalContext,
    lecxNextIndex :: Int,
    lecxMaxIndex :: (Int, Int, Int),
    lecxSlots :: [(Int, BSt.ByteString, SourceRange, IrSlot)],
    lecxUpvalues :: [(BSt.ByteString, SourceRange, Int, Either Int Int)],
    lecxUpconsts :: [(BSt.ByteString, SourceRange, Int, Either Int IrSlot)],
    lecxVararg :: Bool,
    lecxBlocks :: BlockTable}


lecxSaveScope
    :: (Monad m)
    => StateT LexicalContext m Int
lecxSaveScope = do
    slots <- lecxSlots <$> get
    case slots of
        [] -> return $ -1
        (ix, _, _, _):_ -> return $ ix


lecxRestoreScope
    :: (Monad m)
    => Int
    -> StateT LexicalContext m [IrSlot]
lecxRestoreScope limit = do
    slots <- lecxSlots <$> get
    let (dropped, left) = flip break slots $ \(ix, _, _, _) -> ix <= limit
    modify' $ \lecx -> lecx {
        lecxSlots = left}
    return $ map (\(_, _, _, slot) -> slot) $ dropped


lecxCreateSlot
    :: (Monad m)
    => (Int -> IrSlot)
    -> (IrSlot -> Int -> Int)
    -> ((Int, Int, Int) -> Int -> (Int, Int, Int))
    -> BSt.ByteString
    -> SourceRange
    -> StateT LexicalContext m Int
lecxCreateSlot genf ixmatch maxupdate name pr = do
    lecx <- get
    let ix = nextIx $ lecxSlots lecx
    put $ lecx {
        lecxNextIndex = lecxNextIndex lecx + 1,
        lecxMaxIndex = maxupdate (lecxMaxIndex lecx) (ix + 1),
        lecxSlots = (lecxNextIndex lecx, name, pr, genf ix):lecxSlots lecx}
    return $ ix
    where
    nextIx [] = 0
    nextIx ((_, _, _, slot):xs) = ixmatch slot (nextIx xs)


lecxCreateLocal
    :: (Monad m)
    => BSt.ByteString
    -> SourceRange
    -> StateT LexicalContext m Int
lecxCreateLocal name pr = do
    lecxCreateSlot
        ISLocal
        (\slot rest -> case slot of ISLocal ix -> ix + 1; _ -> rest)
        (\(maxl, maxc, maxg) ix -> (max ix maxl, maxc, maxg))
        name pr

lecxCreateConst
    :: (Monad m)
    => BSt.ByteString
    -> SourceRange
    -> StateT LexicalContext m Int
lecxCreateConst name pr = do
    lecxCreateSlot
        ISConst
        (\slot rest -> case slot of ISConst ix -> ix + 1; _ -> rest)
        (\(maxl, maxc, maxg) ix -> (maxl, max ix maxc, maxg))
        name pr

lecxCreateGuard
    :: (Monad m)
    => BSt.ByteString
    -> SourceRange
    -> StateT LexicalContext m Int
lecxCreateGuard name pr = do
    lecxCreateSlot
        ISGuard
        (\slot rest -> case slot of ISGuard ix -> ix + 1; _ -> rest)
        (\(maxl, maxc, maxg) ix -> (maxl, maxc, max ix maxg))
        name pr


lecxAccessVariable
    :: BSt.ByteString
    -> StateT LexicalContext Maybe (
        SourceRange,
        (IrSlot -> a) -> (Int -> a) -> (Int -> a) -> a)
      {- onSlot           onUpvalue     onUpconst -}
lecxAccessVariable name = do
    lecx <- get
    msum [
        searchSlots $ lecxSlots lecx,
        searchUpvalues $ lecxUpvalues lecx,
        searchUpconsts $ lecxUpconsts lecx,
        do
            ((pr, disp), outer') <- lift (do
                outer <- lecxOuter lecx
                runStateT (lecxAccessVariable name) outer)
            let pullUpvalue source = (do
                let ix = case lecxUpvalues lecx of
                        (_, _, uix, _):_ -> uix+1
                        _ -> 0
                put $ lecx {
                    lecxOuter = Just outer',
                    lecxUpvalues = (name, pr, ix, source):lecxUpvalues lecx}
                return $ (pr, (\_ onUpvalue _ -> onUpvalue ix)))
            let pullUpconst source = (do
                let ix = case lecxUpconsts lecx of
                        (_, _, uix, _):_ -> uix+1
                        _ -> 0
                put $ lecx {
                    lecxOuter = Just outer',
                    lecxUpconsts = (name, pr, ix, source):lecxUpconsts lecx}
                return $ (pr, (\_ _ onUpconst -> onUpconst ix)))
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
    searchSlots ((_, defname, pr, slot):rest) = do
        if defname == name
            then return $ (pr, (\onSlot _ _ -> onSlot slot))
            else searchSlots rest
    searchUpvalues [] = do
        lift $ Nothing
    searchUpvalues ((defname, pr, ix, _):rest) = do
        if defname == name
            then return $ (pr, (\_ onUpvalue _ -> onUpvalue ix))
            else searchUpvalues rest
    searchUpconsts [] = do
        lift $ Nothing
    searchUpconsts ((defname, pr, ix, _):rest) = do
        if defname == name
            then return $ (pr, (\_ _ onUpconst -> onUpconst ix))
            else searchUpconsts rest


lecxCreateBlock
    :: Maybe BSt.ByteString
    -> StateT LexicalContext Maybe Int
lecxCreateBlock mname = do
    lecx <- get
    case mname of
        Just name -> lift $ checkDup name $ lecxBlocks lecx
        Nothing -> return ()
    let ix = case lecxBlocks lecx of
            (bix, _, _):_ -> bix+1
            _ -> 0
    let locstack = map (\(myix,_,_,_) -> myix) $ lecxSlots lecx
    put $ lecx {
        lecxBlocks = (ix, mname, locstack):lecxBlocks lecx}
    return $ ix
    where
    checkDup _ [] = Just ()
    checkDup name ((_, Nothing, _):xs) = do
        checkDup name xs
    checkDup name ((_, Just name', _):xs) = do
        if name == name'
            then Nothing
            else checkDup name xs


lecxNewBlock
    :: (Monad m) => StateT LexicalContext m Int
lecxNewBlock = do
    mapStateT (return . fromJust) $ lecxCreateBlock Nothing


lecxGetLocalStack
    :: (Monad m) => StateT LexicalContext m [(Int, IrSlot)]
lecxGetLocalStack = do
    lecx <- get
    return $ map (\(ix, _, _, slot) -> (ix, slot)) $ lecxSlots lecx


type Compile t = StateT LexicalContext (Either CompileError) t


compileVariable
    :: NameNode
    -> Compile
        (  (IrSlot -> a)             -- onSlot
        -> (Int -> a)                -- onUpvalue
        -> (Int -> a)                -- onUpconst
        -> (IrValue -> IrValue -> a) -- onIndex
        -> a)
compileVariable (NameNode (pr, name)) = do
    mapStateT (maybe err Right) $
        tryAccess `mplus` tryEnv
    where
    err = Left $ CompileError pr "Cannot access a variable"
    tryAccess = do
        (_, disp) <- lecxAccessVariable name
        disp
            (\slot -> do
                return $ (\onSlot _ _ _ -> do
                    onSlot slot))
            (\upvalue -> do
                return $ (\_ onUpvalue _ _ -> do
                    onUpvalue upvalue))
            (\upconst -> do
                return $ (\_ _ onUpconst _ -> do
                    onUpconst upconst))
    tryEnv = do
        (_, disp) <- lecxAccessVariable "_ENV"
        disp
            (\slot -> do
                return $ (\_ _ _ onIndex -> do
                    onIndex (IASlot slot) (IAString name)))
            (\upvalue -> do
                return $ (\_ _ _ onIndex -> do
                    onIndex (IAUpvalue upvalue) (IAString name)))
            (\upconst -> do
                return $ (\_ _ _ onIndex -> do
                    onIndex (IAUpconst upconst) (IAString name)))


compileExpressionRead
    :: ShowS
    -> ExprNode
    -> Compile IrValue
compileExpressionRead _ (ExprNode (_, ExprNil)) = do
    return $ IANil
compileExpressionRead _ (ExprNode (_, ExprBool b)) = do
    return $ IABool b
compileExpressionRead _ (ExprNode (_, ExprInt i)) = do
    return $ IAInteger i
compileExpressionRead _ (ExprNode (_, ExprReal r)) = do
    return $ IADouble r
compileExpressionRead _ (ExprNode (_, ExprString s)) = do
    return $ IAString s
compileExpressionRead target value@(ExprNode (pr, ExprFunction _ _ _)) = do
    let name = BSt.pack $ target ""
    functionira <- compileFunction pr name value
    return $ functionira
compileExpressionRead target (ExprNode (_, ExprTable items mlast)) = do
    let (posits, indits) = foldr
            (\(mindex, value) (posits', indits') -> do
                case mindex of
                    Nothing -> (value:posits', indits')
                    Just index -> (posits', (index, value):indits'))
            ([], [])
            items
    let positname n = target . "[" . shows (n :: Int) . "]"
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
compileExpressionRead _ (ExprNode (_, ExprVar namenode)) = do
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
compileExpressionRead target (ExprNode (_, ExprIndex table index)) = do
    IAIndex
        <$> compileExpressionRead target table
        <*> compileExpressionRead target index
compileExpressionRead target (ExprNode (_, ExprUnary op a)) = do
    selectunary op
        <$> compileExpressionRead target a
    where
    selectunary UnaryNot = IALNot
    selectunary UnaryLength = IAUnaryLen
    selectunary UnaryMinus = IAUnaryUnm
    selectunary UnaryBNot = IAUnaryBNot
compileExpressionRead target (ExprNode (_, ExprBinary op a b)) = do
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
compileExpressionRead target (ExprNode (_, ExprAnd a b)) = do
    IALAnd
        <$> compileExpressionRead target a
        <*> compileExpressionRead target b
compileExpressionRead target (ExprNode (_, ExprOr a b)) = do
    IALOr
        <$> compileExpressionRead target a
        <*> compileExpressionRead target b
compileExpressionRead target (ExprNode (_, ExprGroup a)) = do
    compileExpressionRead target a
{-ExprEllipsis, ExprCall, ExprMethodCall-}
compileExpressionRead target expr = do
    listira <- compileExpressionReadLast target expr
    return $ IACar listira


compileExpressionReadLast
    :: ShowS
    -> ExprNode
    -> Compile IrList
compileExpressionReadLast _ (ExprNode (pr, ExprEllipsis)) = do
    vararg <- lecxVararg <$> get
    if vararg
        then return $ IAArguments
        else lift $ Left $
            CompileError pr "Ellipsis must appear inside a vararg function"
compileExpressionReadLast target (ExprNode (_, ExprCall func args mlast)) = do
    let argname n = "(argument " . shows (n :: Int) . " of "
            . defString 0 func . ")"
    let argtargets = map argname [1..]
    IACall
        <$> compileExpressionRead target func
        <*> compileExpressionList argtargets args mlast
compileExpressionReadLast target
        (ExprNode (_, ExprMethodCall obj name args mlast)) = do
    let argname n = "(argument " . shows (n :: Int) . " of "
            . defString 0 obj . ":" . unpackSt name . ")"
    let argtargets = map argname [1..]
    IACallMethod
        <$> compileExpressionRead target obj
        <*> (return $ IAString name)
        <*> compileExpressionList argtargets args mlast
compileExpressionReadLast _ _ = do
    error "the parser shouldn't produce that"


compileExpressionWrite
    :: ExprNode
    -> Compile IrSink
compileExpressionWrite (ExprNode (pr, ExprVar namenode)) = do
    disp <- compileVariable namenode
    disp
        (\slot -> do
            case slot of
                ISLocal ix -> return $ IASetLocal ix
                _ -> lift $ Left $ errConst)
        (\upvalue -> do
            return $ IASetUpvalue upvalue)
        (\_ -> do
            lift $ Left $ errConst)
        (\table index -> do
            return $ IASetIndex table index)
    where
    errConst = CompileError pr
        ("Cannot assign to an immutable variable " $ defString 0 namenode $ "")
compileExpressionWrite (ExprNode (_, ExprIndex table index)) = do
    IASetIndex
        <$> compileExpressionRead "(table)" table
        <*> compileExpressionRead "(index)" index
compileExpressionWrite (ExprNode (pr, _)) = do
    lift $ Left $ CompileError pr "A variable or table field expected"


compileExpressionList
    :: [ShowS]
    -> [ExprNode]
    -> Maybe ExprNode
    -> Compile IrList
compileExpressionList _ [] Nothing = do
    return $ IAEmpty
compileExpressionList (t:_) [] (Just final) = do
    compileExpressionReadLast t final
compileExpressionList (t:targets) (x:xs) mfinal = do
    IACons
        <$> compileExpressionRead t x
        <*> compileExpressionList targets xs mfinal
compileExpressionList [] _ _ = undefined


newtype Link = Link {
    link
        :: (BlockTable, Maybe Int)
        -> (IrAction, IrBody)
        -> Either CompileError (IrAction, IrBody)}


instance Semigroup Link where
    Link fa <> Link fb = Link $ \ctx rest ->
        fa ctx =<< fb ctx rest


instance Monoid Link where
    mempty = Link $ \_ rest -> Right rest


linkAction :: (IrAction -> IrAction) -> Link
linkAction iras = Link $ \(_, _) (after, bbs) -> do
    return $ (iras after, bbs)


linkLast :: IrAction -> Link
linkLast ira = Link $ \(_, _) (_, bbs) -> do
    return $ (ira, bbs)


linkBranch :: IrValue -> Link -> Link -> Link
linkBranch condira ptbody pestat = Link (\ctx (after, bbs) -> do
    (estatira, estatbbs) <- link pestat ctx (after, bbs)
    (tbodyira, tbodybbs) <- link ptbody ctx (after, estatbbs)
    return $ (IABranch condira tbodyira estatira, tbodybbs))


linkSplit :: Int -> Link
linkSplit ix = Link $ \(_, _) (after, bbs) -> do
    return $ (IABlock ix, (ix, after):bbs)


linkLocalBreak :: Int -> Link -> Link
linkLocalBreak bix body = Link $ \(blocks, _) (after, bbs) -> do
    link body (blocks, Just bix) (after, bbs)


linkBreak :: SourceRange -> [(Int, IrSlot)] -> Link
linkBreak pr fromstack = Link $ \(blocks, mbreak) (_, bbs) -> do
    case mbreak of
        Nothing -> Left $ CompileError pr "Invalid break"
        Just bix -> do
            let tostack = lookupBlockByIx bix blocks
            diff <- stackDifference pr fromstack tostack
            return $ (makeDropBefore diff $ IABlock bix, bbs)


linkGoto :: SourceRange -> BSt.ByteString -> [(Int, IrSlot)] -> Link
linkGoto pr name fromstack = Link $ \(blocks, _) (_, bbs) -> do
    case lookupBlockByName name blocks of
        Nothing -> Left $ CompileError pr $
            "Invalid label " $ unpackSt name $ ""
        Just (bix, tostack) -> do
            diff <- stackDifference pr fromstack tostack
            return $ (makeDropBefore diff $ IABlock bix, bbs)


lookupBlockByIx :: Int -> BlockTable -> [Int]
lookupBlockByIx bix ((ix, _, stack):rest)
    | bix == ix = stack
    | otherwise = lookupBlockByIx bix rest
lookupBlockByIx _ [] = undefined


lookupBlockByName :: BSt.ByteString -> BlockTable -> Maybe (Int, [Int])
lookupBlockByName nameref ((_, Nothing, _):rest) = do
    lookupBlockByName nameref rest
lookupBlockByName nameref ((ix, Just name, stack):rest)
    | nameref == name = Just (ix, stack)
    | otherwise = lookupBlockByName nameref rest
lookupBlockByName _ [] = Nothing


stackDifference
    :: SourceRange
    -> [(Int, IrSlot)]
    -> [Int]
    -> Either CompileError [IrSlot]
stackDifference pr from to = do
    case (from, to) of
        ([], []) -> do
            return $! []
        ((_, fslot):frest, []) -> do
            (fslot:) <$!> stackDifference pr frest to
        ((findex, fslot):frest, tindex:_)
            | findex == tindex -> do
                return $! []
            | findex > tindex -> do
                (fslot:) <$!> stackDifference pr frest to
        _ -> Left $ CompileError pr "Jump lands inside a variable's scope"


makeDropBefore
    :: [IrSlot]
    -> IrAction
    -> IrAction
makeDropBefore [] after
    = after
makeDropBefore (slot:rest) after
    = IADrop slot $ makeDropBefore rest after


compileStat
    :: StatNode
    -> Compile Link

compileStat (StatNode (_, StatNull)) = do
    return $ mempty

compileStat (StatNode (pr, StatAssign lhs rhs mlast)) = do
    let targets = map (defString 0) lhs ++ repeat id
    sourceira <- compileExpressionList targets rhs mlast
    sinks <- forM lhs compileExpressionWrite
    return $ linkAction $
        IAMark pr . IAAssign sourceira sinks

compileStat (StatNode (pr, StatInvoke expr)) = do
    case expr of
        ExprNode (_, ExprCall _ _ _) -> return ()
        ExprNode (_, ExprMethodCall _ _ _ _) -> return ()
        _ -> lift $ Left $
            CompileError pr "Function call expected"
    exprira <- compileExpressionReadLast "(invoke)" expr
    return $ linkAction $
        IAMark pr . IASequence exprira

compileStat (StatNode (pr, StatLabel label)) = do
    let (NameNode (_, name)) = label
    nextid <- mapStateT (maybe (err name) Right) $
        lecxCreateBlock (Just name)
    return $ linkSplit nextid <> linkAction (IAMark pr)
    where
    err name = Left $ CompileError pr ("Duplicate label " $ unpackSt name $ "")

compileStat (StatNode (pr, StatBreak)) = do
    currentstack <- lecxGetLocalStack
    return $ linkAction (IAMark pr) <> linkBreak pr currentstack

compileStat (StatNode (pr, StatGoto label)) = do
    let NameNode (_, name) = label
    currentstack <- lecxGetLocalStack
    return $ linkAction (IAMark pr) <> linkGoto pr name currentstack

compileStat (StatNode (_, StatDo body)) = do
    scope <- lecxSaveScope
    pbody <- compileBody body
    diff <- lecxRestoreScope scope
    return $ pbody <> linkAction (makeDropBefore diff)

compileStat (StatNode (_, StatWhile cond body)) = do
    let (ExprNode (condpr, _)) = cond
    loopid <- lecxNewBlock
    condira <- compileExpressionRead "(while condition)" cond
    scope <- lecxSaveScope
    pbody <- compileBody body
    diff <- lecxRestoreScope scope
    nextid <- lecxNewBlock
    return $ mconcat [
        linkSplit loopid,
        linkAction $ IAMark condpr,
        linkBranch condira
            (linkLocalBreak nextid $
                pbody <> (linkLast $ makeDropBefore diff $ IABlock loopid))
            mempty,
        linkSplit nextid]

compileStat (StatNode (_, StatRepeat body cond)) = do
    let (ExprNode (condpr, _)) = cond
    loopid <- lecxNewBlock
    scope <- lecxSaveScope
    pbody <- compileBody body
    condira <- compileExpressionRead "(repeat condition)" cond
    diff <- lecxRestoreScope scope
    nextid <- lecxNewBlock
    return $ mconcat [
        linkSplit loopid,
        linkLocalBreak nextid $
            pbody
                <> linkAction (IAMark condpr)
                <> linkBranch condira
                    (linkAction $ makeDropBefore diff)
                    (linkLast $ makeDropBefore diff $ IABlock loopid),
        linkSplit nextid]

compileStat (StatNode (_, StatIf cond tbody estat)) = do
    let ExprNode (condpr, _) = cond
    condira <- compileExpressionRead "(if condition)" cond
    scope <- lecxSaveScope
    ptbody <- compileBody tbody
    diff <- lecxRestoreScope scope
    pestat <- compileStat estat
    nextid <- lecxNewBlock
    return $ mconcat [
        linkAction $ IAMark condpr,
        linkBranch condira
            (ptbody <> (linkAction $ makeDropBefore diff))
            pestat,
        linkSplit nextid]

compileStat (StatNode (pr, StatForNum param start limit mdelta body)) = do
    let (NameNode (parampr, paramname)) = param
    startira <- compileExpressionRead "(range start)" start
    limitira <- compileExpressionRead "(range limit)" limit
    deltaira <- case mdelta of
            Just delta -> compileExpressionRead "(range delta)" delta
            Nothing -> return $ IAInteger 1
    scopeouter <- lecxSaveScope
    fiterid <- lecxCreateConst "(range iterator)" pr
    paramid <- lecxCreateLocal paramname parampr
    scopeinner <- lecxSaveScope
    loopid <- lecxNewBlock
    pbody <- compileBody body
    diffinner <- lecxRestoreScope scopeinner
    diffouter <- lecxRestoreScope scopeouter
    nextid <- lecxNewBlock
    return $ mconcat [
        linkAction $ IAMark pr . IAOpen
            (IARange startira limitira deltaira)
            [
                (Just (pr, "(range iterator)"), ISConst fiterid),
                (Just (parampr, paramname), ISLocal paramid)],
        linkSplit loopid,
        linkAction $ IAMark parampr . IAAssign
            (IACall (IASlot $ ISConst fiterid) IAEmpty)
            [IASetLocal paramid],
        linkBranch (IASlot $ ISLocal paramid)
            (linkLocalBreak nextid $
                pbody <> (linkLast $
                    makeDropBefore diffinner $ IABlock loopid))
            (linkAction $ makeDropBefore diffouter),
        linkSplit nextid]

compileStat (StatNode (pr, StatForEach lhs rhs mlast body)) = do
    let lhspr = foldl' (<>) (collapseRangeNull pr) $
            map (\(NameNode (pr', _)) -> pr') lhs
    initlistira <- compileExpressionList
        ([
            "(for iterator)",
            "(for context)",
            "(for index)",
            "(for guard)"] ++ repeat id)
        rhs mlast
    scopeouter <- lecxSaveScope
    fiterid <- lecxCreateConst "(for iterator)" pr
    fstateid <- lecxCreateConst "(for context)" pr
    findexid <- lecxCreateLocal "(for index)" pr
    fguardid <- lecxCreateGuard "(for guard)" pr
    locals <- forM lhs (\(NameNode (pr', name)) -> do
        lix <- lecxCreateLocal name pr'
        return $ (Just (pr', name), lix))
    let (_, firstlocalid):_ = locals
    loopid <- lecxNewBlock
    scopeinner <- lecxSaveScope
    pbody <- compileBody body
    diffinner <- lecxRestoreScope scopeinner
    diffouter <- lecxRestoreScope scopeouter
    nextid <- lecxNewBlock
    return $ mconcat [
        linkAction $ IAMark pr . IAOpen
            initlistira
            [
                (Just (pr, "(for iterator)"), ISConst fiterid),
                (Just (pr, "(for context)"), ISConst fstateid),
                (Just (pr, "(for index)"), ISLocal findexid),
                (Just (pr, "(for guard)"), ISGuard fguardid)],
        linkAction $ IAOpen
            IAEmpty
            (map (\(mdef, lid) -> (mdef, ISLocal lid)) locals),
        linkSplit loopid,
        linkAction $ IAMark lhspr . IAAssign
            (IACall
                (IASlot $ ISConst fiterid)
                (IACons
                    (IASlot $ ISConst fstateid)
                    (IACons
                        (IASlot $ ISLocal findexid)
                        IAEmpty)))
            (map (IASetLocal . snd) locals),
        linkAction $ IAAssign
            (IACons (IASlot (ISLocal firstlocalid)) IAEmpty)
            [IASetLocal findexid],
        linkBranch (IASlot $ ISLocal findexid)
            (linkLocalBreak nextid $
                pbody <> (linkLast $ makeDropBefore diffinner $ IABlock loopid))
            (linkAction $ makeDropBefore diffouter),
        linkSplit nextid]

compileStat (StatNode (pr, StatFunction target value)) = do
    let name = BSt.pack $ defString 0 target ""
    functionira <- compileFunction pr name value
    targetira <- compileExpressionWrite target
    return $ linkAction $
        IAMark pr
            . IAAssign
                (IACons functionira IAEmpty)
                [targetira]

compileStat (StatNode (pr, StatLocalFunction namenode value)) = do
    let (NameNode (namepr, name)) = namenode
    lid <- lecxCreateLocal name namepr
    functionira <- compileFunction pr name value
    return $ linkAction $
        IAMark pr
            . IAOpen
                IAEmpty
                [(Just (namepr, name), ISLocal lid)]
            . IAAssign
                (IACons functionira IAEmpty)
                [IASetLocal lid]

compileStat (StatNode (pr, StatLocalDef lhs rhs mlast)) = do
    let targets = map (\(name, _) -> defString 0 name) lhs ++ repeat id
    sourcesira <- compileExpressionList targets rhs mlast
    locals <- forM lhs $ \(NameNode (pr', name), mattr) -> do
        case mattr of
            Nothing -> do
                ix <- lecxCreateLocal name pr'
                return $ (Just (pr', name), ISLocal ix)
            Just "const" -> do
                ix <- lecxCreateConst name pr'
                return $ (Just (pr', name), ISConst ix)
            Just "close" -> do
                ix <- lecxCreateGuard name pr'
                return $ (Just (pr', name), ISGuard ix)
            Just attr -> lift $ Left $ CompileError pr $
                "Unknown attribute <" ++ BSt.unpack attr ++ ">"
    return $ linkAction $
        IAMark pr . IAOpen sourcesira locals

compileStat (StatNode (pr, StatReturn rhs mlast)) = do
    slots <- lecxSlots <$> get
    let hasGuards = checkGuards slots
    case (rhs, mlast) of
        ([], Just (ExprNode (_, elast)))
            | ExprCall func args arglast <- elast, not hasGuards -> do
                let argname n = "(argument " . shows (n :: Int) . " of "
                        . defString 0 func . ")"
                let argtargets = map argname [1..]
                funcira <- compileExpressionRead "(return)" func
                argira <- compileExpressionList argtargets args arglast
                return $ linkLast $
                    IAMark pr $ IATailCall funcira argira
            | ExprMethodCall obj name args arglast <- elast, not hasGuards -> do
                let argname n = "(argument " . shows (n :: Int) . " of "
                        . defString 0 obj . ":" . unpackSt name . ")"
                let argtargets = map argname [1..]
                objira <- compileExpressionRead "(return)" obj
                let nameira = IAString name
                argira <- compileExpressionList argtargets args arglast
                return $ linkLast $
                    IAMark pr $ IATailCallMethod objira nameira argira
        _ -> do
            let targets = map (\n -> "(return " . shows (n :: Int) . ")") [1..]
            valueira <- compileExpressionList targets rhs mlast
            return $ linkLast $
                IAMark pr $ IAReturn valueira
    where
    checkGuards slots = do
        case slots of
            [] -> False
            (_, _, _, ISGuard _):_ -> True
            (_, _, _, _):rest -> checkGuards rest


compileBody
    :: [StatNode]
    -> Compile Link
compileBody body = do
    foldM
        (\buf stat -> do
            mine <- compileStat stat
            return $ buf <> mine)
        mempty
        body


compileFunction
    :: SourceRange
    -> BSt.ByteString
    -> ExprNode
    -> Compile IrValue
compileFunction pr name (ExprNode (_,
        ExprFunction paramnodes mvarargnode stats)) = do
    let isvararg = isJust mvarargnode
    outer <- get
    let (paramdefs, paramdecls, paramcount) = foldr
            (\(NameNode (pr', name')) (defs, decls, count) ->
                (
                    (count, name', pr', ISLocal count):defs,
                    (count, Just (pr', name')):decls,
                    count+1))
            ([], [], 0)
            (reverse paramnodes)
    let innerContext = LexicalContext {
        lecxOuter = Just outer,
        lecxNextIndex = paramcount,
        lecxMaxIndex = (paramcount, 0, 0),
        lecxSlots = paramdefs,
        lecxUpvalues = [],
        lecxUpconsts = [],
        lecxVararg = isvararg,
        lecxBlocks = [(0, Nothing, [])]}
    (pbody, context) <- lift $ runStateT (compileBody stats) innerContext
    put $ fromJust $ lecxOuter context
    (main, bbs) <- lift $ link
        pbody
        ((lecxBlocks context), Nothing)
        (IAReturn IAEmpty, [])
    let upvaluedecls = reverse $ map
            (\(name', pr', _, source) -> (source, Just (pr', name')))
            (lecxUpvalues context)
    let upconstdecls = reverse $ map
            (\(name', pr', _, source) -> (source, Just (pr', name')))
            (lecxUpconsts context)
    return $!! IAFunction
        (Just (pr, name))
        upvaluedecls
        upconstdecls
        (reverse paramdecls)
        (let (a, _, _) = lecxMaxIndex context in a)
        (let (_, a, _) = lecxMaxIndex context in a)
        (let (_, _, a) = lecxMaxIndex context in a)
        ((0, main):bbs)
compileFunction _ _ _ = undefined


compileChunk
    :: FilePath
    -> [StatNode]
    -> Either CompileError (Int, Int, Int, IrBody)
compileChunk filename stats = do
    let baseContext = LexicalContext {
        lecxOuter = Nothing,
        lecxNextIndex = 0,
        lecxMaxIndex = (0, 0, 0),
        lecxSlots = [],
        lecxUpvalues = [("_ENV", nullRange filename, 0, Right 0)],
        lecxUpconsts = [],
        lecxVararg = True,
        lecxBlocks = [(0, Nothing, [])]}
    (pbody, context) <- runStateT (compileBody stats) baseContext
    (main, bbs) <- link
        pbody
        ((lecxBlocks context), Nothing)
        (IAReturn IAEmpty, [])
    let (maxl, maxc, maxg) = lecxMaxIndex context
    return $!! (maxl, maxc, maxg, (0, main):bbs)


translateLua
    :: FilePath ->
    B.ByteString ->
    Either String (Int, Int, Int, IrBody)
translateLua filename source = do
    parse <- errToStr $ parseChunk filename source
    errToStr $ compileChunk filename parse
    where
    errToStr :: (Show a) => Either a b -> Either String b
    errToStr (Left x) = Left $ show x
    errToStr (Right y) = Right y


-- tt :: IO ()
-- tt = do
    -- let path = "test.lua"
    -- source <- B.readFile path
    -- case translateLua path source of
        -- Left err -> putStrLn err
        -- Right (maxl, maxc, maxg, body) -> do
            -- let str = shows maxl $ "@ "
                    -- $ shows maxc $ "% "
                    -- $ shows maxg $ "&\n"
                    -- $ defString 0 body $ ""
            -- writeFile "testir.lua" str

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Lua.Translate (
    IrSlot(..),
    IrValue(..),
    IrList(..),
    IrSink(..),
    IrAction(..),
    IrBody,
    translateLua,
) where


import Data.List (foldl')
import Data.Maybe
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.Reader
import Control.Monad.State.Strict
import Lua.DefString
import Lua.Parse
import Lua.SourceRange


data IrSlot
    = ISLocal Int
    | ISConst Int
    | ISGuard Int
    deriving (Eq)


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


lecxSetSlots
    :: (Monad m)
    => [(Int, BSt.ByteString, SourceRange, IrSlot)]
    -> StateT LexicalContext m ()
lecxSetSlots slots = do
    modify' (\lecx -> lecx {
        lecxSlots = slots})


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
      -- onSlot           onUpvalue     onUpconst
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
    let locstack = reverse $ map (\(index,_,_,_) -> index) $ lecxSlots lecx
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
    mapStateT (\(Just x) -> return x) $ lecxCreateBlock Nothing


lecxGetLocalStack
    :: (Monad m) => StateT LexicalContext m [(Int, IrSlot)]
lecxGetLocalStack = do
    lecx <- get
    return $ map (\(index, _, _, slot) -> (index, slot)) $ lecxSlots lecx


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
    -> StateT LexicalContext (Either CompileError) IrValue
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
    -> StateT LexicalContext (Either CompileError) IrList
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
    -> StateT LexicalContext (Either CompileError) IrSink
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
    -> StateT LexicalContext (Either CompileError) IrList
compileExpressionList _ [] Nothing = do
    return $ IAEmpty
compileExpressionList (t:_) [] (Just final) = do
    compileExpressionReadLast t final
compileExpressionList (t:targets) (x:xs) mfinal = do
    IACons
        <$> compileExpressionRead t x
        <*> compileExpressionList targets xs mfinal
compileExpressionList [] _ _ = undefined


type Link t = ReaderT (BlockTable, Maybe Int) (Either CompileError) t


type BodyS m a
    =  IrAction
    -> IrBody
    -> (IrAction, IrBody)


pfindBlock
    :: Int
    -> Link [Int]
pfindBlock ixref = do
    (blocks, _) <- ask
    search blocks
    where
    search [] = error "this shouldn't happen"
    search ((ix, _, stack):rest) = do
        if ix == ixref
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
    search ((_, Nothing, _):rest) = search rest
    search ((ix, Just name, stack):rest) = do
        if name == nameref
            then return $ (ix, stack)
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

compileBody (StatNode (_, StatNull):others) prev = do
    compileBody others prev

compileBody (StatNode (pr, StatAssign lhs rhs mlast):others) prev = do
    let targets = map (defString 0) lhs ++ repeat id
    sourceira <- compileExpressionList targets rhs mlast
    sinks <- forM lhs compileExpressionWrite
    compileBody others (do
        cprev <- prev
        return (\after bbs -> do
            cprev
                (IAMark pr
                    (IAAssign
                        sourceira
                        sinks
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
        return (\_ bbs -> do
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
        return (\_ bbs -> do
            case stackdiff of
                [] -> cprev
                    (IAMark pr $
                        IABlock targetid)
                    bbs
                _ -> cprev
                    (IAMark pr $
                        IADrop stackdiff (IABlock targetid))
                    bbs))

compileBody (StatNode (_, StatDo body):others) prev = do
    pbody <- compileBody body prev
    compileBody others pbody

compileBody (StatNode (_, StatWhile cond body):others) prev = do
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

compileBody (StatNode (_, StatRepeat body cond):others) prev = do
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
    oldscope <- lecxSlots <$> get
    fiterid <- lecxCreateConst "(range iterator)" pr
    paramid <- lecxCreateLocal paramname parampr
    loopid <- lecxNewBlock
    pbody <- compileBody body (return (,))
    lecxSetSlots oldscope
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
            map (\(NameNode (pr', _)) -> pr') lhs
    initlistira <- compileExpressionList
        ([
            "(for iterator)",
            "(for context)",
            "(for index)",
            "(for guard)"] ++ repeat id)
        rhs mlast
    oldscope <- lecxSlots <$> get
    fiterid <- lecxCreateConst "(for iterator)" pr
    fstateid <- lecxCreateConst "(for context)" pr
    findexid <- lecxCreateLocal "(for index)" pr
    fguardid <- lecxCreateGuard "(for guard)" pr
    locals <- forM lhs (\(NameNode (pr', name)) -> do
        lix <- lecxCreateLocal name pr'
        return $ (Just (pr', name), lix))
    let (_, firstlocalid):_ = locals
    loopid <- lecxNewBlock
    pbody <- compileBody body (return (,))
    lecxSetSlots oldscope
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
    let name = BSt.pack $ defString 0 target ""
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
    oldscope <- lecxSlots <$> get
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
    oldscope <- lecxSlots <$> get
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
    makeLocal (NameNode (pr', name), mattr) = do
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
            _ -> lift $ Left $ CompileError pr "Invalid variable attribute"

compileBody (StatNode (pr,
        StatReturn [] (Just (ExprNode (_,
            ExprCall func args mlast)))):others) prev = do
    let argname n = "(argument " . shows (n :: Int) . " of "
            . defString 0 func . ")"
    let argtargets = map argname [1..]
    funcira <- compileExpressionRead "(return)" func
    argira <- compileExpressionList argtargets args mlast
    compileBody others (do
        cprev <- prev
        return (\_ bbs -> do
            cprev
                (IAMark pr
                    (IATailCall
                        funcira
                        argira))
                bbs))

compileBody (StatNode (pr,
        StatReturn [] (Just (ExprNode (_,
            ExprMethodCall obj name args mlast)))):others) prev = do
    let argname n = "(argument " . shows (n :: Int) . " of "
            . defString 0 obj . ":" . unpackSt name . ")"
    let argtargets = map argname [1..]
    objira <- compileExpressionRead "(return)" obj
    let nameira = IAString name
    argira <- compileExpressionList argtargets args mlast
    compileBody others (do
        cprev <- prev
        return (\_ bbs -> do
            cprev
                (IAMark pr
                    (IATailCallMethod
                        objira
                        nameira
                        argira))
                bbs))

compileBody (StatNode (pr, StatReturn rhs mlast):others) prev = do
    let targets = map (\n -> "(return " . shows (n :: Int) . ")") [1..]
    valueira <- compileExpressionList targets rhs mlast
    compileBody others (do
        cprev <- prev
        return (\_ bbs -> do
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
    (pbody, context) <- lift $
        runStateT (compileBody stats (return (,))) innerContext
    put $ fromJust $ lecxOuter context
    cbody <- lift $ runReaderT pbody ((lecxBlocks context), Nothing)
    let (main, bbs) = cbody (IAReturn IAEmpty) []
    let upvaluedecls = reverse $ map
            (\(name', pr', _, source) -> (source, Just (pr', name')))
            (lecxUpvalues context)
    let upconstdecls = reverse $ map
            (\(name', pr', _, source) -> (source, Just (pr', name')))
            (lecxUpconsts context)
    return $ IAFunction
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
    (pbody, context) <- runStateT (compileBody stats (return (,))) baseContext
    cbody <- runReaderT pbody ((lecxBlocks context), Nothing)
    let (main, bbs) = cbody (IAReturn IAEmpty) []
    let (maxl, maxc, maxg) = lecxMaxIndex context
    return $ (maxl, maxc, maxg, (0, main):bbs)


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

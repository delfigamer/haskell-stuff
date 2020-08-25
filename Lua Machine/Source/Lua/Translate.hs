{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Lua.Translate (
    IrAction(..),
    IrBlock(..),
    IrBody,
    IrList(..),
    IrSink(..),
    IrValue(..),
    translateLua,
) where


import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.STRef
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua.DefString
import Lua.Parse
import Lua.SourceRange


data IrValue
    = IANil
    | IABool Bool
    | IAInteger Integer
    | IADouble Double
    | IAString BSt.ByteString
    | IATable IrList [(IrValue, IrValue)]
    | IALocal Int
    | IAUpvalue Int
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
        [Maybe (SourceRange, BSt.ByteString)] -- named arguments
        Int -- max count of locals
        IrBody -- function body
    deriving (Generic, NFData)


data IrList
    = IAArguments
    | IACall IrValue IrList
    | IACallMethod IrValue IrValue IrList
    | IARange IrValue IrValue IrValue
    | IAEmpty
    | IACons IrValue IrList
    deriving (Generic, NFData)


data IrSink
    = IASetLocal Int
    | IASetUpvalue Int
    | IASetIndex IrValue IrValue
    deriving (Generic, NFData)


data IrAction
    = IAMark SourceRange IrAction
    | IAAssign IrList [IrSink] IrAction
    | IAOpen
        IrList
        [(Maybe (SourceRange, BSt.ByteString), Bool, Int)]
        IrAction
    | IASequence IrList IrAction
    | IATest IrValue Int IrAction
    | IAJump Int
    | IAReturn IrList
    | IATailCall IrValue IrList
    | IATailCallMethod IrValue IrValue IrList
    deriving (Generic, NFData)


data IrBlock = IrBlock Int Int IrAction deriving (Generic, NFData)


type IrBody = [IrBlock]


instance DefString IrBody where
    defString _ [] rest = rest
    defString d [IrBlock ix top act] rest
        = "block _" $ shows ix $ " | " $ shows top $ ":"
            $ defBr (d+1) $ defString (d+1) act $ rest
    defString d (IrBlock ix top act:x:xs) rest
        = "block _" $ shows ix $ " | " $ shows top $ ":"
            $ defBr (d+1) $ defString (d+1) act
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
    defString _ (IALocal ix) rest
        = "@" $ shows ix $ rest
    defString _ (IAUpvalue ix) rest
        = "$" $ shows ix $ rest
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
    defString d (IAFunction mlocation upvalues params maxlocals body) rest
        = "(" $ defBr (d+1) $ "function" $ defLocation mlocation
            $ defUpvalues upvalues (0 :: Int)
            $ defParameters params
            $ defBr (d+1) $ "max " $ shows maxlocals $ "@"
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
        defParameters [] rest' = rest'
        defParameters (mloc:xs) rest'
            = defBr (d+1) $ "* arg" $ defLocation mloc $ defParameters xs
                $ rest'


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
    defString d (IAMark pr after) rest
        = "mark " $ shows pr $ ";" $ defBr d $ defString d after $ rest
    defString d (IAAssign source targets after) rest
        = "assign {" $ defString d source $ "} ["
            $ defList ", " d targets $ "];"
            $ defBr d $ defString d after $ rest
    defString d (IAOpen source targets after) rest
        = "open {" $ defString d source $ "} ["
            $ defTargets targets $ "];"
            $ defBr d $ defString d after $ rest
        where
        defTargets [] rest' = rest'
        defTargets ((Nothing, close, ix):xs) rest'
            = defBr (d+1) $ (if close then "!@" else "+@") $ shows ix
                $ defTargets xs $ rest'
        defTargets ((Just (pr, name), close, ix):xs) rest'
            = defBr (d+1) $ (if close then "!@" else "+@") $ shows ix
                $ " --[[ " $ unpackSt name $ " " $ shows pr $ " ]]"
                $ defTargets xs $ rest'
    defString d (IASequence expr after) rest
        = defString d expr $ ";" $ defBr d $ defString d after $ rest
    defString d (IATest cond target after) rest
        = "test " $ defString d cond $ " _" $ shows target $ ";"
            $ defBr d $ defString d after $ rest
    defString _ (IAJump target) rest
        = "jump _" $ shows target $ rest
    defString d (IAReturn x) rest
        = "return {" $ defString d x $ "}" $ rest
    defString d (IATailCall func args) rest
        = "tailcall " $ defString d func $ " {" $ defString d args $ "}" $ rest
    defString d (IATailCallMethod func index args) rest
        = "tailcallmethod " $ defString d func $ " " $ defString d index $ " {"
            $ defString d args $ "}" $ rest


data CompileError = CompileError SourceRange String


instance Show CompileError where
    show (CompileError range msg) = show range ++ ": " ++ msg


data LocalDecl = LocalDecl
    !Int
    !Bool
    !Bool
    !(Maybe (SourceRange, BSt.ByteString))
    !Int


data UpvalueDecl = UpvalueDecl
    !(Either UpvalueDecl LocalDecl)
    !Bool
    !(Maybe (SourceRange, BSt.ByteString))
    !Int


data BlockDecl = BlockDecl
    [LocalDecl]
    !Int


data LabelDecl = LabelDecl
    !SourceRange
    !BSt.ByteString
    !Int


data FunctionContext s = FunctionContext {
    lecxOuter :: Maybe (STRef s (FunctionContext s), [LocalDecl]),
    lecxNextVarname :: !Int,
    lecxMaxIndex :: !Int,
    lecxUpvalues :: [UpvalueDecl],
    lecxVararg :: !Bool,
    lecxBlocks :: [BlockDecl]}


newtype Link = Link {
    runLink
        :: forall r
        .  [LabelDecl]
        -> [BlockDecl]
        -> Maybe Int
        -> IrAction
        -> IrBody
        -> (CompileError -> r)
        -> (IrAction -> IrBody -> r)
        -> r}


instance Semigroup Link where
    alink <> blink = Link $
        \labels blocks mbreak after bbs onError onResult -> do
            runLink blink
                labels blocks mbreak after bbs
                onError $
                    \after2 bbs2 -> do
                        runLink alink
                            labels blocks mbreak after2 bbs2
                            onError onResult


instance Monoid Link where
    mempty = Link $ \_ _ _ after bbs _ onResult -> onResult after bbs


fuseJump :: IrBody -> Int -> Int
fuseJump [] target = target
fuseJump (IrBlock ix _ action:rest) target
    | ix == target = follow action
    | otherwise = fuseJump rest target
    where
    follow (IAMark _ next) = follow next
    follow (IAJump target2) = fuseJump rest target2
    follow _ = fuseJump rest target


linkWithBreak :: Int -> Link -> Link
linkWithBreak ix inner = Link $
    \labels blocks _ -> runLink inner labels blocks (Just ix)


linkWithLabels :: [LabelDecl] -> Link -> Link
linkWithLabels innerlabels inner = Link $
    \labels -> runLink inner (innerlabels ++ labels)


linkAction :: (IrAction -> IrAction) -> Link
linkAction action = Link $
    \_ _ _ after bbs _ onResult -> do
        onResult (action after) bbs


linkJump :: Int -> Link
linkJump target = Link $
    \_ _ _ _ bbs _ onResult -> do
        onResult (IAJump (fuseJump bbs target)) bbs


linkTest :: IrValue -> Int -> Link
linkTest cond target = Link $
    \_ _ _ after bbs _ onResult -> do
        onResult (IATest cond (fuseJump bbs target) after) bbs


newtype Compile s a = Compile {
    runCompile
        :: forall r
        .  STRef s (FunctionContext s)
        -> [LocalDecl]
        -> [LabelDecl]
        -> Link
        -> (CompileError -> ST s r)
        -> ([LocalDecl] -> [LabelDecl] -> Link -> a -> ST s r)
        -> ST s r}


instance Functor (Compile s) where
    fmap = liftM


instance Applicative (Compile s) where
    pure = return
    (<*>) = ap


instance Monad (Compile s) where
    return x = Compile $ \_ stack labels link _ onResult -> do
        onResult stack labels link x
    a >>= f = Compile $ \plecx stack1 labels1 link1 onError onResult -> do
        runCompile a plecx stack1 labels1 link1 onError $
            \stack2 labels2 link2 x -> do
                runCompile (f x) plecx stack2 labels2 link2 onError onResult


cfail :: SourceRange -> String -> Compile s a
cfail pr msg = Compile $
    \_ _ _ _ onError _ -> do
        onError $ CompileError pr msg


compileEmit
    :: Link
    -> Compile s ()
compileEmit mylink = Compile $
    \_ stack labels link _ onResult -> do
        onResult stack labels (link <> mylink) ()


compileOpen
    :: IrList
    -> [(Bool, Maybe (SourceRange, BSt.ByteString), Bool)]
    -> Compile s [Int]
compileOpen sourceira locals = Compile $
    \plecx stack labels link _ onResult -> do
        lecx <- readSTRef plecx
        let beginix = case stack of
                [] -> 0
                LocalDecl _ _ _ _ ix:_ -> ix + 1
        let (varname', stack', endix) = prependLocals
                (lecxNextVarname lecx) stack beginix locals
        let ixes = [beginix .. endix - 1]
        writeSTRef plecx $! lecx {
            lecxNextVarname = varname',
            lecxMaxIndex = max (lecxMaxIndex lecx) (endix - 1)}
        let mylink = linkAction $
                IAOpen
                    sourceira
                    (zipWith
                        (\(_, mdef, toclose) ix -> (mdef, toclose, ix))
                        locals
                        ixes)
        onResult stack' labels (link <> mylink) ixes
    where
    prependLocals varname stack ix [] = do
        (varname, stack, ix)
    prependLocals varname stack ix ((isconst, mdef, toclose):rest) = do
        prependLocals
            (varname + 1)
            (LocalDecl varname isconst toclose mdef ix:stack)
            (ix + 1)
            rest


accessVariable
    :: BSt.ByteString
    -> Compile s (Maybe (Either UpvalueDecl LocalDecl))
accessVariable name = Compile $
    \plecx stack labels link _ onResult -> do
        search plecx stack (onResult stack labels link)
    where
    search plecx stack onResult = do
        case searchStack stack of
            r@(Just _) -> onResult r
            Nothing -> do
                lecx@FunctionContext {
                        lecxOuter = mouter,
                        lecxUpvalues = upvalues}
                    <- readSTRef plecx
                case searchUpvalues upvalues of
                    r@(Just _) -> onResult r
                    Nothing -> do {
    case mouter of
        Nothing -> onResult Nothing
        Just (pouterlecx, outerstack) -> do
            search pouterlecx outerstack $ \mouterdecl -> do
                case mouterdecl of
                    Nothing -> onResult Nothing
                    Just outerdecl -> do
                        let ix = case upvalues of
                                [] -> 0
                                UpvalueDecl _ _ _ uix:_ -> uix + 1
                        let (isconst, mdef) = case outerdecl of
                                Left (UpvalueDecl _ uisconst umdef _) -> do
                                    (uisconst, umdef)
                                Right (LocalDecl _ uisconst _ umdef _) -> do
                                    (uisconst, umdef)
                        let mydecl = UpvalueDecl outerdecl isconst mdef ix
                        writeSTRef plecx $! lecx {
                            lecxUpvalues = mydecl:upvalues}
                        onResult $ Just $ Left $ mydecl
                    }
    searchStack [] = Nothing
    searchStack (decl@(LocalDecl _ _ _ (Just (_, defname)) _):rest)
        | defname == name = Just $ Right $ decl
        | otherwise = searchStack rest
    searchStack (_:rest) = searchStack rest
    searchUpvalues [] = Nothing
    searchUpvalues (decl@(UpvalueDecl _ _ (Just (_, defname)) _):rest)
        | defname == name = Just $ Left $ decl
        | otherwise = searchUpvalues rest
    searchUpvalues (_:rest) = searchUpvalues rest


compileVariable
    :: NameNode
    -> (Bool -> Int -> Compile s a)
    -> (Bool -> Int -> Compile s a)
    -> (IrValue -> IrValue -> Compile s a)
    -> Compile s a
compileVariable (NameNode (_, name)) onLocal onUpvalue onIndex = do
    mdecl <- accessVariable name
    case mdecl of
        Just (Right (LocalDecl _ isconst _ _ ix)) -> do
            onLocal isconst ix
        Just (Left (UpvalueDecl _ isconst _ ix)) -> do
            onUpvalue isconst ix
        Nothing -> do
            menvdecl <- accessVariable "_ENV"
            case menvdecl of
                Just (Right (LocalDecl _ _ _ _ envix)) -> do
                    onIndex (IALocal envix) (IAString name)
                Just (Left (UpvalueDecl _ _ _ envix)) -> do
                    onIndex (IAUpvalue envix) (IAString name)
                Nothing -> undefined


compileSplit
    :: Compile s BlockDecl
compileSplit = Compile $
    \plecx stack labels link _ onResult -> do
        lecx <- readSTRef plecx
        let myix = case lecxBlocks lecx of
                [] -> 0
                BlockDecl _ ix:_ -> ix + 1
        let mytop = case stack of
                [] -> 0
                LocalDecl _ _ _ _ ix:_ -> ix + 1
        let mydecl = BlockDecl stack myix
        writeSTRef plecx $! lecx {lecxBlocks = mydecl:lecxBlocks lecx}
        let mylink = Link $ \_ _ _ after nextbbs _ onLinkResult -> do
            let bbs = IrBlock myix mytop after:nextbbs
            onLinkResult (IAJump (fuseJump bbs myix)) bbs
        onResult stack labels (link <> mylink) mydecl


compileLabel
    :: SourceRange
    -> BSt.ByteString
    -> Compile s ()
compileLabel pr name = do
    BlockDecl _ ix <- compileSplit
    Compile $ \_ stack labels link onError onResult -> do
        case find (\(LabelDecl _ name2 _) -> name2 == name) labels of
            Just (LabelDecl pr2 _ _) -> onError $ CompileError pr $
                "Label " $ unpackSt name $ " is already defined at " $
                    shows pr2 $ ""
            Nothing -> onResult stack (LabelDecl pr name ix:labels) link ()


compileBreak :: SourceRange -> Compile s ()
compileBreak pr = Compile $
    \_ stack labels link _ onResult -> do
        let mylink = Link $ \_ _ mbreak _ bbs onLinkError onLinkResult -> do
            case mbreak of
                Nothing -> onLinkError $ CompileError pr "Invalid break"
                Just target -> do
                    let ix = fuseJump bbs target
                    onLinkResult (IAJump ix) bbs
        onResult stack labels (link <> mylink) ()


compileGoto :: SourceRange -> BSt.ByteString -> Compile s ()
compileGoto pr name = Compile $
    \_ stack labels link _ onResult -> do
        let mylink = Link $ linkGoto (stackpos stack)
        onResult stack labels (link <> mylink) ()
    where
    linkGoto frompos labels blocks _ _ bbs onLinkError onLinkResult = do
        case find (\(LabelDecl _ name2 _) -> name2 == name) labels of
            Nothing -> onLinkError $ CompileError pr $
                "Invalid label " $ unpackSt name $ ""
            Just (LabelDecl _ _ target) -> do
                let ix = fuseJump bbs target
                let Just (BlockDecl tostack _) = find
                        (\(BlockDecl _ myix) -> myix == ix)
                        blocks
                if frompos < (stackpos tostack)
                    then onLinkError $ CompileError pr $
                        "Invalid goto"
                    else onLinkResult (IAJump ix) bbs
    stackpos [] = -1
    stackpos (LocalDecl varname _ _ _ _:_) = varname


compileInner :: Compile s a -> Compile s (Link, a)
compileInner inner = Compile $
    \plecx stack labels link onError onResult -> do
        runCompile inner plecx stack labels mempty onError $
            \_ innerlabels innerlink x -> do
                onResult stack labels link
                    (linkWithLabels innerlabels innerlink, x)


compileScope :: Compile s a -> Compile s a
compileScope inner = do
    (innerlink, x) <- compileInner $ inner
    (splitlink, _) <- compileInner $ compileSplit
    compileEmit $ innerlink
    compileEmit $ splitlink
    return $ x


compileLoop :: (Compile s () -> Compile s a) -> Compile s a
compileLoop innerfunc = do
    (alink, BlockDecl _ aix) <- compileInner $ compileSplit
    (innerlink, x) <- compileInner $ innerfunc $ compileEmit $ linkJump aix
    (blink, BlockDecl _ bix) <- compileInner $ compileSplit
    compileEmit $ alink
    compileEmit $ linkWithBreak bix $ innerlink
    compileEmit $ blink
    return $ x


compileBranch :: IrValue -> Compile s () -> Compile s () -> Compile s ()
compileBranch condira tpath epath = do
    (tlink, _) <- compileInner $ tpath
    (esplit, BlockDecl _ etarget) <- compileInner $ compileSplit
    (elink, _) <- compileInner $ epath
    (asplit, BlockDecl _ atarget) <- compileInner $ compileSplit
    compileEmit $ linkTest condira etarget
    compileEmit $ tlink
    compileEmit $ linkJump atarget
    compileEmit $ esplit
    compileEmit $ elink
    compileEmit $ asplit


compileAction :: (IrAction -> IrAction) -> Compile s ()
compileAction action = compileEmit $ linkAction action


compileLast :: IrAction -> Compile s ()
compileLast lasta = compileEmit $ linkAction $ \_ -> lasta


compileExpressionRead
    :: ShowS
    -> ExprNode
    -> Compile s IrValue
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
    compileVariable namenode
        (\_ ix -> do
            return $ IALocal ix)
        (\_ ix -> do
            return $ IAUpvalue ix)
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
    -> Compile s IrList
compileExpressionReadLast _ (ExprNode (pr, ExprEllipsis)) = Compile $
    \plecx stack labels link onError onResult -> do
        lecx <- readSTRef plecx
        if lecxVararg lecx
            then onResult stack labels link IAArguments
            else onError $ CompileError pr $
                "Ellipsis must appear inside a vararg function"
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
    -> Compile s IrSink
compileExpressionWrite (ExprNode (pr, ExprVar namenode)) = do
    compileVariable namenode
        (\isconst ix -> do
            if isconst
                then errConst
                else return $ IASetLocal ix)
        (\isconst ix -> do
            if isconst
                then errConst
                else return $ IASetUpvalue ix)
        (\table index -> do
            return $ IASetIndex table index)
    where
    errConst = cfail pr $
        "Cannot assign to an immutable variable " $ defString 0 namenode $ ""
compileExpressionWrite (ExprNode (_, ExprIndex table index)) = do
    IASetIndex
        <$> compileExpressionRead "(table)" table
        <*> compileExpressionRead "(index)" index
compileExpressionWrite (ExprNode (pr, _)) = do
    cfail pr "A variable or table field expected"


compileExpressionList
    :: [ShowS]
    -> [ExprNode]
    -> Maybe ExprNode
    -> Compile s IrList
compileExpressionList _ [] Nothing = do
    return $ IAEmpty
compileExpressionList (t:_) [] (Just final) = do
    compileExpressionReadLast t final
compileExpressionList (t:targets) (x:xs) mfinal = do
    IACons
        <$> compileExpressionRead t x
        <*> compileExpressionList targets xs mfinal
compileExpressionList [] _ _ = undefined


compileStat
    :: StatNode
    -> Compile s ()

compileStat (StatNode (_, StatNull)) = do
    return ()

compileStat (StatNode (pr, StatAssign lhs rhs mlast)) = do
    let targets = map (defString 0) lhs ++ repeat id
    sourceira <- compileExpressionList targets rhs mlast
    sinks <- forM lhs compileExpressionWrite
    compileAction $ IAMark pr . IAAssign sourceira sinks

compileStat (StatNode (pr, StatInvoke expr)) = do
    case expr of
        ExprNode (_, ExprCall _ _ _) -> return ()
        ExprNode (_, ExprMethodCall _ _ _ _) -> return ()
        _ -> cfail pr "Function call expected"
    exprira <- compileExpressionReadLast "(invoke)" expr
    compileAction $ IAMark pr . IASequence exprira

compileStat (StatNode (pr, StatLabel label)) = do
    let (NameNode (_, name)) = label
    compileLabel pr name
    compileAction $ IAMark pr

compileStat (StatNode (pr, StatBreak)) = do
    compileAction $ IAMark pr
    compileBreak pr

compileStat (StatNode (pr, StatGoto label)) = do
    let NameNode (_, name) = label
    compileAction $ IAMark pr
    compileGoto pr name

compileStat (StatNode (_, StatDo body)) = do
    compileScope $ mapM_ compileStat body

compileStat (StatNode (_, StatWhile cond body)) = do
    let (ExprNode (condpr, _)) = cond
    compileLoop $ \compileNext -> do
        condira <- compileExpressionRead "(while condition)" cond
        compileAction $ IAMark condpr
        compileBranch condira
            (mapM_ compileStat body >> compileNext)
            (return ())

compileStat (StatNode (_, StatRepeat body cond)) = do
    let (ExprNode (condpr, _)) = cond
    compileLoop $ \compileNext -> do
        mapM_ compileStat body
        condira <- compileExpressionRead "(repeat condition)" cond
        compileAction $ IAMark condpr
        compileBranch condira
            (return ())
            compileNext

compileStat (StatNode (_, StatIf cond tbody estat)) = do
    let ExprNode (condpr, _) = cond
    condira <- compileExpressionRead "(if condition)" cond
    compileAction $ IAMark condpr
    compileBranch condira
        (compileScope $ mapM_ compileStat tbody)
        (compileStat estat)

compileStat (StatNode (pr, StatForNum param start limit mdelta body)) = do
    let (NameNode (parampr, paramname)) = param
    let headpr = case (start, mdelta) of
            (ExprNode (pra, _), Nothing) -> pra
            (ExprNode (pra, _), Just (ExprNode (prb, _))) -> pra <> prb
    compileScope $ do
        compileAction $ IAMark headpr
        startira <- compileExpressionRead "(range start)" start
        limitira <- compileExpressionRead "(range limit)" limit
        deltaira <- case mdelta of
                Just delta -> compileExpressionRead "(range delta)" delta
                Nothing -> return $ IAInteger 1
        ~[fiterid] <- compileOpen
            (IARange startira limitira deltaira)
            [(True, Just (pr, "(range iterator)"), False)]
        compileLoop $ \compileNext -> do
            compileAction $ IAMark parampr
            ~[paramid] <- compileOpen
                (IACall (IALocal fiterid) IAEmpty)
                [(False, Just (parampr, paramname), False)]
            compileBranch (IALocal paramid)
                (mapM_ compileStat body >> compileNext)
                (return ())

compileStat (StatNode (pr, StatForEach lhs rhs mlast body)) = do
    let lhspr = foldl' (<>) (collapseRangeNull pr) $
            map (\(NameNode (pr', _)) -> pr') lhs
    let rhspr = foldl' (<>) (collapseRangeNull pr) $
            map (\(NameNode (pr', _)) -> pr') lhs
    let headpr = case mlast of
            Just (ExprNode (prb, _)) -> rhspr <> prb
            Nothing -> rhspr
    compileAction $ IAMark headpr
    initlistira <- compileExpressionList
        ([
            "(for iterator)",
            "(for context)",
            "(for index)",
            "(for guard)"] ++ repeat id)
        rhs mlast
    compileScope $ do
        compileAction $ IAMark lhspr
        ~[fiterid, fstateid, findexid, _] <- compileOpen
            initlistira
            [
                (True, Just (pr, "(for iterator)"), False),
                (True, Just (pr, "(for context)"), False),
                (False, Just (pr, "(for index)"), False),
                (True, Just (pr, "(for context)"), True)]
        compileLoop $ \compileNext -> do
            ~(firstlocalid:_) <- compileOpen
                (IACall
                    (IALocal fiterid)
                    (IACons
                        (IALocal fstateid)
                        (IACons
                            (IALocal findexid)
                            IAEmpty)))
                (map
                    (\(NameNode (pr', name)) -> do
                        (False, Just (pr', name), False))
                    lhs)
            compileAction $ IAAssign
                (IACons (IALocal firstlocalid) IAEmpty)
                [IASetLocal findexid]
            compileBranch (IALocal findexid)
                (mapM_ compileStat body >> compileNext)
                (return ())

compileStat (StatNode (pr, StatFunction target value)) = do
    let ExprNode (targetpr, _) = target
    let name = BSt.pack $ defString 0 target ""
    functionira <- compileFunction pr name value
    targetira <- compileExpressionWrite target
    compileAction $ IAMark targetpr . IAAssign
        (IACons functionira IAEmpty)
        [targetira]

compileStat (StatNode (pr, StatLocalFunction namenode value)) = do
    let (NameNode (namepr, name)) = namenode
    ~[lid] <- compileOpen
        IAEmpty
        [(False, Just (namepr, name), False)]
    functionira <- compileFunction pr name value
    compileAction $ IAMark pr
    compileAction $ IAAssign
        (IACons functionira IAEmpty)
        [IASetLocal lid]

compileStat (StatNode (pr, StatLocalDef lhs rhs mlast)) = do
    let targets = map (\(name, _) -> defString 0 name) lhs ++ repeat id
    sourcesira <- compileExpressionList targets rhs mlast
    localdefs <- forM lhs $ \(NameNode (pr', name), mattr) -> do
        case mattr of
            Nothing -> do
                return $ (False, Just (pr', name), False)
            Just "const" -> do
                return $ (True, Just (pr', name), False)
            Just "close" -> do
                return $ (True, Just (pr', name), True)
            Just attr -> cfail pr $ "Unknown attribute <" $ unpackSt attr $ ">"
    compileAction $ IAMark pr
    _ <- compileOpen sourcesira localdefs
    return ()

compileStat (StatNode (pr, StatReturn rhs mlast)) = do
    hasGuards <- checkGuards
    case (hasGuards, rhs, mlast) of
        (False, [], Just (ExprNode (_,
                ExprCall func args arglast))) -> do
            let argname n = "(argument " . shows (n :: Int) . " of "
                    . defString 0 func . ")"
            let argtargets = map argname [1..]
            funcira <- compileExpressionRead "(return)" func
            argira <- compileExpressionList argtargets args arglast
            compileLast $
                IAMark pr $
                    IATailCall funcira argira
        (False, [], Just (ExprNode (_,
                ExprMethodCall obj name args arglast))) -> do
            let argname n = "(argument " . shows (n :: Int) . " of "
                    . defString 0 obj . ":" . unpackSt name . ")"
            let argtargets = map argname [1..]
            objira <- compileExpressionRead "(return)" obj
            let nameira = IAString name
            argira <- compileExpressionList argtargets args arglast
            compileLast $
                IAMark pr $
                    IATailCallMethod objira nameira argira
        _ -> do
            let targets = map (\n -> "(return " . shows (n :: Int) . ")") [1..]
            valueira <- compileExpressionList targets rhs mlast
            compileLast $
                IAMark pr $
                    IAReturn valueira
    where
    checkGuards = Compile $
        \_ stack labels link _ onResult -> do
            onResult stack labels link $
                any (\(LocalDecl _ _ toclose _ _) -> toclose) $ stack


filterBody :: IrBody -> IrBody
filterBody bbs@(IrBlock mainix _ main:_) = do
    let liveixs = walk [mainix] main
    filter (\(IrBlock ix _ _) -> ix `elem` liveixs) bbs
    where
    walk liveixs ira = do
        case ira of
            IAMark _ after -> walk liveixs after
            IAAssign _ _ after -> walk liveixs after
            IAOpen _ _ after -> walk liveixs after
            IASequence _ after -> walk liveixs after
            IATest _ target after -> do
                let mid = walk liveixs after
                if target `elem` mid
                    then mid
                    else walk (target:mid) $ getblock target
            IAJump target -> do
                if target `elem` liveixs
                    then liveixs
                    else walk (target:liveixs) $ getblock target
            IAReturn _ -> liveixs
            IATailCall _ _ -> liveixs
            IATailCallMethod _ _ _ -> liveixs
    getblock ix = do
        let Just (IrBlock _ _ ira) = find
                (\(IrBlock ix2 _ _) -> ix2 == ix) bbs
        ira
filterBody _ = undefined


buildBody
    :: FunctionContext s
    -> [LocalDecl]
    -> Compile s ()
    -> (CompileError -> ST s r)
    -> (IrBody -> Int -> [UpvalueDecl] -> ST s r)
    -> ST s r
buildBody lecx1 locals act onError onResult = do
    plecx <- newSTRef $! lecx1
    runCompile act plecx locals [] mempty onError $
        \_ innerlabels innerlink _ -> do
            lecx2 <- readSTRef plecx
            runLink innerlink
                innerlabels (lecxBlocks lecx2) Nothing
                (IAReturn IAEmpty) []
                onError $ \main bbs -> do
                    let maintop = case locals of
                            [] -> 0
                            LocalDecl _ _ _ _ ix:_ -> ix + 1
                    onResult
                        (filterBody (IrBlock 0 maintop main:bbs))
                        (lecxMaxIndex lecx2)
                        (lecxUpvalues lecx2)


compileFunction
    :: SourceRange
    -> BSt.ByteString
    -> ExprNode
    -> Compile s IrValue
compileFunction pr name (ExprNode (_,
        ExprFunction paramnodes mvarargnode stats)) = Compile $
    \pouterlecx outerlocals outerlabels outerlink onError onResult -> do
        let (paramlocals, paramdecls, paramcount) = foldr
                (\(NameNode (pr', name')) (locals, decls, count) -> do
                    let mdef = Just (pr', name')
                    (
                        (LocalDecl count False False mdef count):locals,
                        mdef:decls,
                        count+1))
                ([], [], 0)
                (reverse paramnodes)
        let innerlecx = FunctionContext {
            lecxOuter = Just (pouterlecx, outerlocals),
            lecxNextVarname = paramcount,
            lecxMaxIndex = paramcount - 1,
            lecxUpvalues = [],
            lecxVararg = isJust mvarargnode,
            lecxBlocks = [BlockDecl [] 0]}
        buildBody innerlecx paramlocals (mapM_ compileStat stats) onError $
            \innerbody maxindex upvalues -> do
                let upvaluedecls = reverse $ map
                        (\(UpvalueDecl source _ mdef _) -> do
                            case source of
                                Left (UpvalueDecl _ _ _ ix) -> do
                                    (Left ix, mdef)
                                Right (LocalDecl _ _ _ _ ix) -> do
                                    (Right ix, mdef))
                        upvalues
                onResult
                    outerlocals
                    outerlabels
                    outerlink
                    $!! IAFunction
                        (Just (pr, name))
                        upvaluedecls
                        (reverse paramdecls)
                        maxindex
                        innerbody
compileFunction _ _ _ = undefined


compileChunk
    :: FilePath
    -> [StatNode]
    -> (CompileError -> ST s r)
    -> (Int -> IrBody -> ST s r)
    -> ST s r
compileChunk filename stats onError onResult = do
    let lecx = FunctionContext {
        lecxOuter = Nothing,
        lecxNextVarname = 0,
        lecxMaxIndex = -1,
        lecxUpvalues = [UpvalueDecl
            (Right $ LocalDecl 0 False False Nothing 0)
            False
            (Just (nullRange filename, "_ENV"))
            0],
        lecxVararg = True,
        lecxBlocks = [BlockDecl [] 0]}
    buildBody lecx [] (mapM_ compileStat stats) onError $
        \innerbody maxindex _ -> do
            (onResult $! maxindex) $!! innerbody


translateLua
    :: FilePath ->
    B.ByteString ->
    Either String (Int, IrBody)
translateLua filename source = do
    case parseChunk filename source of
        Left err -> Left $ show err
        Right parse -> do
            runST $ compileChunk filename parse
                (\err -> return $ Left $ show err)
                (\maxindex body -> return $ Right $ (maxindex, body))


-- tt :: IO ()
-- tt = do
    -- let path = "test.lua"
    -- source <- B.readFile path
    -- case translateLua path source of
        -- Left err -> putStrLn err
        -- Right (maxl, body) -> do
            -- let str = "max " $ shows maxl $ "@\n" $ defString 0 body $ ""
            -- writeFile "testir.lua" str

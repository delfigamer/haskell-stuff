{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Debug (
    LuaFrameReturn,
    LuaStackFrame(..),
    SourceRange,
    luadGetDebugHook,
    luadGetStack,
    luadGetTraceback,
    luadRunFunction,
    luadSetDebugHook,
    luadSetLocation,
    luadSetMetatable,
    luadSetStackLimit,
    luadWithinLocal,
) where


import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BSt
import Lua.Core
import Lua.SourceRange


luadGetTraceback
    :: LuaState q s [(Maybe BSt.ByteString, Maybe SourceRange)]
luadGetTraceback = do
    stack <- lxAskStack
    forM stack (\frame -> do
        let fname = snd <$> lsfDefinition frame
        let locref = lsfCurrentLocation frame
        loc <- lxRead locref
        return $ (fname, loc))


luadGetStack :: LuaState q s [LuaStackFrame q s]
luadGetStack = lxAskStack


luadModifyStackTop
    :: (LuaStackFrame q s -> LuaStackFrame q s)
    -> [LuaStackFrame q s]
    -> [LuaStackFrame q s]
luadModifyStackTop func (frame:fs) = func frame:fs
luadModifyStackTop _ [] = []


luadSetLocation
    :: Maybe SourceRange
    -> LuaState q s ()
luadSetLocation mpr = do
    stack <- lxAskStack
    case stack of
        frame:_ -> do
            let locationRef = lsfCurrentLocation frame
            lxWrite locationRef mpr
        _ -> return ()
    (hookf, _, _, lineflag) <- lxGetDebugHook
    when lineflag (do
        let mline = do
            SourceRange (_, Just (srow, _, _, _)) <- mpr
            Just $ LInteger $ toInteger srow
        () <$ lxCall hookf [LString "line", fromMaybe LNil mline])


luadWithinLocal
    :: Maybe (SourceRange, BSt.ByteString)
    -> LuaRef q s (LuaValue q s)
    -> LuaState q s t
    -> LuaState q s t
luadWithinLocal mdef slot act = do
    lxLocalStack
        (luadModifyStackTop $ lsfModifyLocals $
            ((mdef, slot):))
        act


type LuaFrameReturn q s =
    Either (LuaFunction q s, [LuaValue q s]) [LuaValue q s]


luadRunFunction
    :: Maybe (SourceRange, BSt.ByteString)
    -> LuaVariableList q s
    -> LuaVariableList q s
    -> LuaState q s (LuaFrameReturn q s)
    -> LuaState q s [LuaValue q s]
luadRunFunction mdef upvalues args act = do
    locationRef <- lxAlloc $ fst <$> mdef
    let lstackFrame = LuaStackFrame {
        lsfDefinition = mdef,
        lsfCurrentLocation = locationRef,
        lsfUpvalues = upvalues,
        lsfLocals = args}
    result <- lxStackLevel $ lxLocalStack (lstackFrame:) $ do
        (callhookf, callflag, _, _) <- lxGetDebugHook
        when callflag (do
            () <$ lxCall callhookf [LString "call"])
        result' <- act
        (rethookf, _, retflag, _) <- lxGetDebugHook
        when retflag (do
            () <$ lxCall rethookf [
                LString $ either (const "tail call") (const "return") result'])
        return $ result'
    case result of
        Left (tailfunc, args') -> tailfunc args'
        Right rets -> return rets


luadGetDebugHook
    :: LuaState q s (LuaValue q s, Bool, Bool, Bool)
luadGetDebugHook = lxGetDebugHook


luadSetDebugHook
    :: (LuaValue q s, Bool, Bool, Bool)
    -> LuaState q s ()
luadSetDebugHook = lxSetDebugHook


luadSetStackLimit
    :: Int -> LuaState q s (Maybe Int)
luadSetStackLimit = lxSetStackLimit


luadSetMetatable
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luadSetMetatable a b = do
    meta <- case b of
        LTable _ _ -> return $ lxProduceMetatable b
        LNil -> return $ lxDefaultMetatable
        _ -> lxError $ errWrongMetatableValue
    case a of
        LNil -> lxSetNilMetatable meta
        LBool _ -> lxSetBoolMetatable meta
        LInteger _ -> lxSetNumberMetatable meta
        LRational _ -> lxSetNumberMetatable meta
        LDouble _ -> lxSetNumberMetatable meta
        LString _ -> lxSetStringMetatable meta
        LFunction _ _ -> lxSetFunctionMetatable meta
        LThread _ _ -> lxSetThreadMetatable meta
        LTable _ (LuaTable _ pmeta) -> lxWrite pmeta $ meta
        _ -> lxError $ errWrongMetatableOwner a

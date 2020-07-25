module Lua.Debug (
    luadSetLocation,
    luadWithinFunction,
    luadWithinLocal,
) where


import qualified Data.ByteString.Char8 as BSt
import Lua.Core
import Lua.SourceRange


luadModifyStackTop
    :: (LuaStackFrame q s -> LuaStackFrame q s)
    -> [LuaStackFrame q s]
    -> [LuaStackFrame q s]
luadModifyStackTop func (frame:fs) = func frame:fs
luadModifyStackTop func [] = []


luadWithinLocal
    :: Maybe (SourceRange, BSt.ByteString)
    -> Either (LuaValue q s) (LuaRef q s (LuaValue q s))
    -> LuaState q s t
    -> LuaState q s t
luadWithinLocal mdef slot act = do
    lxLocalStack
        (luadModifyStackTop $ lsfModifyLocals $
            ((mdef, slot):))
        act


luadWithinFunction
    :: Maybe (SourceRange, BSt.ByteString)
    -> LuaVariableList q s
    -> LuaVariableList q s
    -> LuaState q s t
    -> LuaState q s t
luadWithinFunction mdef upvalues args act = do
    locationRef <- lxAlloc Nothing
    let lstackFrame = LuaStackFrame {
        lsfDefinition = mdef,
        lsfCurrentLocation = locationRef,
        lsfUpvalues = upvalues,
        lsfLocals = args}
    lxLocalStack
        (lstackFrame:)
        act


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

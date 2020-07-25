{-# LANGUAGE RankNTypes #-}

module Lua (
    LuaErrorHandler,
    LuaFunction,
    LuaMetatable(..),
    LuaPure(..),
    LuaPush(..),
    LuaRef,
    LuaState,
    LuaValue(..),
    errArgType,
    errDivideZero,
    errNilIndex,
    errNonSuspended,
    errWrongArith1,
    errWrongArith2,
    errWrongBit,
    errWrongCall,
    errWrongCompare,
    errWrongConcat,
    errWrongIO,
    errWrongLen,
    errWrongResume,
    errWrongMetatableOwner,
    errWrongMetatableValue,
    errWrongRangeIter,
    errWrongTable,
    errWrongThreadFunc,
    errWrongYield,
    errZeroStep,
    luaAlloc,
    luaArithAdd,
    luaArithBAnd,
    luaArithBNot,
    luaArithBOr,
    luaArithBXor,
    luaArithDiv,
    luaArithIDiv,
    luaArithMul,
    luaArithMod,
    luaArithPow,
    luaArithShl,
    luaArithShr,
    luaArithSub,
    luaArithUnm,
    luaCall,
    luaCloseAfter,
    luaCompareLt,
    luaCompareLe,
    luaCompareEq,
    luaConcat,
    luaCreateFunction,
    luaCreatePureUserdata,
    luaCreateTable,
    luaCreateThread,
    luaCreateUserdata,
    luaError,
    luaExtend,
    luaFromUserdata,
    luaGet,
    luaGetMetatable,
    luaLen,
    luaLiftIO,
    luaLiftST,
    luaNewTable,
    luaNewThread,
    luaPCall,
    luaRangeIter,
    luaRawEqual,
    luaRawGet,
    luaRawLen,
    luaRawSet,
    luaRead,
    luaResume,
    luaRunT,
    luaSet,
    luaSetMetatable,
    luaToBoolean,
    luaToDouble,
    luaToInteger,
    luaToFunction,
    luaToPureUserdata,
    luaToRational,
    luaToString,
    luaToUserdata,
    luaTry,
    luaWrite,
    luaXPCall,
    luaXTry,
    luaYield,
    luadSetLocation,
    luadWithinFunction,
    luadWithinLocal,
    luaLoad,
    lualibs,
    luaRunST,
    luaRunIO,
) where

import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSt
import Lua.Common
import Lua.Debug
import Lua.Interpret
import Lua.Lib


luaRunST
    :: (forall q . LuaState q s t)
    -> ST s (Either String t)
luaRunST = luaRunT resolveST resolveIO resolveYield onError onPure
    where
    resolveST act = act
    resolveIO act = return $ Nothing
    resolveYield vals = return $ Left $ errWrongYield
    onError (LString msg)
        = return $ Left $ BSt.unpack msg
    onError _ = return $ Left $ "Unknown error"
    onPure x = return $ Right $ x


luaRunIO
    :: (forall q . LuaState q RealWorld t)
    -> IO (Either String t)
luaRunIO = luaRunT resolveST resolveIO resolveYield onError onPure
    where
    resolveST act = stToIO $ act
    resolveIO act = Just <$> act
    resolveYield vals = return $ Left $ errWrongYield
    onError (LString msg)
        = return $ Left $ BSt.unpack msg
    onError _ = return $ Left $ "Unknown error"
    onPure x = return $ Right $ x


module Expr (
    SExpr(..),
    SExprValue(..),
) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BSt
import Text.Parsec.Pos
import qualified Number as N
import SourceRange

data SExprValue
    = SAtom BSt.ByteString
    | SNumber N.Number
    | SChar Char
    | SString BSt.ByteString
    | SBool Bool
    | SVector [SExpr]
    | SCons SExpr SExpr
    | SNull
newtype SExpr = SExpr (SourceRange, SExprValue) deriving (Show)

showcons :: SExprValue -> String
showcons (SCons car (SExpr (_, cdr@(SCons _ _))))
    = show car ++ " " ++ showcons cdr
showcons (SCons car (SExpr (_, SNull))) = show car
showcons (SCons car cdr) = show car ++ " . " ++ show cdr

instance Show SExprValue where
    show (SAtom name) = BSt.unpack name
    show (SNumber x) = show x
    show (SChar '\n') = "#\\newline"
    show (SChar ' ') = "#\\space"
    show (SChar c) = ['#', '\\', c]
    show (SString cont) = show cont
    show (SBool False) = "#f"
    show (SBool True) = "#t"
    show (SVector xs) = "#(" ++ L.intercalate " " (map show xs) ++ ")"
    show x@(SCons _ _) = "(" ++ showcons x ++ ")"
    show SNull = "()"

module SourceRange (
    SourceRange,
    range,
    rangeStart,
    rangeEnd,
) where

import Text.Parsec.Pos
import Data.Semigroup (stimes, stimesIdempotent)

newtype SourceRange = SourceRange (SourcePos, SourcePos)

instance Show SourceRange where
    show (SourceRange (spos, epos))
        =  sourceName spos
        ++ ":"
        ++ show (sourceLine spos) ++ ":" ++ show (sourceColumn spos)
        ++ "-"
        ++ show (sourceLine epos) ++ ":" ++ show (sourceColumn epos)

instance Semigroup SourceRange where
    SourceRange (spos, _) <> SourceRange (_, epos)
        = SourceRange (spos, epos)
    stimes = stimesIdempotent

range :: SourcePos -> SourcePos -> SourceRange
range spos epos = SourceRange (spos, epos)

rangeStart :: SourceRange -> SourcePos
rangeStart (SourceRange (spos, epos)) = spos

rangeEnd :: SourceRange -> SourcePos
rangeEnd (SourceRange (spos, epos)) = epos

module Lua.SourceRange (
    SourceName,
    SourceRange,
    collapseRangeLeft,
    collapseRangeNull,
    collapseRangeRight,
    newRange,
    nullRange,
    rangeLeft,
    rangeRight,
) where

import Text.Parsec.Pos
import Data.Semigroup (stimes, stimesIdempotent)

newtype SourceRange
    = SourceRange (SourceName, Maybe (Line, Column, Line, Column))

instance Show SourceRange where
    show (SourceRange (fname, Just (srow, scol, erow, ecol)))
        =  fname
        ++ ":"
        ++ show srow ++ ":" ++ show scol
        ++ "-"
        ++ show erow ++ ":" ++ show ecol
    show (SourceRange (fname, Nothing))
        =  fname

instance Semigroup SourceRange where
    a <> (SourceRange (_, Nothing))
        = a
    (SourceRange (_, Nothing)) <> b
        = b
    (SourceRange (fname, Just (srow, scol, _, _)))
        <> (SourceRange (_, Just (_, _, erow, ecol)))
        = SourceRange (fname, Just (srow, scol, erow, ecol))
    stimes = stimesIdempotent

newRange :: SourcePos -> SourcePos -> SourceRange
newRange spos epos
    = SourceRange (
        sourceName spos,
        Just (
            sourceLine spos,
            sourceColumn spos,
            sourceLine epos,
            sourceColumn epos))

nullRange :: SourceName -> SourceRange
nullRange fname = SourceRange (fname, Nothing)

collapseRangeNull :: SourceRange -> SourceRange
collapseRangeNull (SourceRange (fname, _))
    = SourceRange (fname, Nothing)

collapseRangeLeft :: SourceRange -> SourceRange
collapseRangeLeft (SourceRange (fname, Just (srow, scol, erow, ecol)))
    = SourceRange (fname, Just (srow, scol, srow, scol))
collapseRangeLeft (SourceRange (fname, Nothing))
    = SourceRange (fname, Nothing)

collapseRangeRight :: SourceRange -> SourceRange
collapseRangeRight (SourceRange (fname, Just (srow, scol, erow, ecol)))
    = SourceRange (fname, Just (erow, ecol, erow, ecol))
collapseRangeRight (SourceRange (fname, Nothing))
    = SourceRange (fname, Nothing)

rangeLeft :: SourceRange -> SourcePos
rangeLeft (SourceRange (fname, Just (srow, scol, erow, ecol)))
    = newPos fname srow scol
rangeLeft (SourceRange (fname, Nothing))
    = initialPos fname

rangeRight :: SourceRange -> SourcePos
rangeRight (SourceRange (fname, Just (srow, scol, erow, ecol)))
    = newPos fname erow ecol
rangeRight (SourceRange (fname, Nothing))
    = initialPos fname

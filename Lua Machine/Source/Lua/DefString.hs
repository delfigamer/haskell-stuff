{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Lua.DefString (
    DefString(..),
    IsString(..),
    defBr,
    defList,
    unpackSt,
) where


import Data.String (IsString(..))
import Data.ByteString.Char8 (ByteString, unpack)


class DefString a where
    defString :: Int -> a -> ShowS


defBr :: Int -> ShowS
defBr d rest = ('\n' : replicate (d*4) ' ') ++ rest


defList :: DefString a => ShowS -> Int -> [a] -> ShowS
defList _ _ [] rest = rest
defList _ d [x] rest = defString d x $ rest
defList sep d (x:y:ys) rest = defString d x $ sep $ defList sep d (y:ys) $ rest


instance IsString ShowS where
    fromString "" = id
    fromString [c] = (c:)
    fromString s = (s ++)


unpackSt :: ByteString -> ShowS
unpackSt s rest = map (toEnum . fromEnum) (unpack s) ++ rest

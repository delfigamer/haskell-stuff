module Main where
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Lua


testfunc :: LuaState q s ()
testfunc = do
    let path = "..\\test.lua"
    input <- luaLiftIO $ B.readFile path
    env <- lualibs
    chunk <- luaLoad path input env
    luaCall chunk []
    return ()


test :: IO ()
test = do
    r <- luaRunIO $ testfunc
    case r of
        Left e -> putStrLn $ e
        Right i -> print $ i


main :: IO ()
main = do
    test
    return ()

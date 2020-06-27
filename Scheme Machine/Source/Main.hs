module Main where
import System.Environment (getArgs)
import Control.Monad (forM_)
import GHC.IORef

main :: IO ()
main = do
    args <- getArgs
    forM_ args (\a ->
        putStrLn a)



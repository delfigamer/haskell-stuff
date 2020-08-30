module Main where


import Control.Monad
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Lua


data Action
    = AExecute String
    | ARequire String
    deriving (Show)


data Arguments = Arguments {
    aActions :: [Action],
    aInteractive :: Bool,
    aVersion :: Bool,
    aArgOffset :: Int,
    aFile :: Maybe String,
    aRest :: [String]}
    deriving (Show)


parseArgs :: [String] -> IO (Maybe Arguments)
parseArgs args0 = do
    case args0 of
        [] -> return $ Just $ Arguments {
            aActions = [],
            aInteractive = True,
            aVersion = True,
            aArgOffset = 1,
            aFile = Nothing,
            aRest = []}
        _ -> do
            doParse 1 args0 $ Arguments {
                aActions = [],
                aInteractive = False,
                aVersion = False,
                aArgOffset = 1,
                aFile = Nothing,
                aRest = []}
    where
    doParse n args state = do
        case args of
            [] -> do
                return $ Just $ state
            "-e":stat:rest -> do
                doParse (n+2) rest $ state {
                    aActions = aActions state ++ [AExecute stat]}
            ["-e"] -> do
                putStrLn $ "statement expected after -e"
                return $ Nothing
            "-l":modname:rest -> do
                doParse (n+2) rest $ state {
                    aActions = aActions state ++ [ARequire modname]}
            ["-l"] -> do
                putStrLn $ "module name expected after -l"
                return $ Nothing
            "-i":rest -> do
                doParse (n+1) rest $ state {
                    aInteractive = True}
            "-v":rest -> do
                doParse (n+1) rest $ state {
                    aVersion = True}
            file:rest
                | take 1 file /= "-" -> do
                    return $ Just $ state {
                        aArgOffset = n-1,
                        aFile = Just file,
                        aRest = rest}
                | otherwise -> do
                    putStrLn $ "invalid argument " ++ file
                    return $ Nothing


main :: IO ()
main = do
    args <- getArgs
    margstate <- parseArgs args
    case margstate of
        Just argstate -> do
            out <- luaRunIO $ do
                env <- lualibs
                require <- luaGet env (luaPush "require")
                (luaSet env (luaPush "args") =<<) $ luaCreateTable $ zipWith
                    (\i v -> (LInteger $ toInteger i, luaPush v))
                    [-aArgOffset argstate ..]
                    args
                forM_ (aActions argstate) $ \act -> do
                    case act of
                        AExecute str -> do
                            luaDo_ (B.pack str) env []
                        ARequire str -> do
                            ~(a :| _) <- luaCall require [luaPush str]
                            case a of
                                LNil -> return ()
                                _ -> luaSet env (luaPush str) a
                case aFile argstate of
                    Just file -> do
                        let chunkargs = map luaPush $
                                drop (aArgOffset argstate + 1) args
                        lret <- luaLoadFile file env
                        case lret of
                            Left err -> luaError $ err
                            Right chunk -> () <$ luaCall chunk chunkargs
                    Nothing -> return ()
            case out of
                Left err -> putStrLn err
                Right _ -> return ()
        Nothing -> return ()
    return ()

module Main where

import           Control.Monad.State
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import qualified Data.Set                 as Set
import qualified Data.Map                 as Map

import           LLVM.AST
import           LLVM.Internal.DataLayout

import qualified Lexer                    as L
import qualified Parser                   as P
import           JIT
import           Error
import qualified AST                      as S
import qualified Modules                  as M
import           Monad


data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , astOnly     :: Bool
    , modulesOnly :: Bool
    , lexOnly     :: Bool
    , filenames   :: [String]
    }
initArgs = Args
    { verbose     = False
    , optimise    = True
    , astOnly     = False
    , modulesOnly = False
    , lexOnly     = False
    , filenames   = []
    }


main :: IO ()
main = do
    args <- fmap (parseArgs initArgs) getArgs

    sources <- forM (filenames args) $ \name -> do
        src <- readFile name
        return (name, src)

    withSession (optimise args) $ \session -> do
        if lexOnly args then do
            forM_ sources $ \(filename, src) -> do
                let res = L.alexScanner filename src
                putStrLn (show res)
        else if sources == [] then
            error "no repl"
        else if astOnly args then do
            forM_ sources $ \(filename, src) -> do
                case parse filename src of
                    Left err  -> printError err (Map.fromList sources)
                    Right ast -> S.prettyAST "" ast

        else do
            res <- runBoMT (M.initModulesState session) $ M.runFiles (filenames args) (verbose args)
            case res of
                Left err            -> printError err (Map.fromList sources)
                Right (_, modState) -> when (modulesOnly args) (M.prettyModules modState)

    where
        parseArgs :: Args -> [String] -> Args
        parseArgs args argStrs = case argStrs of
            []     -> args
            ["-n"] -> args { optimise    = False }
            ["-v"] -> args { verbose     = True }
            ["-a"] -> args { astOnly     = True }
            ["-m"] -> args { modulesOnly = True }
            ["-l"] -> args { lexOnly     = True }
            [str]  -> args { filenames   = (filenames args) ++ [str] }
            (a:as) -> parseArgs (parseArgs args [a]) as


parse :: String -> String -> Either Error S.AST
parse filename source =
    case L.alexScanner filename source of
        Left  errStr -> Left (ErrorStr errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseFail pos -> Left (ErrorFile pos "parse error")
            P.ParseOk ast   -> Right ast 


--runFile :: Session -> String -> Bool -> IO ()
--runFile session source verbose = do
--    res <- compile session C.initCmpState R.initResolverState source False
--    case res of
--        Left err -> printError err source
--        Right (defs, cmpState', resolverState') -> jitAndRun defs session False verbose


--repl :: Session -> Bool -> IO ()
--repl session verbose =
--    runInputT defaultSettings (loop C.initCmpState R.initResolverState) 
--    where
--        loop :: C.MyCmpState -> R.ResolverState -> InputT IO ()
--        loop state resolverState = do
--            getInputLine "% " >>= \minput -> case minput of
--                Nothing    -> return ()
--                Just "q"   -> return ()
--                Just ""    -> loop state resolverState
--                Just input -> do
--                    when verbose $ liftIO (putStrLn "compiling...")
--                    res <- liftIO (compile session state resolverState input verbose)
--                    case res of
--                        Left err             -> do
--                            liftIO (printError err input)
--                            loop state resolverState
--                        Right (defs, state', resolverState') -> do
--                            let keepModule = not $ Set.null (C.exported state')
--                            liftIO (jitAndRun defs session keepModule verbose)
--                            let stateReset = state' { C.exported = Set.empty, C.declared = Set.empty }
--                            loop stateReset resolverState'
--
--
--compile
--    :: Session
--    -> C.MyCmpState
--    -> R.ResolverState
--    -> String
--    -> Bool
--    -> IO (Either Error ([Definition], C.MyCmpState, R.ResolverState))
--compile session state resolverState source verbose =
--    case parse source of
--        Left err  -> return (Left err)
--        Right ast -> do
--            res <- R.resolveAST resolverState ast
--            case res of
--                Left err -> return (Left err)
--                Right (ast', resolverState') -> do
--                    withFFIDataLayout (JIT.dataLayout session) $ \dl -> do
--                        cmpRes <- C.compile (context session) dl state ast'
--                        case cmpRes of
--                            Left err             -> return (Left err)
--                            Right (defs, state') -> return $ Right (defs, state', resolverState')


--                        let astmod = defaultModule { moduleDefinitions = defs }
--                        M.withModuleFromAST (context session) astmod $ \mod -> do
--                            when optimise $ do
--                                passRes <- runPassManager (fromJust $ passManager session) mod
--                                when verbose $ putStrLn ("optimisation pass: " ++ if passRes then "success" else "fail")
--                            when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
--                            M.writeLLVMAssemblyToFile (M.File $ filename ++ ".ll") mod

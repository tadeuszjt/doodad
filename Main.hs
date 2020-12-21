{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import qualified Data.ByteString.Char8    as BS
import qualified Data.Set                 as Set
import qualified Data.Map                 as Map
import           Data.Maybe

import           LLVM.AST
import           LLVM.AST.DataLayout
import           LLVM.Context
import qualified LLVM.Module              as M
import           LLVM.PassManager
import           Foreign.Ptr
import           LLVM.Internal.DataLayout
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.CodeGenOpt          as CodeGenOpt
import qualified LLVM.CodeModel           as CodeModel
import qualified LLVM.Relocation          as Reloc
import           LLVM.Target

import qualified CmpAST                   as C
import qualified Value                 as C
import qualified CmpMonad                 as C
import qualified Lexer                    as L
import qualified Parser                   as P
import qualified Resolver                 as R
import           JIT
import Error
import qualified AST as S
import qualified SymTab

data Args = Args
    { verbose   :: Bool
    , optimise  :: Bool
    , astOnly   :: Bool
    , filenames :: [String]
    }
initArgs = Args
    { verbose   = False
    , optimise  = True
    , astOnly   = False
    , filenames = []
    }


main :: IO ()
main = do
    args <- fmap (parseArgs initArgs) getArgs
    withSession (optimise args) $ \session ->
        if (filenames args) == [] then
            repl session (verbose args)
        else do
            let filename = head (filenames args)
            withFile filename ReadMode $ \h -> do
                source <- hGetContents h
                if (astOnly args) then
                    case parse source of
                        Left err  -> printError err source
                        Right ast -> S.prettyAST ast
                else do
                    putStrLn ("running \"" ++ filename ++ "\" ...")
                    runFile session source
    where
        parseArgs :: Args -> [String] -> Args
        parseArgs args argStrs = case argStrs of
            []     -> args
            ["-n"] -> args { optimise  = False }
            ["-v"] -> args { verbose   = True }
            ["-a"] -> args { astOnly   = True }
            [str]  -> args { filenames = (filenames args) ++ [str] }
            (a:as) -> parseArgs (parseArgs args [a]) as


parse :: String -> Either CmpError S.AST
parse source =
    case L.alexScanner source of
        Left  errStr -> Left $ CmpError (TextPos 0 0 0, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseFail pos -> Left $ CmpError (pos, "parse error")
            P.ParseOk ast   -> Right ast 


runFile :: Session -> String -> IO ()
runFile session source = do
    res <- compile session C.initCmpState R.initResolverState source False
    case res of
        Left err -> printError err source
        Right (defs, cmpState', resolverState') -> jitAndRun defs session False False


repl :: Session -> Bool -> IO ()
repl session verbose =
    runInputT defaultSettings (loop C.initCmpState R.initResolverState) 
    where
        loop :: C.MyCmpState -> R.ResolverState -> InputT IO ()
        loop state resolverState = do
            getInputLine "% " >>= \minput -> case minput of
                Nothing    -> return ()
                Just "q"   -> return ()
                Just ""    -> loop state resolverState
                Just input -> do
                    when verbose $ liftIO (putStrLn "compiling...")
                    res <- liftIO (compile session state resolverState input verbose)
                    case res of
                        Left err             -> do
                            liftIO (printError err input)
                            loop state resolverState
                        Right (defs, state', resolverState') -> do
                            let keepModule = not $ Set.null (C.exported state')
                            liftIO (jitAndRun defs session keepModule verbose)
                            let stateReset = state' { C.exported = Set.empty, C.declared = Set.empty }
                            loop stateReset resolverState'


compile
    :: Session
    -> C.MyCmpState
    -> R.ResolverState
    -> String
    -> Bool
    -> IO (Either CmpError ([Definition], C.MyCmpState, R.ResolverState))
compile session state resolverState source verbose =
    case parse source of
        Left err  -> return (Left err)
        Right ast -> do
            res <- R.resolveAST resolverState ast
            case res of
                Left err -> return (Left err)
                Right (ast', resolverState') -> do
                    withFFIDataLayout (JIT.dataLayout session) $ \dl -> do
                        cmpRes <- C.compile (context session) dl state ast'
                        case cmpRes of
                            Left err             -> return (Left err)
                            Right (defs, state') -> return $ Right (defs, state', resolverState')


--                        let astmod = defaultModule { moduleDefinitions = defs }
--                        M.withModuleFromAST (context session) astmod $ \mod -> do
--                            when optimise $ do
--                                passRes <- runPassManager (fromJust $ passManager session) mod
--                                when verbose $ putStrLn ("optimisation pass: " ++ if passRes then "success" else "fail")
--                            when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
--                            M.writeLLVMAssemblyToFile (M.File $ filename ++ ".ll") mod

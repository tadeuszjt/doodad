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


main :: IO ()
main = do
    args <- getArgs
    let verbose = "-v" `elem` args
    let optimise = not ("-n" `elem` args)
    let hasFile  = length args > 0 && head (head args) /= '-'

    if not hasFile then
            withSession optimise $ \session ->
                repl session verbose
    else do
        let filename = head args
        withFile filename ReadMode $ \h ->
            withSession optimise $ \session -> do
                content <- hGetContents h
                putStrLn "compiling..."
                res <- compile session C.initCmpState R.initResolverState content verbose
                case res of
                    Left err -> printError err content
                    Right (defs, state, resolverState) -> do
                        let astmod = defaultModule { moduleDefinitions = defs }
                        M.withModuleFromAST (context session) astmod $ \mod -> do
                            when optimise $ do
                                passRes <- runPassManager (fromJust $ passManager session) mod
                                when verbose $ putStrLn ("optimisation pass: " ++ if passRes then "success" else "fail")
                            when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
                            M.writeLLVMAssemblyToFile (M.File $ filename ++ ".ll") mod


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
                    liftIO (putStrLn "compiling...")
                    res <- liftIO (compile session state resolverState input verbose)
                    case res of
                        Left err             -> do
                            liftIO (printError err input)
                            liftIO (C.prettySymTab state)
                            loop state resolverState
                        Right (defs, state', resolverState') -> do
                            let keepModule = not $ Set.null (C.exported state')
                            liftIO (jitAndRun defs session keepModule verbose)
                            let stateReset = state' { C.exported = Set.empty , C.declared = Set.empty }
                            loop stateReset resolverState'


compile
    :: Session
    -> C.MyCmpState
    -> R.ResolverState
    -> String
    -> Bool
    -> IO (Either CmpError ([Definition], C.MyCmpState, R.ResolverState))
compile session state resolverState source verbose =
    case L.alexScanner source of
        Left  errStr -> return $ Left $ CmpError (TextPos 1 1 1, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseOk ast -> do
                res <- R.resolveAST resolverState ast
                case res of
                    Left err -> printError err source >> return (Left err)
                    Right (ast', resolverState') -> do
                        let exprs = R.expressions resolverState'
                        withFFIDataLayout (JIT.dataLayout session) $ \dl -> do
                            cmpRes <- C.compile (context session) dl state exprs ast'
                            case cmpRes of
                                Left err             -> return (Left err)
                                Right (defs, state') -> return $ Right (defs, state', resolverState')


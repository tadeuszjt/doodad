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
import qualified TypeChecker              as Y
import           JIT
import Error
import qualified AST as S


main :: IO ()
main = do
    args <- getArgs
    let verbose = "-v" `elem` args
    let optimise = not ("-n" `elem` args)
    let hasFile  = length args > 0 && head (head args) /= '-'
    let onlyIR   = "-i" `elem` args

    if not hasFile then
            withSession optimise $ \session ->
                withFFIDataLayout (JIT.dataLayout session) $ \dl -> 
                    repl dl session verbose
    else do
        let filename = head args
        withFile filename ReadMode $ \h ->
            withContext $ \ctx ->
                withPassManager defaultCuratedPassSetSpec $ \pm ->
                    withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.None $ \tm -> do
                        dl <- getTargetMachineDataLayout tm
                        withFFIDataLayout dl $ \pdl -> do
                            content <- hGetContents h
                            putStrLn "compiling..."
                            res <- compile ctx pdl C.initCmpState content verbose onlyIR
                            case res of
                                Left err -> printError err content
                                Right (defs, state) -> do
                                    let astmod = defaultModule { moduleDefinitions = defs }
                                    M.withModuleFromAST ctx astmod $ \mod -> do
                                        when optimise $ do
                                            passRes <- runPassManager pm mod
                                            when verbose $ putStrLn ("optimisation pass: " ++ if passRes then "success" else "fail")
                                        when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
                                        M.writeLLVMAssemblyToFile (M.File $ filename ++ ".ll") mod


repl :: Ptr FFI.DataLayout -> Session -> Bool -> IO ()
repl dl session verbose = runInputT defaultSettings (loop C.initCmpState) 
    where
        loop :: C.MyCmpState -> InputT IO ()
        loop state = do
            getInputLine "% " >>= \minput -> case minput of
                Nothing    -> return ()
                Just "q"   -> return ()
                Just ""    -> loop state
                Just input -> do
                    liftIO (putStrLn "compiling...")
                    res <- liftIO $ compile (JIT.context session) dl state input verbose False
                    case res of
                        Left err             -> do
                            liftIO $ printError err input 
                            liftIO $ C.prettySymTab $ state
                            loop state
                        Right (defs, state') -> do
                            let keepModule = not $ Set.null (C.exported state')
                            liftIO (jitAndRun defs session keepModule verbose)
                            loop state'
                                { C.exported = Set.empty
                                , C.declared = Set.empty
                                }


compile :: Context -> Ptr FFI.DataLayout -> C.MyCmpState -> String -> Bool -> Bool -> IO (Either CmpError ([Definition], C.MyCmpState))
compile ctx dl state source verbose onlyIR =
    case L.alexScanner source of
        Left  errStr -> return $ Left $ CmpError (TextPos 0 0 0, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseOk ast -> do
                res <- R.resolveAST ast
                case res of
                    Left err -> printError err source >> return (Left err)
                    Right (ast', exprs) -> do
                        mapM_ (\(i, e) -> putStrLn $ show i ++ ": " ++ show e) (Map.toList exprs)
                        Y.prettyFlatAST (Y.flattenAST ast')
                        putStrLn ""
                        --mapM_ (\(i, t) -> putStrLn $ show i ++ ": " ++ show t) $ Map.toList (Y.typeCheck exprs)
                        C.compile ctx dl state exprs ast'


{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module JIT where

import           Control.Monad.Except     hiding (void)
import           Control.Monad
import qualified Data.ByteString.Char8    as BS
import           Data.IORef
import           Data.Maybe
import           System.IO
import           Foreign.Ptr

import qualified LLVM.CodeGenOpt          as CodeGenOpt
import qualified LLVM.CodeModel           as CodeModel
import qualified LLVM.Relocation          as Reloc
import           LLVM.Target
import           LLVM.Linking
import           LLVM.AST
import           LLVM.AST.DataLayout
import           LLVM.Context
import qualified LLVM.Module              as M
import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer
import           LLVM.PassManager
import           LLVM.Internal.ObjectFile
import           Foreign.Ptr
import qualified LLVM.Internal.FFI.DataLayout   as FFI
import           LLVM.Internal.DataLayout


foreign import ccall "dynamic" mkFun :: FunPtr (IO ()) -> (IO ())

run :: FunPtr a -> IO ()
run fn = mkFun (castFunPtr fn :: FunPtr (IO ()))


data Session
    = Session
        { context          :: Context
        , executionSession :: ExecutionSession
        , compileLayer     :: IRCompileLayer ObjectLinkingLayer
        , passManager      :: Maybe PassManager
        , linkingLayer     :: ObjectLinkingLayer
        , targetMachine    :: TargetMachine
        }


withMyHostTargetMachine :: (TargetMachine -> IO ()) -> IO ()
withMyHostTargetMachine f = do
    initializeNativeTarget
    triple <- getProcessTargetTriple
    cpu <- getHostCPUName
    features <- getHostCPUFeatures
    (target, _) <- lookupTarget Nothing triple
    withTargetOptions $ \options ->
        withTargetMachine target triple cpu features options reloc model genOpt f
    where
        reloc  = Reloc.PIC
        model  = CodeModel.Default
        genOpt = CodeGenOpt.None


withSession :: Bool -> (Session -> IO ()) -> IO ()
withSession optimise f = do
    resolvers <- newIORef []
    withContext $ \ctx ->
        withExecutionSession $ \es ->
            withMyHostTargetMachine $ \tm -> do
                withObjectLinkingLayer es (\_ -> head <$> readIORef resolvers) $ \oll ->
                    withIRCompileLayer oll tm $ \cl ->
                        withPassManager defaultCuratedPassSetSpec $ \pm ->
                            withSymbolResolver es (myResolver cl) $ \psr -> do
                                writeIORef resolvers [psr]
                                loadLibraryPermanently Nothing
                                f $ Session ctx es cl (if optimise then Just pm else Nothing) oll tm
    where
        myResolver :: IRCompileLayer ObjectLinkingLayer -> SymbolResolver
        myResolver cl = SymbolResolver $ \mangled -> do
            symbol <- findSymbol cl mangled False
            case symbol of
                Right _ -> return symbol
                Left _  -> do
                    ptr <- getSymbolAddressInProcess mangled
                    return $ Right $ JITSymbol
                        { jitSymbolAddress = ptr
                        , jitSymbolFlags   = defaultJITSymbolFlags { jitSymbolExported = True }
                        }


jitAddObjectFile :: Session -> FilePath -> IO ModuleKey
jitAddObjectFile session filepath = do
    withObjectFile filepath $ \objFile -> do
        withModuleKey (executionSession session) $ \modKey -> do
            addObjectFile (linkingLayer session) modKey objFile
            return modKey


jitCompileToObject :: Bool -> FilePath -> [Definition] -> Session -> IO ()
jitCompileToObject verbose file defs session = do
    let astmod = defaultModule { moduleDefinitions = defs }

    withModuleKey (executionSession session) $ \modKey ->
        M.withModuleFromAST (context session) astmod $ \mod -> do
            let pm = passManager session
            when (isJust pm) $ do
                when verbose (putStrLn "running optimisation passes...")
                void $ runPassManager (fromJust pm) mod

            when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
            M.writeObjectToFile (JIT.targetMachine session) (M.File file) mod
    


jitAndRun :: [Definition] -> Session -> Bool -> Bool -> IO ()
jitAndRun defs session keepModule verbose = do
    let astmod = defaultModule { moduleDefinitions = defs }

    withModuleKey (executionSession session) $ \modKey ->
        M.withModuleFromAST (context session) astmod $ \mod -> do
            let pm = passManager session
            let cl = compileLayer session

            when (isJust pm) $ do
                when verbose (putStrLn "running optimisation passes...")
                void $ runPassManager (fromJust pm) mod

            when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
            addModule cl modKey mod

            mangled <- mangleSymbol cl "main"
            res <- findSymbolIn cl modKey mangled False
            case res of
                Left _                -> when verbose (putStrLn "no main")
                Right (JITSymbol fn _)-> do
                    when verbose (putStrLn "running main...")
                    run $ castPtrToFunPtr (wordPtrToPtr fn)

            unless keepModule $ do
                when verbose (putStrLn "removing module...")
                (removeModule cl modKey)

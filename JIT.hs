{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module JIT where

import           Control.Monad.Except     hiding (void)
import           Control.Monad
import qualified Data.ByteString.Char8    as BS
import           Data.IORef
import           Data.Maybe
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


foreign import ccall "dynamic" mkFun :: FunPtr (IO ()) -> (IO ())

run :: FunPtr a -> IO ()
run fn = mkFun (castFunPtr fn :: FunPtr (IO ()))


data Session
    = Session
        { context          :: Context
        , executionSession :: ExecutionSession
        , compileLayer     :: IRCompileLayer ObjectLinkingLayer
        , passManager      :: Maybe PassManager
        , dataLayout       :: DataLayout
        }

withSession :: Bool -> (Session -> IO ()) -> IO ()
withSession optimise f = do
    resolvers <- newIORef []
    withContext $ \ctx ->
        withExecutionSession $ \es ->
            withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.None $ \tm ->
                withObjectLinkingLayer es (\_ -> fmap head $ readIORef resolvers) $ \oll ->
                    withIRCompileLayer oll tm $ \cl ->
                        withPassManager defaultCuratedPassSetSpec $ \pm ->
                            withSymbolResolver es (myResolver cl) $ \psr -> do
                                writeIORef resolvers [psr]
                                loadLibraryPermanently Nothing
                                dl <- getTargetMachineDataLayout tm 
                                f $ Session ctx es cl (if optimise then Just pm else Nothing) dl


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


jitAndRun :: [Definition] -> Session -> Bool -> Bool -> IO ()
jitAndRun defs session keepModule verbose = do
    let astmod = defaultModule { moduleDefinitions = defs }

    withModuleKey (executionSession session) $ \modKey ->
        M.withModuleFromAST (context session) astmod $ \mod -> do
            let pm = passManager session
            let cl = compileLayer session
            unless (isNothing pm) $
                void $ runPassManager (fromJust pm) mod
            when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)

            addModule cl modKey mod
            mangled <- mangleSymbol cl "main"
            res <- findSymbolIn cl modKey mangled False
            case res of
                Left _                -> return ()
                Right (JITSymbol fn _)-> run $ castPtrToFunPtr (wordPtrToPtr fn)
            unless keepModule (removeModule cl modKey)

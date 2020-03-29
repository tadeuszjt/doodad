module Main where

import GHC.Ptr
import Data.Word
import Foreign.Ptr
import Foreign.C.String
import System.Console.Haskeline
import Control.Monad.Except hiding (void)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import qualified Lexer    as L
import qualified Parser   as P
import qualified Compiler as C

import LLVM.AST
import LLVM.Context
import LLVM.Target
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.Internal.OrcJIT
import LLVM.Linking
import LLVM.AST.Global
import LLVM.AST.Type
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel 
import qualified LLVM.CodeGenOpt as CodeGenOpt 
import qualified LLVM.Module as M
import qualified LLVM.Internal.FFI.OrcJIT as FFI

--foreign import ccall "dynamic" haskFun :: FunPtr (IO ()) -> (IO ())

--run :: FunPtr a -> IO ()
--run fn = haskFun (castFunPtr fn :: FunPtr (IO ()))

mkBSS = BSS.toShort . BS.pack


main :: IO ()
main =
	withContext $ \ctx ->
	withExecutionSession $ \es ->
	withMyTargetMachine $ \tm ->
	withSymbolResolver es myResolver $ \psr ->
	withObjectLinkingLayer es (\_ -> return psr) $ \oll ->
	withIRCompileLayer oll tm $ \ircl -> do
		loadLibraryPermanently Nothing
		repl ctx es tm ircl

	where
		withMyTargetMachine :: (TargetMachine -> IO a) -> IO a
		withMyTargetMachine f = do
			initializeNativeTarget
			triple      <- getProcessTargetTriple
			cpu         <- getHostCPUName
			features    <- getHostCPUFeatures
			(target, _) <- lookupTarget Nothing triple
			let rm = Reloc.Default
			let cm = CodeModel.Default
			let go = CodeGenOpt.None
			withTargetOptions $ \options ->
				withTargetMachine target triple cpu features options rm cm go f

		myResolver :: SymbolResolver
		myResolver = SymbolResolver $ \mangled -> do
			ptr <- getSymbolAddressInProcess mangled
			return $ Right $ JITSymbol
				{ jitSymbolAddress = ptr 
				, jitSymbolFlags   = defaultJITSymbolFlags { jitSymbolExported = True }
				}


repl :: Context -> ExecutionSession -> TargetMachine -> IRCompileLayer ObjectLinkingLayer ->  IO ()
repl ctx es tm cl = runInputT defaultSettings (loop C.initCmpState)
	where
		loop :: C.CmpState -> InputT IO ()
		loop state = do
			minput <- getInputLine "% "
			case minput of
				Nothing    -> return ()
				Just "q"   -> return ()
				Just input -> liftIO (process state input) >>= loop

		process :: C.CmpState -> String -> IO C.CmpState
		process state source = do
			case L.alexScanner source of
				Left  errStr -> putStrLn errStr >> return state
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast -> do
						let (res, state') = C.codeGen state (head ast)
						case res of
							Left err  -> print err >> return state
							Right def -> runDefinition state def >> return state'

		runDefinition :: C.CmpState -> Definition -> IO ()
		runDefinition state def = do
			let astmod = defaultModule { moduleDefinitions = (C.externs state) ++ [def] }
			M.withModuleFromAST ctx astmod $ \mod -> do
				BS.putStrLn =<< M.moduleLLVMAssembly mod
				withModuleKey es $ \modKey ->
					case def of
						GlobalDefinition (GlobalVariable _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
							addModule cl modKey mod
						GlobalDefinition (Function _ _ _ _ _ _ fnName _ _ _ _ _ _ _ _ _ _) -> do
							withModule cl modKey mod $ do
								mangled <- let Name fnStr = fnName in mangleSymbol cl fnStr
								_ <- findSymbol cl mangled False -- silly hack to make findSymbol work
								res <- findSymbol cl mangled False
								print (res, mangled)
								return ()


{-#LANGUAGE ForeignFunctionInterface#-}
module Main where

import Data.Int
import Foreign.Ptr
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
import LLVM.Linking
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import qualified LLVM.Module     as M
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel  as CodeModel 
import qualified LLVM.CodeGenOpt as CodeGenOpt 

foreign import ccall "dynamic" mkFun :: FunPtr (IO Int32) -> (IO Int32)


run :: FunPtr a -> IO Int32
run fn = mkFun (castFunPtr fn :: FunPtr (IO Int32))


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
			let rm = Reloc.PIC
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
		loop state =
			getInputLine "% " >>= \minput -> case minput of
				Nothing    -> return ()
				Just "q"   -> return ()
				Just input -> liftIO (process state input) >>= loop

		process :: C.CmpState -> String -> IO C.CmpState
		process state source =
			case L.alexScanner source of
				Left  errStr -> putStrLn errStr >> return state
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast ->
						let (res, state') = C.codeGen state (head ast) in
						case res of
							Left err          -> putStrLn (show err) >> return state
							Right (defs, def) -> runDefinition state (defs, def) >> return state'

		runDefinition :: C.CmpState -> ([Definition], Definition) -> IO ()
		runDefinition state (defs, def) = do
			let astmod = defaultModule
				{ moduleDefinitions = (C.externs state) ++ defs ++ [def]
				, moduleName        = mkBSS "bolang"
				}
			M.withModuleFromAST ctx astmod $ \mod -> do
				BS.putStrLn =<< M.moduleLLVMAssembly mod
				withModuleKey es $ \modKey ->
					case def of
						GlobalDefinition (GlobalVariable _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
							addModule cl modKey mod
						GlobalDefinition (Function _ _ _ _ _ _ (Name fnName) _ _ _ _ _ _ _ _ _ _) ->
							withModule cl modKey mod $ do
								res <- (\mangled -> findSymbol cl mangled False) =<< mangleSymbol cl fnName
								case res of
									Left _ -> putStrLn ("Couldn't find: " ++ show fnName)
									Right (JITSymbol fn _)-> do
										i <- run $ castPtrToFunPtr (wordPtrToPtr fn)
										putStrLn ("Result: " ++ show i)


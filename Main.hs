module Main where

import GHC.Ptr
import Data.Word
import Foreign.Ptr
import Foreign.C.String
import System.Console.Haskeline
import Control.Monad.Except
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
	withIRCompileLayer oll tm $ \ircl -> 
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
							Left err -> print err >> return state
							Right def -> do
								let astmod = defaultModule
									{ moduleName        = mkBSS "bolang"
									, moduleDefinitions = (C.externs state) ++ [def]
									}
								M.withModuleFromAST ctx astmod $ \mod -> do
									modKey <- allocateModuleKey es
									case def of
										GlobalDefinition (GlobalVariable _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> 
											addModule cl modKey mod
										GlobalDefinition (Function _ _ _ _ _ _ fnName _ _ _ _ _ _ _ _ _ _) ->
											withModule cl modKey mod $ do
												let Name fnNameStr = fnName
												mangled <- mangleSymbol cl fnNameStr
												res <- findSymbolIn cl modKey mangled False
												print astmod 
												return ()
								return state'


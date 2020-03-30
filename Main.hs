{-#LANGUAGE ForeignFunctionInterface#-}
module Main where

import Foreign.Ptr
import System.IO
import System.Console.Haskeline
import Control.Monad.Except hiding (void)
import qualified Data.Map              as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import qualified Lexer    as L
import qualified Parser   as P
import qualified Compiler as C

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.Context
import LLVM.Target
import LLVM.Linking
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import qualified LLVM.Module     as M
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel  as CodeModel 
import qualified LLVM.CodeGenOpt as CodeGenOpt 

foreign import ccall "dynamic" mkFun :: FunPtr (IO ()) -> (IO ())


run :: FunPtr a -> IO ()
run fn = mkFun (castFunPtr fn :: FunPtr (IO ()))


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
							Left err -> putStrLn (show err) >> return state
							Right () -> runDefinition state' >> return state'
								{ C.globals      = Map.empty
								, C.instructions = []
								}

		runDefinition :: C.CmpState -> IO ()
		runDefinition state = do
			let globals = Map.elems (C.globals state)
			let externs = Map.elems (C.externs state)
			let instructions = reverse (C.instructions state)

			let mainName = mkBSS "main.0"
			let mainFn = GlobalDefinition $ functionDefaults
				{ returnType  = void
				, name        = Name mainName
				, basicBlocks = [BasicBlock (mkName "entry") instructions (Do $ Ret Nothing [])]
				}

			case instructions of
				[] -> do
					let astmod = defaultModule
						{ moduleDefinitions = externs ++ globals 
						}
					M.withModuleFromAST ctx astmod $ \mod -> do
						BS.putStrLn =<< M.moduleLLVMAssembly mod
						withModuleKey es $ \modKey ->
							addModule cl modKey mod
				x -> do
					let astmod = defaultModule
						{ moduleDefinitions = externs ++ globals ++ [mainFn]
						}
					M.withModuleFromAST ctx astmod $ \mod -> do
						BS.putStrLn =<< M.moduleLLVMAssembly mod
						withModuleKey es $ \modKey ->
							withModule cl modKey mod $ do
								res <- (\mangled -> findSymbol cl mangled False) =<< mangleSymbol cl mainName
								case res of
									Left _ -> putStrLn ("Couldn't find: " ++ show mainName)
									Right (JITSymbol fn _)-> do
										run $ castPtrToFunPtr (wordPtrToPtr fn)


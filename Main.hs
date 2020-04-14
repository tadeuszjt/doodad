{-#LANGUAGE ForeignFunctionInterface#-}
module Main where

import Foreign.Ptr
import System.IO
import System.Console.Haskeline
import Control.Monad.Except hiding (void)
import Data.IORef
import qualified Data.Map              as Map
import qualified Data.Set              as Set
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
main = do
	resolvers <- newIORef []
	withContext $ \ctx ->
		withExecutionSession $ \es ->
			withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.None $ \tm ->
				withObjectLinkingLayer es (\_ -> fmap head $ readIORef resolvers) $ \oll ->
					withIRCompileLayer oll tm $ \cl ->
						withSymbolResolver es (myResolver cl) $ \psr -> do
							writeIORef resolvers [psr]
							loadLibraryPermanently Nothing
							repl ctx es cl

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


repl :: Context -> ExecutionSession -> IRCompileLayer ObjectLinkingLayer ->  IO ()
repl ctx es cl = runInputT defaultSettings (loop C.initCmpState)
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
								{ C.declared     = Set.empty
								, C.globals      = []
								, C.instructions = []
								}

		runDefinition :: C.CmpState -> IO ()
		runDefinition state = do
			let globals = reverse (C.globals state)
			let instructions = reverse (C.instructions state)

			case instructions of
				[] -> do
					let astmod = defaultModule { moduleDefinitions = globals }
					M.withModuleFromAST ctx astmod $ \mod -> do
						BS.putStrLn =<< M.moduleLLVMAssembly mod
						withModuleKey es $ \modKey ->
							addModule cl modKey mod
				x -> do
					let mainName = mkBSS "main.0"
					let mainFn = GlobalDefinition $ functionDefaults
						{ returnType  = void
						, name        = Name mainName
						, basicBlocks = [BasicBlock (mkName "entry") instructions (Do $ Ret Nothing [])]
						}
					let astmod = defaultModule { moduleDefinitions = globals ++ [mainFn] }
					M.withModuleFromAST ctx astmod $ \mod -> do
						BS.putStrLn =<< M.moduleLLVMAssembly mod
						withModuleKey es $ \modKey ->
							withModule cl modKey mod $
								mangleSymbol cl mainName >>= \mangled -> findSymbol cl mangled False >>= \res -> case res of
									Left _                -> putStrLn "linkage error"
									Right (JITSymbol fn _)-> run $ castPtrToFunPtr (wordPtrToPtr fn)


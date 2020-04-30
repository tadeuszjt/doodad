{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import           Control.Monad
import           Control.Monad.Except     hiding (void)
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Short    as BSS
import           Data.IORef
import qualified Data.Set                   as Set
import           Foreign.Ptr
import           System.Console.Haskeline
import           System.Environment
import           System.IO

import           LLVM.AST
import           LLVM.AST.Global
import qualified LLVM.CodeGenOpt          as CodeGenOpt
import qualified LLVM.CodeModel           as CodeModel
import           LLVM.Context
import           LLVM.Linking
import qualified LLVM.Module              as M
import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer
import           LLVM.PassManager
import qualified LLVM.Relocation          as Reloc
import           LLVM.Target

import qualified Compiler                 as C
import qualified CmpBuilder               as C
import qualified Cmp                      as C
import qualified Lexer                    as L
import qualified Parser                   as P

foreign import ccall "dynamic" mkFun :: FunPtr (IO ()) -> (IO ())


run :: FunPtr a -> IO ()
run fn = mkFun (castFunPtr fn :: FunPtr (IO ()))


mkBSS = BSS.toShort . BS.pack


main :: IO ()
main = do
	args <- getArgs
	let verbose = "-v" `elem` args
	let dontOptimise = "-n" `elem` args
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

								if length args > 0 && head (head args) /= '-' then
									withFile (head args) ReadMode $ \h -> do
										content <- hGetContents h
										void (compile C.initCmpState content ctx es cl pm verbose dontOptimise)
								else
									repl ctx es cl pm verbose dontOptimise

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


repl :: Context -> ExecutionSession -> IRCompileLayer ObjectLinkingLayer -> PassManager -> Bool -> Bool -> IO ()
repl ctx es cl pm verbose dontOptimise = runInputT defaultSettings (loop C.initCmpState)
	where
		loop :: C.State -> InputT IO ()
		loop state =
			getInputLine "% " >>= \minput -> case minput of
				Nothing    -> return ()
				Just "q"   -> return ()
				Just ""    -> loop state
				Just input -> liftIO (compile state input ctx es cl pm verbose dontOptimise) >>= loop


compile
	:: C.State
	-> String
	-> Context
	-> ExecutionSession
	-> IRCompileLayer ObjectLinkingLayer
	-> PassManager
	-> Bool
	-> Bool
	-> IO C.State 
compile state source ctx es cl pm verbose dontOptimise =
	case L.alexScanner source of
		Left  errStr -> putStrLn errStr >> return state
		Right tokens -> case (P.parseTokens tokens) 0 of
			P.ParseOk ast -> case C.compile state ast of
				Left err             -> printError err source >> return state
				Right (defs, state') -> jitAndRun defs state'
	where
		jitAndRun :: [Definition] -> C.CmpState C.ValType -> IO (C.CmpState C.ValType)
		jitAndRun defs state = do
			let exported = C.exported state
			let astmod   = defaultModule { moduleDefinitions = defs }
			withModuleKey es $ \modKey ->
				M.withModuleFromAST ctx astmod $ \mod -> do
					unless dontOptimise $ do
						passRes <- runPassManager pm mod
						when verbose $ putStrLn ("optimisation pass: " ++ show passRes)
					when verbose $ BS.putStrLn =<< M.moduleLLVMAssembly mod
					addModule cl modKey mod
					mangled <- mangleSymbol cl (mkBSS "main")
					res <- findSymbolIn cl modKey mangled False
					case res of
						Left _                -> putStrLn "linkage error"
						Right (JITSymbol fn _)-> run $ castPtrToFunPtr (wordPtrToPtr fn)
					when (Set.null exported) (removeModule cl modKey)
					return state


		printError :: C.CmpError -> String -> IO ()
		printError (C.CmpError (C.TextPos p l c, str)) source = do
			putStrLn ("error " ++ show l ++ ":" ++ show c ++ " " ++ str ++ ":")
			let sourceLines = lines source
			unless (length sourceLines <= 1) $ putStrLn ("\t" ++ sourceLines !! (l-2))
			unless (length sourceLines <= 0) $ putStrLn ("\t" ++ sourceLines !! (l-1))
			putStrLn ("\t" ++ replicate (c-1) '-' ++ "^")

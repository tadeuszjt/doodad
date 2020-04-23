{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import           Control.Monad.Except     hiding (void)
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Short    as BSS
import           Data.IORef
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Foreign.Ptr
import           System.Console.Haskeline
import           System.Environment
import           System.IO

import           LLVM.AST
import           LLVM.AST.Global
import           LLVM.AST.Type
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

import qualified CmpState                 as C
import qualified Compiler                 as C
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
								repl ctx es cl pm verbose

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


repl :: Context -> ExecutionSession -> IRCompileLayer ObjectLinkingLayer -> PassManager -> Bool -> IO ()
repl ctx es cl pm verbose = runInputT defaultSettings (loop C.initCmpState)
	where
		loop :: C.CmpState -> InputT IO ()
		loop state =
			getInputLine "% " >>= \minput -> case minput of
				Nothing    -> return ()
				Just "q"   -> return ()
				Just ""    -> loop state
				Just input -> liftIO (compile state input) >>= loop

		compile :: C.CmpState -> String -> IO C.CmpState
		compile state source =
			case L.alexScanner source of
				Left  errStr -> putStrLn errStr >> return state
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast -> do
						let ((res, defs), state') = C.codeGen state ast
						case res of
							Left err -> putStrLn (show err) >> return state
							Right () -> jitAndRun defs state'

		jitAndRun :: [Definition] -> C.CmpState -> IO C.CmpState
		jitAndRun defs state = do
			let mainName = mkBSS ".main"
			let exported = C.exported state
			let blocks   = reverse $ head (C.basicBlocks state)
			let mainFn   = C.funcDef (Name mainName) void [] False blocks
			let astmod   = defaultModule { moduleDefinitions = defs ++ [mainFn] }

			withModuleKey es $ \modKey ->
				M.withModuleFromAST ctx astmod $ \mod -> do
					passRes <- runPassManager pm mod
					when verbose $ putStrLn ("optimisation pass: " ++ show passRes)
					when verbose $ BS.putStrLn =<< M.moduleLLVMAssembly mod
					addModule cl modKey mod
					unless (blocks == [BasicBlock (mkName "entry") [] (Do $ Ret Nothing [])]) $ do
						mangled <- mangleSymbol cl mainName
						res <- findSymbolIn cl modKey mangled False
						case res of
							Left _                -> putStrLn "linkage error"
							Right (JITSymbol fn _)-> run $ castPtrToFunPtr (wordPtrToPtr fn)
					when (Set.null exported) (removeModule cl modKey)
					return state
						{ C.curRetType  = VoidType
						, C.declared    = Set.empty
						, C.exported    = Set.empty
						, C.basicBlocks = [[]]
						}

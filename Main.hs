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
import qualified CmpState as C

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
				Just input -> liftIO (compile state input) >>= loop

		compile :: C.CmpState -> String -> IO C.CmpState
		compile state source =
			case L.alexScanner source of
				Left  errStr -> putStrLn errStr >> return state
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast ->
						let (res, state') = C.codeGen state ast in
						case res of
							Left err -> putStrLn (show err) >> return state
							Right () -> jitAndRun state'

		jitAndRun :: C.CmpState -> IO C.CmpState
		jitAndRun state = do
			let globals = reverse (C.globals state)
			let exported = C.exported state
			let mainName = mkBSS "main.0"
			let blocks = reverse $ head (C.basicBlocks state)

			let mainFn = GlobalDefinition $ functionDefaults
				{ returnType  = void
				, name        = Name mainName
				, basicBlocks = blocks
				}
			let astmod = defaultModule { moduleDefinitions = globals ++ [mainFn] }

			withModuleKey es $ \modKey ->
				M.withModuleFromAST ctx astmod $ \mod -> do
					BS.putStrLn =<< M.moduleLLVMAssembly mod
					addModule cl modKey mod 
					unless (null blocks) $ do
						mangled <- mangleSymbol cl mainName
						res <- findSymbolIn cl modKey mangled False
						case res of
							Left _                -> putStrLn "linkage error"
							Right (JITSymbol fn _)-> run $ castPtrToFunPtr (wordPtrToPtr fn)
					when (Set.null exported) (removeModule cl modKey)
					return state
						{ C.declared    = Set.empty
						, C.exported    = Set.empty
						, C.globals     = []
						, C.basicBlocks = [[]]
						}

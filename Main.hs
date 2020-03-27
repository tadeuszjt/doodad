module Main where

import System.IO
import System.Environment
import System.Console.Haskeline
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import qualified Parser as P
import qualified Lexer  as L
--import qualified AST      as S
import qualified Compiler as C

import qualified LLVM.Module as LM
import LLVM.AST
import LLVM.Context


hPutLLVMModule :: Handle -> Module -> IO ()
hPutLLVMModule handle mod =
	withContext $ \ctx ->
		BS.hPutStrLn handle =<< LM.withModuleFromAST ctx mod LM.moduleLLVMAssembly


main :: IO ()
main = do
	mod <- runInputT defaultSettings (loop C.initCmpState)

	handle <- openFile "main.ll" WriteMode
	hPutLLVMModule handle mod
	hClose handle
	
	where
		loop :: C.CmpState -> InputT IO Module
		loop state = do
			minput <- getInputLine "% "
			case minput of
				Nothing    -> return (C.llvmModule state)
				Just "q"   -> return (C.llvmModule state)
				Just input -> liftIO (process state input) >>= loop


		process :: C.CmpState -> String -> IO C.CmpState
		process state source = do
			case L.alexScanner source of
				Left  errStr -> putStrLn errStr >> return state
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast -> do
						res <- C.codeGen state ast
						case res of
							Right newState -> hPutLLVMModule stdout (C.llvmModule newState) >> return newState
							Left e -> putStrLn (show e) >> return state
						

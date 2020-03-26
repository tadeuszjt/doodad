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


putLLVMModule :: Module -> IO ()
putLLVMModule mod =
	withContext $ \ctx ->
		BS.putStrLn =<< LM.withModuleFromAST ctx mod LM.moduleLLVMAssembly


initModule = defaultModule
	{ moduleName = BSS.toShort $ BS.pack "I just don't give a JIT"
	}


main :: IO ()
main = runInputT defaultSettings (loop initModule)
	where
		loop :: Module -> InputT IO ()
		loop mod = do
			minput <- getInputLine "% "
			case minput of
				Nothing    -> return ()
				Just "q"   -> return ()
				Just input -> liftIO (process mod input) >>= loop


		process :: Module -> String -> IO Module
		process mod source = do
			case L.alexScanner source of
				Left  errStr -> putStrLn errStr >> return mod
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast -> C.codeGen mod ast >> return mod

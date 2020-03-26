module Main where

import System.IO
import System.Environment
import System.Console.Haskeline
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS

import qualified Parser as P
import qualified Lexer  as L
--import qualified AST      as S
import qualified Compiler as C

import qualified LLVM.Module as LM
import LLVM.AST
import LLVM.Context


--putLLVMModule :: Module -> IO ()
--putLLVMModule mod =
--	withContext $ \ctx ->
--		BS.putStrLn =<< LM.withModuleFromAST ctx mod LM.moduleLLVMAssembly


main :: IO ()
main = runInputT defaultSettings loop
	where
		loop :: InputT IO ()
		loop = do
			minput <- getInputLine "% "
			case minput of
				Nothing    -> return ()
				Just "q"   -> return ()
				Just input -> process input >> loop

		process :: String -> InputT IO ()
		process input = do
			case L.alexScanner input of
				Left  errStr -> outputStrLn $ "Lexer error: " ++ errStr
				Right tokens -> case (P.parseTokens tokens) 0 of
					P.ParseOk ast -> outputStrLn $ show ast
			



--	[fileName] <- getArgs
--	handle <- openFile fileName ReadMode
--	content <- hGetContents handle
--
--	let tokens = L.alexScanTokens content
--	let ast = P.parseTokens tokens
--
--	case C.compileAST ast of
--		Left e -> print e
--		Right m -> putLLVMModule m
--

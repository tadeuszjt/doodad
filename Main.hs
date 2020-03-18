module Main where

import System.IO
import System.Environment
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS

import qualified Parser   as P
import qualified Lexer    as L
import qualified AST      as S
import qualified Compiler as C

import qualified LLVM.Module as LM
import LLVM.AST
import LLVM.Context


putLLVMModule :: Module -> IO ()
putLLVMModule mod =
	withContext $ \ctx ->
		BS.putStrLn =<< LM.withModuleFromAST ctx mod LM.moduleLLVMAssembly


main :: IO ()
main = do
	[fileName] <- getArgs
	handle <- openFile fileName ReadMode
	content <- hGetContents handle

	let tokens = L.alexScanTokens content
	let ast = P.parseTokens tokens

	case C.compileAST ast of
		Left e -> print e
		Right m -> putLLVMModule m


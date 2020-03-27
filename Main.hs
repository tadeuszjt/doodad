module Main where

import System.Console.Haskeline
import Control.Monad.Except

import qualified Lexer    as L
import qualified Parser   as P
import qualified Compiler as C


main :: IO ()
main = do
	runInputT defaultSettings (loop C.initCmpState)
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
						res <- C.codeGen state ast
						case res of
							Right newState -> return newState
							Left e -> putStrLn (show e) >> return state
						

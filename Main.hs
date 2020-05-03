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
import           JIT


main :: IO ()
main = do
    args <- getArgs
    let verbose = "-v" `elem` args
    let optimise = not ("-n" `elem` args)
    let hasFile  = length args > 0 && head (head args) /= '-'

    if hasFile then
        let filename = head args in
        withFile filename ReadMode $ \h ->
            withContext $ \ctx -> do
                content <- hGetContents h
                case compile C.initCmpState content verbose of
                    Left err -> printError err content
                    Right (defs, state) -> do
                        let astmod = defaultModule { moduleDefinitions = defs }
                        M.withModuleFromAST ctx astmod $ \mod ->
                            M.writeLLVMAssemblyToFile (M.File $ filename ++ ".ll") mod
                            
    else
        withSession optimise $ \session -> repl session verbose


repl :: Session -> Bool -> IO ()
repl session verbose = runInputT defaultSettings (loop C.initCmpState)
    where
        loop :: C.CmpState C.ValType -> InputT IO ()
        loop state =
            getInputLine "% " >>= \minput -> case minput of
                Nothing    -> return ()
                Just "q"   -> return ()
                Just ""    -> loop state
                Just input -> return ()--liftIO (compile state input session verbose) >>= loop


compile :: C.CmpState C.ValType -> String -> Bool -> Either C.CmpError ([Definition], C.CmpState C.ValType)
compile state source verbose =
    case L.alexScanner source of
        Left  errStr -> Left $ C.CmpError (C.TextPos{}, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseOk ast -> C.compile state ast


printError :: C.CmpError -> String -> IO ()
printError (C.CmpError (C.TextPos p l c, str)) source = do
    putStrLn ("error " ++ show l ++ ":" ++ show c ++ " " ++ str ++ ":")
    let sourceLines = lines source
    unless (length sourceLines <= 1) $ putStrLn (sourceLines !! (l-2))
    unless (length sourceLines <= 0) $ putStrLn (sourceLines !! (l-1))
    putStrLn (replicate (c-1) '-' ++ "^")

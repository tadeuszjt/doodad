{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import qualified Data.ByteString.Char8    as BS
import qualified Data.Set                 as Set
import qualified Data.Map                 as Map

import           LLVM.AST
import           LLVM.AST.DataLayout
import           LLVM.Context
import qualified LLVM.Module              as M
import           LLVM.PassManager
import           Foreign.Ptr
import           LLVM.Internal.DataLayout
import qualified LLVM.Internal.FFI.DataLayout as FFI

import qualified CmpAST                   as C
import qualified Value                 as C
import qualified CmpMonad                 as C
import qualified Lexer                    as L
import qualified Parser                   as P
import           JIT


main :: IO ()
main = do
    args <- getArgs
    let verbose = "-v" `elem` args
    let optimise = not ("-n" `elem` args)
    let hasFile  = length args > 0 && head (head args) /= '-'

    if not hasFile then
            withSession optimise $ \session ->
                withFFIDataLayout (JIT.dataLayout session) $ \dl -> 
                    repl dl session verbose
	else
        return ()
--        let filename = head args in
--        withFile filename ReadMode $ \h ->
--            withContext $ \ctx ->
--                withPassManager defaultCuratedPassSetSpec $ \pm -> do
--                    content <- hGetContents h
--                    case compile C.initCmpState content verbose of
--                        Left err -> printError err content
--                        Right (defs, state) -> do
--                            forM_ (head $ C.symTab state) $ \ent -> do
--                                forM_ ent $ \e ->
--                                    putStrLn (show e)
--                                putStrLn ""
--                            let astmod = defaultModule { moduleDefinitions = defs }
--                            M.withModuleFromAST ctx astmod $ \mod -> do
--                                when optimise $ do
--                                    passRes <- runPassManager pm mod
--                                    when verbose $ putStrLn ("optimisation pass: " ++ if passRes then "success" else "fail")
--                                when verbose (BS.putStrLn =<< M.moduleLLVMAssembly mod)
--                                M.writeLLVMAssemblyToFile (M.File $ filename ++ ".ll") mod


repl :: Ptr FFI.DataLayout -> Session -> Bool -> IO ()
repl dl session verbose = runInputT defaultSettings (loop C.initCmpState) 
    where
        loop :: C.MyCmpState -> InputT IO ()
        loop state = do
            getInputLine "% " >>= \minput -> case minput of
                Nothing    -> return ()
                Just "q"   -> return ()
                Just ""    -> loop state
                Just input -> do
                    liftIO (putStrLn "compiling...")
                    res <- liftIO $ compile (JIT.context session) dl state input verbose
                    case res of
                        Left err             -> do
                            liftIO $ printError err input 
                            liftIO $ C.prettySymTab $ state
                            loop state
                        Right (defs, state') -> do
                            let keepModule = not $ Set.null (C.exported state')
                            liftIO (jitAndRun defs session keepModule verbose)
                            loop state'
                                { C.exported = Set.empty
                                , C.declared = Set.empty
                                }


compile :: Context -> Ptr FFI.DataLayout -> C.MyCmpState -> String -> Bool -> IO (Either C.CmpError ([Definition], C.MyCmpState))
compile ctx dl state source verbose =
    case L.alexScanner source of
        Left  errStr -> return $ Left $ C.CmpError (C.TextPos 0 0 0, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseOk ast -> C.compile ctx dl state ast


printError :: C.CmpError -> String -> IO ()
printError (C.CmpError (C.TextPos p l c, str)) source = do
    putStrLn ("error " ++ show l ++ ":" ++ show c ++ " " ++ str ++ ":")
    let sourceLines = lines source
    unless (length sourceLines <= 1) $ putStrLn (sourceLines !! (l-2))
    unless (length sourceLines <= 0) $ putStrLn (sourceLines !! (l-1))
    putStrLn (replicate (c-1) '-' ++ "^")

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CmpFuncs where

import           Control.Monad
import           Data.Char
import           Data.List
import           Prelude                    hiding (EQ, and, or)

import           LLVM.AST 
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              hiding (void)
import           LLVM.AST.Typed
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Module
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Constant as C

import           CmpMonad


for :: MonadInstrCmp k o m => Operand -> (Operand -> m ()) -> m ()
for num f = do
    forCond <- freshName "for_cond"
    forBody <- freshName "for_body"
    forExit <- fresh

    i <- alloca i64 Nothing 0
    store i 0 (int64 0)
    br forCond

    emitBlockStart forCond
    li <- load i 0
    cnd <- icmp SLT li num 
    condBr cnd forBody forExit

    emitBlockStart forBody
    store i 0 =<< add li (int64 1)
    f li
    br forCond

    emitBlockStart forExit


if_ :: MonadInstrCmp k o m => Operand -> m () -> m () -> m ()
if_ cnd trueIns falseIns = do
    true  <- freshName "if_true"
    false <- freshName "if_false"
    exit  <- fresh

    condBr cnd true false
    emitBlockStart true
    trueIns
    br exit

    emitBlockStart false
    falseIns
    br exit

    emitBlockStart exit


switch_ :: MonadInstrCmp k o m => [(m Operand, m ())] -> m ()
switch_ cases = do
    exitName <- fresh
    cndNames <- replicateM (length cases) (freshName "case")
    stmtNames <- replicateM (length cases) (freshName "case_stmt")
    let nextNames = cndNames ++ [exitName]
    let (cmpCnds, cmpStmts) = unzip cases

    br (head nextNames)
    forM_ (zip5 cmpCnds cmpStmts cndNames stmtNames (tail nextNames)) $
        \(cmpCnd, cmpStmt, cndName, stmtName, nextName) -> do
            emitBlockStart cndName
            cnd <- cmpCnd
            condBr cnd stmtName nextName
            emitBlockStart stmtName
            cmpStmt
            br exitName

    br exitName
    emitBlockStart exitName


putchar :: MonadInstrCmp k o m => Char -> m Operand
putchar ch = do
    op <- ensureExtern "putchar" [i32] i32 False
    let c8 = fromIntegral (ord ch)
    call op [(int32 c8, [])]


putchar' :: MonadInstrCmp k o m => Operand -> m Operand
putchar' ch = do
    op <- ensureExtern "putchar" [i32] i32 False
    call op [(ch, [])]
    


printf :: MonadInstrCmp k o m => String -> [Operand] -> m Operand
printf fmt args = do
    op <- ensureExtern "printf" [ptr i8] i32 True
    str <- globalStringPtr fmt =<< fresh
    call op $ map (\a -> (a, [])) (cons str:args)


puts :: MonadInstrCmp k o m => Operand -> m Operand
puts str = do
    op <- ensureExtern "puts" [ptr i8] i32 False
    call op [(str, [])]


strcmp :: MonadInstrCmp k o m => Operand -> Operand -> m Operand
strcmp a b = do
    op <- ensureExtern "strcmp" [ptr i8, ptr i8] i32 False
    call op [(a, []), (b, [])] 


isCons :: Operand -> Bool
isCons (ConstantOperand _) = True
isCons _                   = False


isLocal :: Operand -> Bool
isLocal (LocalReference _ _) = True
isLocal _                    = False


isGlobal :: Operand -> Bool
isGlobal (ConstantOperand (C.GlobalReference _ _)) = True
isGlobal _                                         = False



toCons :: Operand -> C.Constant
toCons (ConstantOperand c) = c


cons :: C.Constant -> Operand
cons = ConstantOperand


externOf :: Definition -> Definition
externOf (GlobalDefinition var@(GlobalVariable _ _ _ _ _ _ _ _ _ _ _ _ _ _)) = GlobalDefinition (var { G.initializer = Nothing } )
externOf (GlobalDefinition fun@(Function _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)) = GlobalDefinition (fun { G.basicBlocks = [] } )


globalDef :: Name -> Type -> Maybe C.Constant -> (Definition, Operand)
globalDef nm ty init = (def, op)
    where
        op  = ConstantOperand $ C.GlobalReference (ptr ty) nm
        def = GlobalDefinition globalVariableDefaults
            { G.name        = nm
            , G.type'       = ty
            , G.initializer = init
            }


funcDef :: Name -> [(Type, Name)] -> Type -> [BasicBlock] -> Definition
funcDef nm params retty blocks = GlobalDefinition functionDefaults
    { G.name        = nm
    , G.parameters  = ([Parameter t n [] | (t, n) <- params], False)
    , G.returnType  = retty
    , G.basicBlocks = blocks
    }


stringDef :: Name -> String -> (Definition, Operand)
stringDef name str = (def, op)
    where
        chars    = map (fromIntegral . ord) str
        chArr    = C.Array i8 $ map (C.Int 8) (chars ++ [0])
        typ      = typeOf chArr
        (def, _) = globalDef name typ (Just chArr)
        op       = cons (C.GetElementPtr True (C.GlobalReference (ptr typ) name) [C.Int 32 0, C.Int 32 0])


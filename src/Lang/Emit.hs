{-# LANGUAGE OverloadedStrings #-}

module Lang.Emit where

import qualified Data.Map as Map

import Control.Monad.State.Strict
import qualified Language.Java.Syntax as S
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST.Constant as CONST
import qualified LLVM.AST.Float as FT
import qualified LLVM.Prelude as LP
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import Lang.Codegen
import Lang.Misc

import Debug.Trace

import Data.Char (ord)
import LLVM.Prelude (toInteger)

codegenModule :: S.CompilationUnit -> LLVM()
codegenModule (S.CompilationUnit package impt ty ) = do
  case package of
    Nothing -> codegenDefine ty
    Just (S.PackageDecl name) -> withPackageName (getPackageName name) ty
    where
      getPackageName :: S.Name -> S.Ident
      getPackageName (S.Name idents) = S.Ident $ foldl (\seek (S.Ident str) -> str ++ "_" ++ seek) "_" idents
      withPackageName :: S.Ident -> [S.TypeDecl] -> LLVM ()
      withPackageName (S.Ident pkname) typeDecls = do
        renameModule pkname
        codegenDefine typeDecls
      codegenDefine :: [S.TypeDecl] -> LLVM ()
      codegenDefine = mapM_ codegenTypeDecl

      codegenTypeDecl :: S.TypeDecl -> LLVM ()
      codegenTypeDecl (S.InterfaceTypeDecl interfaceDecl) = undefined
      codegenTypeDecl (S.ClassTypeDecl classDecl) = codegenClassDecl classDecl

      codegenClassDecl :: S.ClassDecl -> LLVM ()
      codegenClassDecl (S.EnumDecl _ ident refTypes body) = undefined
      codegenClassDecl (S.ClassDecl _ (S.Ident className) params refType refTypes classBody) = do
        pkname <- gets AST.moduleName
        renameModule (toString pkname ++ "_" ++ className)
        codegenClassBody classBody

      codegenClassBody :: S.ClassBody -> LLVM ()
      codegenClassBody (S.ClassBody decls) = mapM_ codegenDecl decls

      codegenDecl :: S.Decl -> LLVM ()
      codegenDecl (S.MemberDecl memberDecl) = codegenMemberDecl memberDecl
      codegenDecl (S.InitDecl _ _) = undefined

      codegenMemberDecl :: S.MemberDecl -> LLVM ()
      codegenMemberDecl (S.FieldDecl _ ty varDecls) = undefined
      codegenMemberDecl (S.MethodDecl modifiers tyParams retTy (S.Ident methodName) args _ exp methodBody) = do
        case modifiers of
          S.Public:S.Static:S.Native:[] -> external (toLType retTy) methodName (map toSig args)
          _ -> define (toLType retTy) methodName (map toSig args) (bls methodBody)
        where
          bls :: S.MethodBody -> [AST.BasicBlock]
          bls body = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM (map toSig args) $ \(ty,nm@(AST.Name str)) -> do
              var <- alloca ty Nothing
              store ty var (local ty nm)
              assign (toString str) var
            let stmts = getStmts body

            forM (init stmts) $ \stmt -> cgen stmt
            cgen (last stmts)

      codegenMemberDecl _ = undefined

getStmts :: S.MethodBody -> [S.Stmt]
getStmts (S.MethodBody Nothing) = []
getStmts (S.MethodBody (Just (S.Block blockStmts))) = map getStmt blockStmts

getStmt :: S.BlockStmt -> S.Stmt
getStmt (S.BlockStmt stmt) = stmt
getStmt _ = undefined

cgen :: S.Stmt -> Codegen ()
cgen (S.StmtBlock (S.Block blockStmts)) = undefined
cgen (S.Return Nothing) = undefined
cgen (S.Return (Just exp)) = do
  operand <- cgenExp exp
  traceM $ "codegen : ret -> " ++ show operand
  ret operand
  return ()

cgenExp :: S.Exp -> Codegen AST.Operand
cgenExp (S.ExpName (S.Name idents)) = do
  let name = foldl (\seek (S.Ident str) -> seek ++ str) "" idents
  var <- getvar name
  traceM $ "name : " ++ name ++ ", var : " ++ show var
  return var

cgenExp (S.Lit (S.Int i32)) = return $ AST.ConstantOperand (CONST.Int 32 i32)
cgenExp (S.Lit (S.Word i16)) = return $ AST.ConstantOperand (CONST.Int 16 i16)
cgenExp (S.Lit (S.Float float)) = return $ AST.ConstantOperand (CONST.Float (FT.Double float))
cgenExp (S.Lit (S.Double double)) = return $ AST.ConstantOperand (CONST.Float (FT.Double double))
cgenExp (S.Lit (S.Boolean bool)) = return $ AST.ConstantOperand (CONST.Int 8 (if bool then 1 else 0))
cgenExp (S.Lit (S.Char char)) = return $ AST.ConstantOperand (CONST.Int 8 (toInteger (ord char)))
cgenExp (S.Lit (S.String string)) = do
  let array = map (\char -> (CONST.Int 8 (toInteger (ord char)))) string
  return $ AST.ConstantOperand (CONST.Array Type.i8 array)
cgenExp (S.Lit S.Null) = return $ AST.ConstantOperand (CONST.Null Type.void)

cgenExp (S.BinOp expa op expb) = do
  oa <- cgenExp expa
  ob <- cgenExp expb
  case op of
    S.Add -> add Type.i32 oa ob
    _ -> error ("未知的操作符 : " ++ show op)

toLType :: Maybe S.Type -> AST.Type
toLType Nothing = Type.void
toLType (Just (S.PrimType S.BooleanT)) = Type.i8
toLType (Just (S.PrimType S.ByteT)) = Type.i8
toLType (Just (S.PrimType S.ShortT)) = Type.i16
toLType (Just (S.PrimType S.IntT)) = Type.i32
toLType (Just (S.PrimType S.LongT)) = Type.i64
toLType (Just (S.PrimType S.CharT)) = Type.i8
toLType (Just (S.PrimType S.FloatT)) = Type.float
toLType (Just (S.PrimType S.DoubleT)) = Type.double

toSig :: S.FormalParam -> (AST.Type,AST.Name)
toSig (S.FormalParam _ ty _ (S.VarId (S.Ident name))) = (toLType (Just ty),AST.mkName name)

codegen :: S.CompilationUnit -> IO AST.Module
codegen unit = withContext $ \context -> do
  withModuleFromAST context newast $ \modu -> do
    llstr <- moduleLLVMAssembly modu
    (putStrLn . toString') llstr
    return newast
  where
    mod = emptyModule ""
    newast = runLLVM mod (codegenModule unit)
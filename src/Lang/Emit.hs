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
import LLVM.AST.AddrSpace
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import Lang.Codegen
import Lang.Misc

import Debug.Trace

import Data.Char (ord)
import LLVM.Prelude (toInteger)

scanModule :: S.CompilationUnit -> ModuleContext
scanModule (S.CompilationUnit package impt typeDecls) =
  foldl (\m decl -> scanTypeDecl decl m) Map.empty typeDecls

scanTypeDecl :: S.TypeDecl -> ModuleContext -> ModuleContext
scanTypeDecl (S.ClassTypeDecl classDecl) = scanClassDecl classDecl
scanTypeDecl _ = id

scanClassDecl :: S.ClassDecl -> ModuleContext -> ModuleContext
scanClassDecl (S.ClassDecl _ (S.Ident className) _ _ _ (S.ClassBody decls)) =
  flip (foldl (\m decl -> scanDecl decl m)) decls
scanClassDecl _ = id

scanDecl :: S.Decl -> ModuleContext -> ModuleContext
scanDecl (S.MemberDecl memberDecl) = scanMemberDecl memberDecl
scanDecl _ = id

scanMemberDecl :: S.MemberDecl -> ModuleContext -> ModuleContext
scanMemberDecl (S.MethodDecl modifiers tyParams retTy (S.Ident methodName) args _ exp _) =
  Map.insert fnName fnTy
  where
    fnName = AST.mkName methodName
    fnTy = Type.FunctionType (toLType retTy) (map (fst . toSig) args) False
scanMemberDecl _ = id

codegenModule :: S.CompilationUnit -> LLVM ()
codegenModule unit@(S.CompilationUnit package impt ty) = do
  let ctx = scanModule unit
  traceM $ "Scan Function : " ++ show (ctx)
  case package of
    Nothing -> codegenDefine ctx ty
    Just (S.PackageDecl name) -> withPackageName (getPackageName name) ty ctx
    where
      getPackageName :: S.Name -> S.Ident
      getPackageName (S.Name idents) = S.Ident $ foldl (\seek (S.Ident str) -> seek ++ "_" ++ str) "" idents
      withPackageName :: S.Ident -> [S.TypeDecl] -> ModuleContext -> LLVM ()
      withPackageName (S.Ident pkname) typeDecls ctx = do
        renameModule pkname
        codegenDefine ctx typeDecls
      codegenDefine :: ModuleContext -> [S.TypeDecl] -> LLVM ()
      codegenDefine ctx = mapM_ (flip codegenTypeDecl ctx)

      codegenTypeDecl :: S.TypeDecl -> ModuleContext -> LLVM ()
      codegenTypeDecl (S.InterfaceTypeDecl interfaceDecl) = undefined
      codegenTypeDecl (S.ClassTypeDecl classDecl) = codegenClassDecl classDecl

      codegenClassDecl :: S.ClassDecl -> ModuleContext -> LLVM ()
      codegenClassDecl (S.EnumDecl _ ident refTypes body) ctx = undefined
      codegenClassDecl (S.ClassDecl _ (S.Ident className) params refType refTypes classBody) ctx = do
        pkname <- gets AST.moduleName
        renameModule (toString pkname ++ "_" ++ className)
        codegenClassBody classBody ctx

      codegenClassBody :: S.ClassBody  -> ModuleContext -> LLVM ()
      codegenClassBody (S.ClassBody decls) ctx = mapM_ (flip codegenDecl ctx) decls

      codegenDecl :: S.Decl -> ModuleContext -> LLVM ()
      codegenDecl (S.MemberDecl memberDecl) = codegenMemberDecl memberDecl
      codegenDecl (S.InitDecl _ _) = undefined

      codegenMemberDecl :: S.MemberDecl -> ModuleContext -> LLVM ()
      codegenMemberDecl (S.FieldDecl _ ty varDecls) ctx = undefined
      codegenMemberDecl (S.MethodDecl modifiers tyParams retTy (S.Ident methodName) args _ exp methodBody) ctx = do
        traceM $ "ret type : " ++ show retTy
        case modifiers of
          S.Public:S.Static:S.Native:[] -> external (toLType retTy) methodName (map toSig args)
          _ -> define (toLType retTy) methodName (map toSig args) (bls methodBody ctx)
        where
          bls :: S.MethodBody -> ModuleContext -> [AST.BasicBlock]
          bls body ctx = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM (map toSig args) $ \(ty,nm@(AST.Name str)) -> do
--              var <- alloca ty Nothing
--              store ty var (local ty nm)
              assign (toString str) (AST.LocalReference ty nm)
            let blockStmts = getBlockStmts body
            insts <- mapM (flip cgenBlockStmt ctx) blockStmts
            if retTy == Nothing then
              ret Nothing
            else
              ret (Just (last insts))

      codegenMemberDecl _ ctx = undefined

getBlockStmts :: S.MethodBody -> [S.BlockStmt]
getBlockStmts (S.MethodBody Nothing) = []
getBlockStmts (S.MethodBody (Just (S.Block blockStmts))) = blockStmts

getStmt :: S.BlockStmt -> S.Stmt
getStmt (S.BlockStmt stmt) = stmt

getVars :: S.BlockStmt -> [(S.VarDecl,S.Type)]
getVars (S.LocalVars _ ty varDecls) = map (\varDecl -> (varDecl,ty)) varDecls

getLocalClass :: S.BlockStmt -> S.ClassDecl
getLocalClass (S.LocalClass classDecl) = classDecl

cgenBlockStmt :: S.BlockStmt -> ModuleContext -> Codegen AST.Operand
cgenBlockStmt (S.BlockStmt stmt) ctx = cgenStmt stmt ctx
cgenBlockStmt (S.LocalVars _ ty varDecls) ctx = do
  a <- mapM (\varDecl -> cgenVar varDecl ty ctx) varDecls
  return $ last a
cgenBlockStmt _ _ = undefined

cgenStmt :: S.Stmt -> ModuleContext -> Codegen AST.Operand
cgenStmt (S.StmtBlock (S.Block blockStmts)) = undefined
cgenStmt (S.Return Nothing) = undefined
cgenStmt (S.ExpStmt exp) = cgenExp exp
cgenStmt (S.Return (Just exp)) = cgenExp exp

cgenVar :: S.VarDecl -> S.Type -> ModuleContext -> Codegen AST.Operand
cgenVar (S.VarDecl (S.VarId (S.Ident strid)) Nothing) sty ctx = do
  let ty = toLType (Just sty)
      nm = AST.mkName strid
  var <- alloca ty Nothing
  assign strid var
  return var
cgenVar (S.VarDecl (S.VarId (S.Ident strid)) (Just (S.InitExp exp))) sty ctx = do
  let ty = toLType (Just sty)
      nm = AST.mkName strid
  var <- alloca ty Nothing
  val <- cgenExp exp ctx
  assign strid var
  return var


cgenExp :: S.Exp -> ModuleContext -> Codegen AST.Operand
cgenExp (S.ExpName (S.Name idents)) _ = do
  let name = foldl (\seek (S.Ident str) -> seek ++ str) "" idents
  var <- getvar name
  return var
cgenExp (S.MethodInv (S.MethodCall (S.Name idents) fnParamsExp)) ctx = do
  fnParams <- mapM (flip cgenExp ctx) fnParamsExp
  call fn fnParams
  where
    fnName = foldl (\seek (S.Ident str) -> seek ++ str) "" idents
    fnType = case Map.lookup (AST.mkName fnName) ctx of
      Just f -> f
      Nothing -> error $ "未定以的函数_" ++ fnName
    fnP = Type.PointerType fnType (AddrSpace 0)
    fn = externf fnP (AST.mkName fnName)

cgenExp (S.Lit (S.Int i32)) _ = return $ AST.ConstantOperand (CONST.Int 32 i32)
cgenExp (S.Lit (S.Word i16)) _ = return $ AST.ConstantOperand (CONST.Int 16 i16)
cgenExp (S.Lit (S.Float float)) _ = return $ AST.ConstantOperand (CONST.Float (FT.Double float))
cgenExp (S.Lit (S.Double double)) _ = return $ AST.ConstantOperand (CONST.Float (FT.Double double))
cgenExp (S.Lit (S.Boolean bool)) _ = return $ AST.ConstantOperand (CONST.Int 8 (if bool then 1 else 0))
cgenExp (S.Lit (S.Char char)) _ = return $ AST.ConstantOperand (CONST.Int 8 (toInteger (ord char)))
cgenExp (S.Lit (S.String string)) _ = do
  let array = map (\char -> (CONST.Int 8 (toInteger (ord char)))) string
  return $ AST.ConstantOperand (CONST.Array Type.i8 array)
cgenExp (S.Lit S.Null) _ = return $ AST.ConstantOperand (CONST.Null Type.void)

cgenExp (S.BinOp expa op expb) ctx = do
  oa <- cgenExp expa ctx
  ob <- cgenExp expb ctx
  case op of
    S.Add   -> add Type.i32 oa ob
    S.Sub   -> sub Type.i32 oa ob
    S.Mult  -> mul Type.i32 oa ob
    S.Div   -> udiv Type.i32 oa ob
    S.Rem   -> urem Type.i32 oa ob
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

codegen :: FilePath -> S.CompilationUnit -> IO AST.Module
codegen filename unit = withContext $ \context -> do
  withModuleFromAST context newast $ \modu -> do
    llstr <- moduleLLVMAssembly modu
    (putStrLn . toString') llstr
    return newast
  where
    mod = emptyModule filename
    newast = runLLVM mod (codegenModule unit)
module Pristinum.SAbs where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text hiding
  ( find,
    map,
  )
import Pristinum
import Prelude hiding (id)

-- import Prelude hiding (id)

type TProgram = ([Record], [(Bind, Maybe TExpr)], [TFunction])

data TFunction = TFFunction IDENT [Bind] Type [TStmt]
  deriving (Eq, Ord, Show, Read)

data TStmt
  = TSExpr TExpr
  | TSBind Bind (Maybe TExpr)
  | TSIf TExpr [TStmt] [TStmt]
  | TSDoWhile TExpr [TStmt]
  | TSReturn (Maybe TExpr)
  deriving (Eq, Ord, Show, Read)

type TExpr = (Type, TExpr')

data TExpr'
  = TEAssign TExpr TExpr
  | TELOr TExpr TExpr
  | TELAnd TExpr TExpr
  | TEBOr TExpr TExpr
  | TEBXor TExpr TExpr
  | TEBAnd TExpr TExpr
  | TEEqual TExpr TExpr
  | TENotEqual TExpr TExpr
  | TELess TExpr TExpr
  | TELessEqual TExpr TExpr
  | TEGreater TExpr TExpr
  | TEGreaterEqual TExpr TExpr
  | TEBShl TExpr TExpr
  | TEBShr TExpr TExpr
  | TEAdd TExpr TExpr
  | TESubtract TExpr TExpr
  | TEPower TExpr TExpr
  | TEMultiply TExpr TExpr
  | TEDivide TExpr TExpr
  | TEMod TExpr TExpr
  | TEIncr TExpr
  | TEDecr TExpr
  | TEPos TExpr
  | TENeg TExpr
  | TELNot TExpr
  | TEBNot TExpr
  | TECast Type TExpr
  | TEDeref TExpr
  | TERef TExpr
  | TESizeof Type
  | TEAlignof Type
  | TEPIncr TExpr
  | TEPDecr TExpr
  | TECall IDENT [TExpr]
  | TEIndex TExpr TExpr
  | TEAccess TExpr TExpr
  | TEPAccess TExpr TExpr
  | TENil
  | TETrue
  | TEFalse
  | TEChar Char
  | TEInt Integer
  | TEDouble Double
  | TEString String
  | TEIdent IDENT
  deriving (Eq, Ord, Show, Read)

type Name = Text

data BindingLoc
  = F Function
  | R Record
  | Toplevel
  deriving (Show)

data SemanticError
  = IllegalBinding Name BindingKind VarKind BindingLoc
  | UndefinedSymbol Name SymbolKind Expr
  | TypeError [Type] Type Stmt
  | CastError Type Type Stmt
  | ArgError Int Int Expr
  | Redeclaration Name
  | NoMain
  | AddressError Expr
  | AssignmentError Expr Expr
  | AccessError Expr Expr
  | DeadCode Stmt
  deriving (Show)

data BindingKind
  = Duplicate
  | Void
  deriving (Show)

data SymbolKind
  = Var
  | Func
  deriving (Show)

data VarKind
  = Global
  | Formal
  | Local
  | RecordField
  deriving (Eq, Ord, Show)

type Vars = M.Map (Text, VarKind) Type

type Funcs = M.Map Text Function

type Records = [Record]

data Env = Env
  { sVars :: Vars,
    sFuncs :: Funcs,
    sRecords :: Records
  }

type Semantic = ExceptT SemanticError (State Env)

checkBinds :: VarKind -> BindingLoc -> [Bind] -> Semantic [Bind]
checkBinds kind loc binds = do
  forM binds $ \case
    -- check for illegal types
    BBind (IDENT name) TVoid -> throwError $ IllegalBinding name Void kind loc
    BBind ident@(IDENT name) ty -> do
      vars <- gets sVars
      when (M.member (name, kind) vars) $
        throwError (IllegalBinding name Duplicate kind loc)
      modify $ \env -> env {sVars = M.insert (name, kind) ty vars}
      pure $ BBind ident ty

checkFields :: Record -> Semantic Record
checkFields r@(RRecord recordType ident fields) = do
  fields' <- foldM addField M.empty fields
  pure $ RRecord recordType ident (M.elems fields')
  where
    addField acc field@(BBind (IDENT name) ty) = case ty of
      -- check for illegal types
      TVoid -> throwError $ IllegalBinding name Void RecordField (R r)
      _ ->
        if M.member name acc
          then throwError (IllegalBinding name Duplicate RecordField (R r))
          else pure $ M.insert name field acc

builtIns :: Funcs
builtIns =
  M.fromList $
    map
      toFunc
      [ ("printf", [TPointer TChar], TVoid),
        ("malloc", [TInt64], TPointer TVoid),
        ("free", [TPointer TVoid], TVoid)
      ]
  where
    toFunc (name, tys, retty) =
      (name, FFunction (IDENT name) (map (BBind "__placeholder") tys) retty [])

isNumeric :: Type -> Bool
isNumeric t = case t of
  TVoid -> False
  TBool -> False
  TChar -> False
  TInt64 -> True
  TFloat64 -> True
  TPointer ty -> True
  TIdent id -> False

checkExpr :: Expr -> Semantic TExpr
checkExpr expr = case expr of
  EAssign ex ex' -> undefined
  ELOr ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TELOr lhs rhs
     in if t1 == t2 && t1 == TBool then pure (t1, texpr) else throwError $ TypeError [TBool] t1 (SExpr expr)
  ELAnd ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TELAnd lhs rhs
     in if t1 == t2 && t1 == TBool then pure (t1, texpr) else throwError $ TypeError [TBool] t1 (SExpr expr)
  EBOr ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEBOr lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EBXor ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEBXor lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EBAnd ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEBAnd lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EEqual ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEEqual lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (TBool, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  ENotEqual ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TENotEqual lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (TBool, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  ELess ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TELess lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (TBool, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  ELessEqual ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TELessEqual lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (TBool, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EGreater ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEGreater lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (TBool, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EGreaterEqual ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEGreaterEqual lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (TBool, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EBShl ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEBShl lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EBShr ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEBShr lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EAdd ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEAdd lhs rhs
     in case (t1, t2) of
          (TPointer t, TInt64) -> pure (TPointer t, texpr)
          (TInt64, TPointer t) -> pure (TPointer t, texpr)
          (TInt64, TInt64) -> pure (TInt64, texpr)
          (TFloat64, TFloat64) -> pure (TFloat64, texpr)
          _ -> throwError $ TypeError [TPointer TVoid, TInt64, TFloat64] t1 (SExpr expr)
  ESubtract ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TESubtract lhs rhs
     in case (t1, t2) of
          (TPointer t, TInt64) -> pure (TPointer t, texpr)
          (TInt64, TPointer t) -> pure (TPointer t, texpr)
          (TPointer t, TPointer t') ->
            if t == t'
              then pure (TInt64, texpr)
              else throwError $ TypeError [TPointer t'] (TPointer t) (SExpr expr)
          (TInt64, TInt64) -> pure (TInt64, texpr)
          (TFloat64, TFloat64) -> pure (TFloat64, texpr)
          _ -> throwError $ TypeError [TPointer TVoid, TInt64, TFloat64] t1 (SExpr expr)
  EPower ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEPower lhs rhs
     in case (t1, t2) of
          (TFloat64, TFloat64) -> pure (TFloat64, TECall (IDENT "llvm.pow.f64") [lhs, rhs])
          (TFloat64, TInt64) -> pure (TFloat64, TECall (IDENT "llvm.powi.i64") [lhs, rhs])
          (TInt64, TInt64) -> pure (TInt64, texpr)
          _ -> throwError $ TypeError [TFloat64, TInt64] t1 (SExpr expr)
  EMultiply ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEMultiply lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EDivide ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEDivide lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EMod ex ex' -> do
    lhs@(t1, _) <- checkExpr ex
    rhs@(t2, _) <- checkExpr ex'
    let texpr = TEMod lhs rhs
     in if t1 == t2 && isNumeric t1 then pure (t1, texpr) else throwError $ TypeError [TInt64, TFloat64] t1 (SExpr expr)
  EIncr ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TEIncr e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  EDecr ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TEDecr e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  EPos ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TEPos e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  ENeg ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TENeg e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  ELNot ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TELNot e
     in if ty == TBool then pure (ty, texpr) else throwError $ TypeError [TBool] ty (SExpr expr)
  EBNot ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TEBNot e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  ECast ty ex -> undefined
  EDeref ex -> do
    e@(ty, e') <- checkExpr ex
    let texpr = TEDeref e
     in case ty of
          TPointer ty' -> pure (ty', texpr)
          _ -> throwError $ TypeError [TPointer TVoid, TPointer TInt64, TPointer TFloat64] ty (SExpr expr)
  ERef ex -> do
    e@(ty, e') <- checkExpr ex
    let texpr = TERef e
     in case e' of
          TEDeref x0 -> pure (TPointer ty, texpr)
          TERef x0 -> pure (TPointer ty, texpr)
          TEIndex x0 x1 -> pure (TPointer ty, texpr)
          TEAccess x0 x1 -> pure (TPointer ty, texpr)
          TEPAccess x0 x1 -> pure (TPointer ty, texpr)
          TEIdent id -> pure (TPointer ty, texpr)
          _ -> throwError (AddressError ex)
  ESizeof ty -> pure (TInt64, TESizeof ty)
  EAlignof ty -> pure (TInt64, TEAlignof ty)
  EPIncr ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TEPIncr e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  EPDecr ex -> do
    e@(ty, _) <- checkExpr ex
    let texpr = TEPDecr e
     in if isNumeric ty then pure (ty, texpr) else throwError $ TypeError [TInt64, TFloat64] ty (SExpr expr)
  ECall id exs -> undefined
  EIndex ex ex' -> undefined
  EAccess ex ex' -> undefined
  EPAccess ex ex' -> undefined
  ENil -> pure (TPointer TVoid, TENil)
  ETrue -> pure (TBool, TETrue)
  EFalse -> pure (TBool, TEFalse)
  EChar c -> pure (TChar, TEChar c)
  EInt n -> pure (TInt64, TEInt n)
  EDouble x -> pure (TFloat64, TEDouble x)
  EString s -> pure (TPointer TChar, TEString s)
  EIdent id@(IDENT s) -> do
    vars <- gets sVars
    let foundVars = map (\kind -> M.lookup (s, kind) vars) [Local, Formal, Global]
    case join $ find isJust foundVars of
      Nothing -> throwError $ UndefinedSymbol s Var expr
      Just ty -> pure (ty, TEIdent id)

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

-- import Pristinum
-- import Prelude hiding (id)

-- type TProgram = ([Record], [Bind], [TFunction])

-- data TFunction = TFFunction IDENT [Bind] Type [TStmt]
--   deriving (Eq, Ord, Show, Read)

-- data TStmt
--   = TSExpr TExpr
--   | TSBind Bind TExpr
--   | TSAssign IDENT TExpr
--   | TSIf TExpr [TStmt] [TStmt]
--   | TSDoWhile TExpr [TStmt]
--   | TSReturn (Maybe TExpr)
--   deriving (Eq, Ord, Show, Read)

-- type TExpr = (Type, TExpr')

-- data TExpr'
--   = TENil
--   | TETrue
--   | TEFalse
--   | TEChar Char
--   | TEInt Integer
--   | TEDouble Double
--   | TEString String
--   | TEIdent IDENT
--   | TECall IDENT [TExpr]
--   | TECast Type TExpr
--   | TESizeof Type
--   | TEAlignof Type
--   | TEUnOp UnOp TExpr
--   | TEBinOp BinOp TExpr TExpr
--   deriving (Eq, Ord, Show, Read)

-- type Name = Text

-- data BindingLoc
--   = F Function
--   | R Record
--   | Toplevel
--   deriving (Show)

-- data SemanticError
--   = IllegalBinding Name BindingKind VarKind BindingLoc
--   | UndefinedSymbol Name SymbolKind Expr
--   | TypeError [Type] Type Stmt
--   | CastError Type Type Stmt
--   | ArgError Int Int Expr
--   | Redeclaration Name
--   | NoMain
--   | AddressError Expr
--   | AssignmentError Expr Expr
--   | AccessError Expr Expr
--   | DeadCode Stmt
--   deriving (Show)

-- data BindingKind
--   = Duplicate
--   | Void
--   deriving (Show)

-- data SymbolKind
--   = Var
--   | Func
--   deriving (Show)

-- data VarKind
--   = Global
--   | Formal
--   | Local
--   | RecordField
--   deriving (Eq, Ord, Show)

-- type Vars = M.Map (Text, VarKind) Type

-- type Funcs = M.Map Text Function

-- type Records = [Record]

-- data Env = Env
--   { sVars :: Vars,
--     sFuncs :: Funcs,
--     sRecords :: Records
--   }

-- type Semantic = ExceptT SemanticError (State Env)

-- checkBinds :: VarKind -> BindingLoc -> [Bind] -> Semantic [Bind]
-- checkBinds kind loc binds = do
--   forM binds $ \case
--       -- check for illegal types
--     BBind (IDENT name) TVoid -> throwError $ IllegalBinding name Void kind loc
--     BBind ident@(IDENT name) ty -> do
--       vars <- gets sVars
--       when (M.member (name, kind) vars) $
--         throwError (IllegalBinding name Duplicate kind loc)
--       modify $ \env -> env {sVars = M.insert (name, kind) ty vars}
--       pure $ BBind ident ty

-- checkFields :: Record -> Semantic Record
-- checkFields r@(RRecord recordType ident fields) = do
--   fields' <- foldM addField M.empty fields
--   pure $ RRecord recordType ident (M.elems fields')
--   where
--     addField acc field@(BBind (IDENT name) ty) = case ty of
--       -- check for illegal types
--       TVoid -> throwError $ IllegalBinding name Void RecordField (R r)
--       _ ->
--         if M.member name acc
--           then throwError (IllegalBinding name Duplicate RecordField (R r))
--           else pure $ M.insert name field acc

-- builtIns :: Funcs
-- builtIns =
--   M.fromList $
--     map
--       toFunc
--       [ ("printf", [TPointer TChar], TVoid),
--         ("malloc", [TUint64], TPointer TVoid),
--         ("free", [TPointer TVoid], TVoid)
--       ]
--   where
--     toFunc (name, tys, retty) =
--       (name, FFunction (IDENT name) (map (BBind "__placeholder") tys) retty [])

-- data TypeInfo = TypeInfo
--   { tiIsVoid :: Bool,
--     tiIsBool :: Bool,
--     tiIsChar :: Bool,
--     tiIsInt :: Bool,
--     tiIsUint :: Bool,
--     tiIsFloat :: Bool,
--     tiIsPointer :: Bool,
--     tiConvertibleTo :: [Type],
--     tiConvertibleFrom :: [Type],
--     tiDecayableTo :: Type
--   }
--   deriving (Show)

-- getTypeInfo :: Type -> TypeInfo
-- getTypeInfo ty = case ty of
--   TVoid ->
--     TypeInfo
--       { tiIsVoid = True,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo = [],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TBool ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = True,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TChar ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = True,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TInt8 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = True,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TInt16 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = True,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TInt32 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = True,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TInt64 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = True,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TUint8 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = True,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TUint16 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = True,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TUint32 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = True,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TUint64 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = True,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TFloat32 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = True,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TFloat64 ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = True,
--         tiIsPointer = False,
--         tiConvertibleTo =
--           [ TInt8,
--             TInt16,
--             TInt32,
--             TInt64,
--             TUint8,
--             TUint16,
--             TUint32,
--             TUint64,
--             TFloat32,
--             TFloat64
--           ],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   TPointer ty' ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = True,
--         tiConvertibleTo = [TPointer TVoid],
--         tiConvertibleFrom = [TPointer TVoid],
--         tiDecayableTo = decayType ty
--       }
--   TArray ty' ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = True,
--         tiConvertibleTo = [TPointer TVoid],
--         tiConvertibleFrom = [TPointer TVoid],
--         tiDecayableTo = decayType ty
--       }
--   TRecord id ->
--     TypeInfo
--       { tiIsVoid = False,
--         tiIsBool = False,
--         tiIsChar = False,
--         tiIsInt = False,
--         tiIsUint = False,
--         tiIsFloat = False,
--         tiIsPointer = False,
--         tiConvertibleTo = [],
--         tiConvertibleFrom = [],
--         tiDecayableTo = decayType ty
--       }
--   where
--     decayType t = case t of
--       TPointer t' -> TPointer (decayType t')
--       TArray t' -> TPointer (decayType t')
--       _ -> t

-- checkEUnOp :: (UnOp, Expr) -> Semantic TExpr
-- checkEUnOp (op, e) = do
--   e'@(ty,_) <- checkExpr e
--   case op of
--     UOIncr -> _
--     UODecr -> _
--     UONegative -> _
--     UOPositive -> _
--     UOBitNot -> _
--     UONot -> _
--     UODeref -> _
--     UORef -> _

-- checkExpr :: Expr -> Semantic TExpr
-- checkExpr = undefined

-- checkExpr expr = case expr of
--   ENil -> pure (TPointer TVoid, TENil)
--   ETrue -> pure (TBool, TETrue)
--   EFalse -> pure (TBool, TEFalse)
--   EChar c -> pure (TChar, TEChar c)
--   EInt n -> pure (TInt64, TEInt n)
--   EDouble x -> pure (TFloat64, TEDouble x)
--   EString s -> pure (TPointer TChar, TEString s)
--   EIdent ident@(IDENT s) -> do
--     vars <- gets sVars
--     let foundVars =
--           map (\kind -> M.lookup (s, kind) vars) [Local, Formal, Global]
--     case join $ find isJust foundVars of
--       Nothing -> throwError $ UndefinedSymbol s Var expr
--       Just ty -> pure (ty, TEIdent ident)
--   ECall id exs -> _
--   ECast ty ex -> _
--   ESizeof ty -> pure (TUint64, TESizeof ty)
--   EAlignof ty -> pure (TUint64, TEAlignof ty)
--   EUnOpPost ex uo -> _
--   EUnOpPre uo ex -> _
--   EBinOpPost ex ex' bo -> checkBinOp bo ex ex'
--   EBinOpPre bo ex ex' -> checkBinOp bo ex ex'
--   where
--     checkBinOp op lhs rhs = do
--       lhs'@(t1, _) <- checkExpr lhs
--       rhs'@(t2, _) <- checkExpr rhs
--       let assertSym =
--             unless (t1 == t2) $ throwError $ TypeError [t1] t2 (SExpr expr)
--           checkArith = do
--             unless (isNumeric t1) $
--               throwError $
--                 TypeError
--                   [ TChar,
--                     TInt8,
--                     TInt16,
--                     TInt32,
--                     TInt64,
--                     TUint8,
--                     TUint16,
--                     TUint32,
--                     TUint64,
--                     TFloat32,
--                     TFloat64
--                   ]
--                   t1
--                   (SExpr expr)
--             pure (t1, TEBinOp op lhs' rhs')
--           checkBool = do
--             unless (t1 == TBool) $ throwError $ TypeError [TBool] t1 (SExpr expr)
--             pure (t1, TEBinOp op lhs' rhs')
--           checkRelational = case (snd lhs', snd rhs') of
--             (TENil, _) -> checkExpr ()
--       case op of
--         BOAdd ->
--           let sexpr = TEBinOp BOAdd lhs' rhs'
--            in case (t1, t2) of
--                 (TPointer t, TInt8) -> pure (TPointer t, sexpr)
--                 (TPointer t, TInt16) -> pure (TPointer t, sexpr)
--                 (TPointer t, TInt32) -> pure (TPointer t, sexpr)
--                 (TPointer t, TInt64) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint8) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint16) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint32) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint64) -> pure (TPointer t, sexpr)
--                 (TInt8, TInt8) -> pure (TInt8, sexpr)
--                 (TInt16, TInt16) -> pure (TInt16, sexpr)
--                 (TInt32, TInt32) -> pure (TInt32, sexpr)
--                 (TInt64, TInt64) -> pure (TInt64, sexpr)
--                 (TUint8, TUint8) -> pure (TUint8, sexpr)
--                 (TUint16, TUint16) -> pure (TUint16, sexpr)
--                 (TUint32, TUint32) -> pure (TUint32, sexpr)
--                 (TUint64, TUint64) -> pure (TUint64, sexpr)
--                 (TFloat32, TFloat32) -> pure (TFloat32, sexpr)
--                 (TFloat64, TFloat64) -> pure (TFloat64, sexpr)
--                 _ ->
--                   throwError $
--                     TypeError
--                       [ TPointer TVoid,
--                         TInt8,
--                         TInt16,
--                         TInt32,
--                         TInt64,
--                         TUint8,
--                         TUint16,
--                         TUint32,
--                         TUint64,
--                         TFloat32,
--                         TFloat64
--                       ]
--                       t1
--                       (SExpr expr)
--         BOSubtract ->
--           let sexpr = TEBinOp BOSubtract lhs' rhs'
--            in case (t1, t2) of
--                 (TPointer t, TInt8) -> pure (TPointer t, sexpr)
--                 (TPointer t, TInt16) -> pure (TPointer t, sexpr)
--                 (TPointer t, TInt32) -> pure (TPointer t, sexpr)
--                 (TPointer t, TInt64) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint8) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint16) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint32) -> pure (TPointer t, sexpr)
--                 (TPointer t, TUint64) -> pure (TPointer t, sexpr)
--                 (TInt8, TInt8) -> pure (TInt8, sexpr)
--                 (TInt16, TInt16) -> pure (TInt16, sexpr)
--                 (TInt32, TInt32) -> pure (TInt32, sexpr)
--                 (TInt64, TInt64) -> pure (TInt64, sexpr)
--                 (TUint8, TUint8) -> pure (TUint8, sexpr)
--                 (TUint16, TUint16) -> pure (TUint16, sexpr)
--                 (TUint32, TUint32) -> pure (TUint32, sexpr)
--                 (TUint64, TUint64) -> pure (TUint64, sexpr)
--                 (TFloat32, TFloat32) -> pure (TFloat32, sexpr)
--                 (TFloat64, TFloat64) -> pure (TFloat64, sexpr)
--                 _ ->
--                   throwError $
--                     TypeError
--                       [ TPointer TVoid,
--                         TInt8,
--                         TInt16,
--                         TInt32,
--                         TInt64,
--                         TUint8,
--                         TUint16,
--                         TUint32,
--                         TUint64,
--                         TFloat32,
--                         TFloat64
--                       ]
--                       t1
--                       (SExpr expr)
--         BOMultiply -> assertSym >> checkArith
--         BODivide -> assertSym >> checkArith
--         BOMod -> assertSym >> checkArith
--         BOPower -> case (t1, t2) of
--           (TInt8, TInt8) ->
--             pure (TInt8, TECall (IDENT "llvm.powi.i8") [lhs', rhs'])
--           (TInt16, TInt16) ->
--             pure (TInt16, TECall (IDENT "llvm.powi.i16") [lhs', rhs'])
--           (TInt32, TInt32) ->
--             pure (TInt32, TECall (IDENT "llvm.powi.i32") [lhs', rhs'])
--           (TInt64, TInt64) ->
--             pure (TInt64, TECall (IDENT "llvm.powi.i64") [lhs', rhs'])
--           (TUint8, TUint8) ->
--             pure (TUint8, TECall (IDENT "llvm.powi.u8") [lhs', rhs'])
--           (TUint16, TUint16) ->
--             pure (TUint16, TECall (IDENT "llvm.powi.u16") [lhs', rhs'])
--           (TUint32, TUint32) ->
--             pure (TUint32, TECall (IDENT "llvm.powi.u32") [lhs', rhs'])
--           (TUint64, TUint64) ->
--             pure (TUint64, TECall (IDENT "llvm.powi.u64") [lhs', rhs'])
--           (TFloat32, TFloat32) ->
--             pure (TFloat32, TECall (IDENT "llvm.pow.f32") [lhs', rhs'])
--           (TFloat64, TFloat64) ->
--             pure (TFloat64, TECall (IDENT "llvm.pow.f64") [lhs', rhs'])
--           _ ->
--             throwError $
--               TypeError
--                 [ TInt8,
--                   TInt16,
--                   TInt32,
--                   TInt64,
--                   TUint8,
--                   TUint16,
--                   TUint32,
--                   TUint64,
--                   TFloat32,
--                   TFloat64
--                 ]
--                 t1
--                 (SExpr expr)
--         BOBitShl -> assertSym >> checkArith
--         BOBitShr -> assertSym >> checkArith
--         BOBitAnd -> assertSym >> checkArith
--         BOBitOr -> assertSym >> checkArith
--         BOBitXor -> assertSym >> checkArith
--         BOEqual -> _
--         BONotEqual -> _
--         BOLess -> _
--         BOGreater -> _
--         BOLessEqual -> _
--         BOGreaterEqual -> _
--         BOAnd -> assertSym >> checkBool
--         BOOr -> assertSym >> checkBool
--         BOAccess -> _
--         BOPAccess -> _
--     isNumeric = \case
--        TChar -> True
--        TInt8 -> True
--        TInt16 -> True
--        TInt32 -> True
--        TInt64 -> True
--        TUint8 -> True
--        TUint16 -> True
--        TUint32 -> True
--        TUint64 -> True
--        TFloat32 -> True
--        TFloat64 -> True
--        TPointer _ -> True
--        _ -> False

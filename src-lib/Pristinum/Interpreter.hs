{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Pristinum.Interpreter
  ( interpretString,
  )
where

import Control.Exception hiding
  ( Exception,
    evaluate,
    throw,
  )
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Control.Monad.State.Strict as State
import Data.Bits
import Data.Foldable
import Data.IORef.Lifted
import qualified Data.Map.Strict as Map
import Data.Maybe
import Pristinum.AST
import Pristinum.Parser
import System.IO

data Value
  = ValNull
  | ValBool Bool
  | ValNumber Integer
  | ValString String
  | ValFunction Identifier [Identifier] [Stmt] Env
  | ValBuiltinFunction Identifier Int ([Expr] -> Interpreter Value)

instance Show Value where
  show = \case
    ValNull -> "null"
    ValBool b -> show b
    ValNumber n -> show n
    ValString s -> show s
    ValFunction name _ _ _ -> "function " <> name
    ValBuiltinFunction name _ _ -> "builtin function " <> name

instance Eq Value where
  ValNull == ValNull = True
  ValBool b1 == ValBool b2 = b1 == b2
  ValNumber n1 == ValNumber n2 = n1 == n2
  ValString s1 == ValString s2 = s1 == s2
  _ == _ = False

newtype Interpreter a = Interpreter
  { runInterpreter :: ExceptT Exception (StateT InterpreterState IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase IO,
      MonadState InterpreterState,
      MonadError Exception
    )

type Env = Map.Map String (IORef Value)

newtype InterpreterState = InterpreterState
  { isEnv :: Env
  }

initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> builtinEnv

builtinEnv :: IO Env
builtinEnv = do
  printFn <- newIORef $ ValBuiltinFunction "print" 1 executePrint
  return $ Map.fromList [("print", printFn)]

data Exception
  = Return Value
  | RuntimeError String

defineVar :: Identifier -> Value -> Interpreter ()
defineVar name value = do
  env <- State.gets isEnv
  env' <- defineVarEnv name value env
  setEnv env'

defineVarEnv :: Identifier -> Value -> Env -> Interpreter Env
defineVarEnv name value env = do
  valueRef <- newIORef value
  return $ Map.insert name valueRef env

setEnv :: Env -> Interpreter ()
setEnv env = State.modify' (\is -> is {isEnv = env})

lookupVar :: Identifier -> Interpreter Value
lookupVar name = State.gets isEnv >>= findValueRef name >>= readIORef

assignVar :: Identifier -> Value -> Interpreter ()
assignVar name value =
  State.gets isEnv >>= findValueRef name >>= flip writeIORef value

findValueRef :: Identifier -> Env -> Interpreter (IORef Value)
findValueRef name env = case Map.lookup name env of
  Just ref -> return ref
  Nothing -> throw $ "Unknown variable: " <> name

throw :: String -> Interpreter a
throw = throwError . RuntimeError

evaluate :: Expr -> Interpreter Value
evaluate = \case
  ExprNull -> pure ValNull
  ExprBool b -> pure $ ValBool b
  ExprNumber n -> pure $ ValNumber n
  ExprString s -> pure $ ValString s
  ExprVariable v -> lookupVar v
  call@ExprCall {} -> evaluateCall call
  unary@ExprUnaryOp {} -> evaluateUnaryOp unary
  binary@ExprBinaryOp {} -> evaluateBinaryOp binary

evaluateCall :: Expr -> Interpreter Value
evaluateCall ~(ExprCall name argEs) =
  lookupVar name >>= \case
    ValBuiltinFunction _ arity func -> do
      checkArgCount name argEs arity
      func argEs
    func@(ValFunction _ params body env) -> do
      checkArgCount name argEs (length params)
      callEnv <- State.gets isEnv
      setupCallEnv
      retVal <- executeBody callEnv
      setEnv callEnv
      return retVal
      where
        setupCallEnv = do
          args <- traverse evaluate argEs
          env' <- defineVarEnv name func env
          setEnv env'
          for_ (zip params args) $ uncurry defineVar
        executeBody callEnv =
          (traverse_ execute body >> return ValNull) `catchError` \case
            Return val -> return val
            err -> setEnv callEnv >> throwError err
    val -> throw $ "Invalid function call: " <> show val

checkArgCount :: Foldable t => [Char] -> t a -> Int -> Interpreter ()
checkArgCount name args arity =
  when (length args /= arity) $
    throw $
      name
        <> " call expected "
        <> show arity
        <> " argument(s) but received "
        <> show (length args)

executePrint :: [Expr] -> Interpreter Value
executePrint args = evaluate (head args) >>= liftIO . print >> return ValNull

evaluateUnaryOp :: Expr -> Interpreter Value
evaluateUnaryOp ~(ExprUnaryOp op e) = do
  expr <- evaluate e
  let errMsg msg = msg <> ": " <> show expr
  case (op, expr) of
    (UnOpNegative, ValNumber n) -> pure $ ValNumber (negate n)
    (UnOpBitwiseNot, ValNumber n) -> pure $ ValNumber (complement n)
    (UnOpLogicalNot, ValBool b) -> pure $ ValBool (not b)
    _ -> throw $ errMsg "Invalid unary operation"

evaluateBinaryOp :: Expr -> Interpreter Value
evaluateBinaryOp ~(ExprBinaryOp op e1 e2) = do
  expr1 <- evaluate e1
  expr2 <- evaluate e2
  let errMsg msg = msg <> ": " <> show expr1 <> " and " <> show expr2
  case (op, expr1, expr2) of
    (BinOpAdd, ValNumber n1, ValNumber n2) -> pure $ ValNumber (n1 + n2)
    (BinOpAdd, ValString s1, ValString s2) -> pure $ ValString (s1 <> s2)
    (BinOpAdd, ValString s1, _) -> pure $ ValString (s1 <> show expr2)
    (BinOpAdd, _, ValString s2) -> pure $ ValString (show expr1 <> s2)
    (BinOpSubtract, ValNumber n1, ValNumber n2) -> pure $ ValNumber (n1 - n2)
    (BinOpMultiply, ValNumber n1, ValNumber n2) -> pure $ ValNumber (n1 * n2)
    (BinOpDivide, ValNumber n1, ValNumber n2) -> pure $ ValNumber (n1 `div` n2)
    (BinOpMod, ValNumber n1, ValNumber n2) -> pure $ ValNumber (n1 `mod` n2)
    (BinOpBitwiseShiftLeft, ValNumber n1, ValNumber n2) ->
      pure $ ValNumber (n1 `shiftL` fromInteger n2)
    (BinOpBitwiseShiftRight, ValNumber n1, ValNumber n2) ->
      pure $ ValNumber (n1 `shiftR` fromInteger n2)
    (BinOpBitwiseAnd, ValNumber n1, ValNumber n2) ->
      pure $ ValNumber (n1 .&. n2)
    (BinOpBitwiseOr, ValNumber n1, ValNumber n2) ->
      pure $ ValNumber (n1 .|. n2)
    (BinOpBitwiseXor, ValNumber n1, ValNumber n2) ->
      pure $ ValNumber (n1 `xor` n2)
    (BinOpEqual, _, _) -> pure $ ValBool (expr1 == expr2)
    (BinOpNotEqual, _, _) -> pure $ ValBool (expr1 /= expr2)
    (BinOpLess, ValNumber n1, ValNumber n2) -> pure $ ValBool (n1 < n2)
    (BinOpGreater, ValNumber n1, ValNumber n2) -> pure $ ValBool (n1 > n2)
    (BinOpLessEqual, ValNumber n1, ValNumber n2) -> pure $ ValBool (n1 <= n2)
    (BinOpGreaterEqual, ValNumber n1, ValNumber n2) ->
      pure $ ValBool (n1 >= n2)
    (BinOpLogicalAnd, ValBool b1, ValBool b2) -> pure $ ValBool (b1 && b2)
    (BinOpLogicalOr, ValBool b1, ValBool b2) -> pure $ ValBool (b1 || b2)
    _ -> throw $ errMsg "Invalid binary operation"

execute :: Stmt -> Interpreter ()
execute = \case
  ExprStmt expr -> void $ evaluate expr
  LetStmt name expr -> evaluate expr >>= defineVar name
  AssignStmt name expr -> evaluate expr >>= assignVar name
  IfStmt expr body mElseBody -> do
    cond <- evaluate expr
    if isTruthy cond
      then traverse_ execute body
      else case mElseBody of
        Just elseBody -> traverse_ execute elseBody
        Nothing -> pure ()
  while@(WhileStmt expr body) -> do
    cond <- evaluate expr
    when (isTruthy cond) $ do
      traverse_ execute body
      execute while
  FunctionStmt name params body -> do
    env <- State.gets isEnv
    defineVar name $ ValFunction name params body env
  ReturnStmt mExpr -> do
    mRet <- traverse evaluate mExpr
    throwError . Return . fromMaybe ValNull $ mRet
  where
    isTruthy = \case
      ValNull -> False
      ValBool b -> b
      _ -> True

interpret :: Program -> IO (Either String ())
interpret (Program stmts) = do
  state <- initInterpreterState
  retVal <-
    flip evalStateT state . runExceptT . runInterpreter $
      traverse_
        execute
        stmts
  case retVal of
    Left (RuntimeError err) -> return $ Left err
    Left (Return _) -> return $ Left "Cannot return from outside of functions"
    Right _ -> return $ Right ()

interpretString :: String -> IO ()
interpretString str = do
  case parseString str of
    Left err -> hPutStrLn stderr err
    Right program ->
      interpret program >>= \case
        Left err -> hPutStrLn stderr $ "ERROR: " <> err
        _ -> return ()

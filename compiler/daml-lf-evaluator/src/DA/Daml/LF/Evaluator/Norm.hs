-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module DA.Daml.LF.Evaluator.Norm
  ( normalize,
  ) where

import Control.Monad (forM,liftM,ap)
import DA.Daml.LF.Evaluator.Exp (Prog(..),Exp,Defs,DefKey,Alt,FieldName)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Evaluator.Exp as Exp
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text


-- entry point -- IO in return type only for dev-time debug
normalize :: Prog -> IO Prog
normalize Prog{defs,main} = do
  (defs,main) <- run defs (norm main >>= reify)
  return $ Prog{defs,main}


norm :: Exp -> Effect Norm
norm = \case
  Exp.Lit v -> return $ Syntax $ Exp.Lit v

  Exp.Var x -> do
    env <- GetEnv
    case Map.lookup x env of
      Nothing -> error $ "norm, " <> show x
      Just v -> return v

  Exp.App e1 e2 -> do
    v1 <- norm e1
    v2 <- norm e2
    apply (v1,v2)

  Exp.Lam x body -> do
    restore <- Save
    return $ Macro $ \arg ->
      restore $ ModEnv (Map.insert x arg) $ norm body

  Exp.Let x rhs body -> do
    v <- norm rhs
    ModEnv (Map.insert x v) $ norm body

  Exp.Rec elems -> do
    xs <- forM elems $ \(name,exp) -> do
      n <- norm exp
      return (name,n)
    return $ Record xs

  Exp.Dot exp name -> do
    r <- norm exp
    normProjectRec name r

  Exp.Con tag elems -> do
    elems <- forM elems $ \elem -> do
      norm elem >>= reify
    --return $ Value.Constructed tag vs
    return $ Syntax $ Exp.Con tag elems

  Exp.Match{scrut,alts} -> do
    scrut <- norm scrut >>= reify
    alts <- mapM normAlt alts
    return $ Syntax $ Exp.Match {scrut,alts}

  Exp.Ref i ->
    Share i


normAlt :: Alt -> Effect Alt
normAlt = \case
  Exp.Alt{tag,bound,rhs} -> do
    nameMapping <- do
      forM bound $ \x -> do
        y <- Fresh
        return (x,y)
    let bound = map snd nameMapping
    let f env = foldr (\(x,y) -> Map.insert x (Syntax $ Exp.Var y)) env nameMapping
    rhs <- ModEnv f $ norm rhs >>= reify
    return $ Exp.Alt{tag,bound,rhs}


data Norm -- Normalized Expression
  = Syntax Exp
  | Record [(FieldName,Norm)]
  | Macro (Norm -> Effect Norm)

apply :: (Norm,Norm) -> Effect Norm
apply = \case
  (Syntax func, arg) -> do
    exp <- reify arg
    return $ Syntax (Exp.App func exp)
  (Record _, _) -> error "Norm,apply,record"
  (Macro func, arg@(Syntax exp)) ->
    if isAtomic exp
    then func arg
    else do
      x <- Fresh
      body <- func (Syntax (Exp.Var x)) >>= reify
      return $ Syntax $ Exp.Let x exp body
  (Macro func, arg) ->
    func arg

sayEverythingIsAtomic :: Bool
sayEverythingIsAtomic = False

isAtomic :: Exp -> Bool
isAtomic = \case
  Exp.Lit{} -> True
  Exp.Var{} -> True
  _ -> sayEverythingIsAtomic

reify :: Norm -> Effect Exp
reify = \case
  Syntax exp -> return exp
  Record xs -> do
    elems <- forM xs $ \(name,n) -> do
      exp <- reify n
      return (name,exp)
    return $ Exp.Rec elems
  Macro f -> do
    x <- Fresh
    body <- f (Syntax (Exp.Var x)) >>= reify
    return $ Exp.Lam x body

normProjectRec :: FieldName -> Norm -> Effect Norm
normProjectRec fieldName = \case
  Macro _ -> error "normProjectRec,Macro"
  Syntax exp -> return $ Syntax $ Exp.Dot exp fieldName
  Record xs -> do
    case lookup fieldName xs of
      Nothing -> error $ "normProjectRec, " <> show fieldName
      Just v -> return v


data Effect a where
  Ret :: a -> Effect a
  Bind :: Effect a -> (a -> Effect b) -> Effect b
  IO :: IO a -> Effect a
  GetEnv :: Effect Env
  ModEnv :: (Env -> Env) -> Effect a -> Effect a
  Fresh :: Effect Exp.Var
  Share :: Int -> Effect Norm
  Save :: Effect (Effect a -> Effect a)
  Restore :: InlineScope -> Env -> Effect a -> Effect a


instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind


run :: Defs -> Effect a -> IO (Defs, a)
run defs e = do
  (v,State{retainedDefs=defs}) <- run Set.empty env0 state0 e
  return (defs,v)

  where
    env0 = Map.empty
    state0 = State { unique = 0, retainedDefs = Map.empty }

    run :: InlineScope -> Env -> State -> Effect a -> IO (a,State)
    run scope env state = \case
      Ret x -> return (x,state)
      Bind e f -> do (v,state') <- run scope env state e; run scope env state' (f v)
      IO io -> do x <- io; return (x,state)
      GetEnv -> return (env,state)
      ModEnv f e -> run scope (f env) state e
      Save -> return (Restore scope env, state)
      Restore scope env e -> run scope env state e

      Fresh -> do
        let State{unique} = state
        let state' = state { unique = unique + 1 }
        let x = LF.ExprVarName $ Text.pack ("_u"<> show unique) -- assumes no clash
        return (x,state')

      Share i -> do
        -- directly calls: norm/reify
        let (name,exp) = getDef i
        let doInline = isRecord exp || not (Set.member i scope)
        let State{retainedDefs} = state
        --print ("Share"::String,i,doInline, scope, Map.keys retainedDefs)
        case Map.lookup i retainedDefs of
          Just _ -> return (Syntax $ Exp.Ref i, state)
          Nothing ->
            if doInline then run (Set.insert i scope) env state (norm exp)
            else mdo
              let state1 = state { retainedDefs = Map.insert i (name,expN) retainedDefs }
              (normalized,state2) <- run scope env state1 (norm exp)
              (expN,state3) <- run scope env state2 (reify normalized)
              return (normalized,state3)

    getDef :: Int -> (DefKey,Exp)
    getDef i =
      case Map.lookup i defs of
        Nothing -> error $ "getDef, " <> show i
        Just x -> x


isRecord :: Exp -> Bool
isRecord = \case
  Exp.Rec{} -> True
  _ -> False


type Env = Map Exp.Var Norm
data State = State { unique :: Unique, retainedDefs :: Exp.Defs }
type Unique = Int
type InlineScope = Set Int

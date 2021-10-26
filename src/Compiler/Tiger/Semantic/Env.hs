{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.Tiger.Semantic.Env
  ( EnvEntry (..),
    Env (..),
    getTenv,
    putTenv,
    getVenv,
    putVenv,
    scopeBegin,
    scopeEnd,
    baseEnv,
  )
where

import Compiler.Tiger.Semantic.STable
import Compiler.Tiger.Semantic.Types
import qualified Compiler.Tiger.Semantic.Types
import Compiler.Tiger.Symbol (Sym)
import Control.Monad.State
import Data.Char (isSpace)
import Prelude hiding (lookup)

data EnvEntry = VarEntry Type | FuncEntry {formals :: [Type], result :: Type} deriving (Show)

data Env = Env {tenv :: STable Type, venv :: STable EnvEntry}

getTenv :: Sym -> State Env (Maybe Type)
getTenv s = get >>= \(Env t _) -> return $ lookup s t

putTenv :: Sym -> Type -> State Env ()
putTenv s val = get >>= \(Env t v) -> put $ Env (insert s val t) v

getVenv :: Sym -> State Env (Maybe EnvEntry)
getVenv s = get >>= \(Env t v) -> return $ lookup s v

putVenv :: Sym -> EnvEntry -> State Env ()
putVenv s e = get >>= \(Env t v) -> put $ Env t (insert s e v)

scopeBegin :: State Env ()
scopeBegin = get >>= \(Env t v) -> put $ Env (enter t) (enter v)

scopeEnd :: State Env ()
scopeEnd = get >>= \(Env t v) -> put $ Env (exit t) (exit v)

baseEnv :: Env
baseEnv = execState initEnv $ Env empty empty
  where
    baseTenv = empty
    baseVenv = empty

    initEnv = do
      putTenv "int" IntType
      putTenv "string" StringType

      putVenv "print" $ FuncEntry [StringType] Void
      putVenv "flush" $ FuncEntry [] Void
      putVenv "getchar" $ FuncEntry [] StringType
      putVenv "ord" $ FuncEntry [StringType] IntType
      putVenv "chr" $ FuncEntry [IntType] StringType
      putVenv "size" $ FuncEntry [StringType] IntType
      putVenv "substring" $ FuncEntry [StringType, IntType, IntType] StringType
      putVenv "concat" $ FuncEntry [StringType, StringType] StringType
      putVenv "not" $ FuncEntry [IntType] IntType
      putVenv "exit" $ FuncEntry [IntType] Void

instance Show Env where
  show (Env t v) =
    unlines
      [ "============= TENV =============",
        _trimTail (show t),
        "",
        "============= VENV =============",
        _trimTail (show v)
      ]

_trimTail :: [Char] -> [Char]
_trimTail = reverse . trimHead . reverse
  where
    trimHead [] = []
    trimHead ((isSpace -> True) : xs) = trimHead xs
    trimHead xs = xs
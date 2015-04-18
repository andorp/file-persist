{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Database.Persist.File.TH where

import Language.Haskell.TH.Lib (varE)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Database.Persist.Class
import Database.Persist.Sql hiding (Migration, migrate)
import Database.Persist.TH

import Database.Persist.File.Migrate (Migration, migrate)


mkFileMigrate :: String -> [EntityDef] -> Q [Dec]
mkFileMigrate fun allDefs = do
  body' <- body
  return
    [ SigD (mkName fun) typ
    , FunD (mkName fun) [normalClause [] body']
    ]
  where
    defs = filter isMigrated allDefs
    isMigrated def = not $ "no-migrate" `elem` entityAttrs def
    --typ = AppT (ConT ''IO) (ConT ''())
    typ = ConT ''Migration

    body :: Q Exp
    body =
      case defs of
        [] -> [|return ()|]
        _  -> do
          defsName <- newName "defs"
          defStmt <- do
            defs' <- mapM lift defs
            let defsExp = ListE defs'
            return $ LetS [ValD (VarP defsName) (NormalB defsExp) []]
          stmts <- mapM (toStmt $ VarE defsName) defs
          return (DoE $ defStmt : stmts)

    toStmt :: Exp -> EntityDef -> Q Stmt
    toStmt defsExp ed = do
      u <- lift ed
      m <- [|migrate|]
      return $ NoBindS $ m `AppE` defsExp `AppE` u

normalClause :: [Pat] -> Exp -> Clause
normalClause p e = Clause p (NormalB e) []


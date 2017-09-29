{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Audit.SQL
  ( selectAudit
  , auditInsert
  , auditDelete
  , auditUpdate
  , selectAuditActivity
  , whereEq
  ) where

import Data.List (intercalate)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Dynamic (pgSafeLiteralString)
import Database.PostgreSQL.Typed.Inet (PGInet)
import Database.PostgreSQL.Typed.Query (makePGQuery, parseQueryFlags)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Audit.Types

makeAudit :: Timestamp -> Id Party -> PGInet -> AuditAction -> Audit
makeAudit t u = Audit t . AuditIdentity u

selectAudit :: String -> Selector
selectAudit table = selectColumns 'makeAudit table ["audit_time", "audit_user", "audit_ip", "audit_action"]

actionCmd :: AuditAction -> String
actionCmd AuditActionAdd = "INSERT INTO"
actionCmd AuditActionChange = "UPDATE"
actionCmd AuditActionRemove = "DELETE FROM"
actionCmd a = error $ "actionCmd: " ++ show a

auditQuery :: AuditAction -> TH.Name -- ^ @'AuditIdentity'@
  -> String -> String -> Maybe SelectOutput -> TH.ExpQ
auditQuery action ident tablef stmt =
  maybe (makePGQuery flags sql) (makeQuery flags ((sql ++) . (" RETURNING " ++)))
  where
  sql = "WITH audit_row AS (" <> actionCmd action <> " " <> table <> " " <> stmt
    <> " RETURNING *) INSERT INTO audit." <> table
    <> " SELECT CURRENT_TIMESTAMP, ${auditWho " <> idents <> "}, ${auditIp " <> idents <> "}, " <> pgSafeLiteralString action <> ", * FROM audit_row"
  idents = nameRef ident
  (flags, table) = parseQueryFlags tablef

auditInsert :: TH.Name -> String -> [(String, String)] -> Maybe SelectOutput -> TH.ExpQ
auditInsert ident table args =
  auditQuery AuditActionAdd ident table
    ('(' : intercalate "," (map fst args) ++ ") VALUES (" ++ intercalate "," (map snd args) ++ ")")

auditDelete :: TH.Name -> String -> String -> Maybe SelectOutput -> TH.ExpQ
auditDelete ident table wher =
  auditQuery AuditActionRemove ident table ("WHERE " ++ wher)

auditUpdate :: TH.Name -> String -> [(String, String)] -> String -> Maybe SelectOutput -> TH.ExpQ
auditUpdate ident table sets wher =
  auditQuery AuditActionChange ident table
    ("SET " ++ intercalate "," (map pairEq sets) ++ " WHERE " ++ wher)

selectAuditActivity :: String -> Selector -- ^ @'Timestamp'@
selectAuditActivity table =
  selector ("audit." ++ table ++ " AS audit") (SelectColumn "audit" "audit_time")

pairEq :: (String, String) -> String
pairEq (c, v) = c ++ "=" ++ v

whereEq :: [(String, String)] -> String
whereEq = intercalate " AND " . map pairEq

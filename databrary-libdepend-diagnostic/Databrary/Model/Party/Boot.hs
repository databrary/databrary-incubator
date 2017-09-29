{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Party.Boot
  ( loadParty
  ) where

import Database.PostgreSQL.Typed.Query (makePGQuery, simpleQueryFlags)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types

$(useTDB)

loadParty :: Id Party -> Permission -> TH.ExpQ -- ^ @'Party'@
loadParty i perm = do
  p <- runTDB $
         dbQuery1'
              $(makePGQuery 
                  (simpleQueryFlags)
                  (   "SELECT party.id,party.name,party.prename,party.orcid,party.affiliation,party.url"
                   ++ " FROM party " 
                   ++ "WHERE party.id = ${i}"))
  let p' = (\(pid,nm,pn,orc,af,ur) -> PartyRow pid nm pn orc af ur) p
  TH.lift $ Party p' Nothing perm Nothing
  

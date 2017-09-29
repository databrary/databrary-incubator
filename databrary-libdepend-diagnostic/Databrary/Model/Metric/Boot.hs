{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Metric.Boot
  ( loadMetrics
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Database.PostgreSQL.Typed.Query (makePGQuery, simpleQueryFlags)

import Databrary.Service.DB
import Databrary.Model.Metric.SQL

$(useTDB)

loadMetrics :: TH.ExpQ
loadMetrics = do
  l <- runTDB $ 
           (dbQuery 
              $(makePGQuery 
                  (simpleQueryFlags)
                  (   "SELECT metric.id,metric.category,metric.name,metric.release,metric.type,metric.options,metric.assumed,metric.description,metric.required"
                   ++ " FROM metric " 
                   ++ "ORDER BY id")))
  let l' = 
        fmap 
          (\(i, ca, nm, rl, ty, op, asm, dsc, req) -> makeMetric i ca nm rl ty op asm dsc req)
          l
  TH.lift l'

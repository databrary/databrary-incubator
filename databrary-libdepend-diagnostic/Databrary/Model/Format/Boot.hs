{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Format.Boot
  ( loadFormats
  ) where

import Database.PostgreSQL.Typed.Query (parseQueryFlags)
import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.SQL.Select
import Databrary.Model.Format.Types (makeFormat)

loadFormats :: TH.ExpQ
loadFormats = do
  l <- runTDB 
         $ dbQuery 
            $(makeQuery
                (fst (parseQueryFlags "ORDER BY id"))
                (\_ -> 
                       "SELECT format.id,format.mimetype,format.extension,format.name"
                    ++ " FROM format " 
                    ++ (snd (parseQueryFlags "ORDER BY id")))
                (OutputJoin
                   False 
                   'makeFormat 
                   [ SelectColumn "format" "id"
                   , SelectColumn "format" "mimetype"
                   , SelectColumn "format" "extension"
                   , SelectColumn "format" "name" ]))
  TH.lift l

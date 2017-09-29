{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Notification.Boot
  ( makeNotice
  ) where

import Data.Int (Int16)
import Data.Ix (Ix)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Typed.Query (pgSQL)
import qualified Language.Haskell.TH as TH

import Databrary.Service.DB

useTDB

makeNotice :: TH.DecsQ
makeNotice = do
  nl <- runTDB $ dbQuery [pgSQL|SELECT id, name FROM notice WHERE id >= 0 ORDER BY id|]
  return
    [ TH.DataD [] typn [] (map (\(_, n) -> TH.NormalC (conn n) []) nl)
      [''Eq, ''Ord, ''Enum, ''Ix, ''Bounded, ''Typeable]
    , TH.InstanceD [] (TH.ConT ''Show `TH.AppT` TH.ConT typn)
      [ TH.FunD 'show $ map (\(_, n) -> TH.Clause [TH.ConP (conn n) []]
        (TH.NormalB $ TH.LitE $ TH.StringL n) []) nl
      ]
    , TH.SigD (TH.mkName "noticeToId") (TH.ArrowT `TH.AppT` TH.ConT typn `TH.AppT` TH.ConT ''Int16)
    , TH.FunD (TH.mkName "noticeToId") $ map (\(i, n) -> TH.Clause [TH.ConP (conn n) []]
        (TH.NormalB $ TH.LitE $ liti i) []) nl
    , TH.SigD (TH.mkName "noticeFromId") (TH.ArrowT `TH.AppT` TH.ConT ''Int16 `TH.AppT` (TH.ConT ''Maybe `TH.AppT` TH.ConT typn))
    , TH.FunD (TH.mkName "noticeFromId") $ map (\(i, n) -> TH.Clause [TH.LitP $ liti i]
        (TH.NormalB $ TH.AppE (TH.ConE 'Just) $ TH.ConE $ conn n) []) nl
        ++ [TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE 'Nothing) []]
    , TH.SigD (TH.mkName "noticeFromName") (TH.ArrowT `TH.AppT` TH.ConT ''String `TH.AppT` (TH.ConT ''Maybe `TH.AppT` TH.ConT typn))
    , TH.FunD (TH.mkName "noticeFromName") $ map (\(_, n) -> TH.Clause [TH.LitP $ TH.StringL n]
        (TH.NormalB $ TH.AppE (TH.ConE 'Just) $ TH.ConE $ conn n) []) nl
        ++ [TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE 'Nothing) []]
    ]
  where
  liti :: Int16 -> TH.Lit
  liti = TH.IntegerL . toInteger
  typn = TH.mkName "Notice"
  conn n = TH.mkName $ "Notice" ++ n

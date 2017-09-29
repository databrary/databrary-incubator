{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, TemplateHaskell, TypeSynonymInstances, LiberalTypeSynonyms #-}
module Databrary.Has
  ( Has(..)
  , MonadHas
  , peek
  , peeks
  , focusReaderT
  , focusReader
  , focusLift
  , focusBase
  , focusIO
  , makeHasFor
  , makeHasRec
  ) where

import Control.Monad (unless, liftM, liftM2)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT(..), reader, withReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Language.Haskell.TH as TH

class Has a c where
  view :: c -> a

instance Has a a where
  view = id

type MonadHas a c m = (Functor m, Applicative m, MonadReader c m, Has a c)

{-# INLINE peek #-}
peek :: (MonadReader c m, Has a c) => m a
peek = reader view

{-# INLINE peeks #-}
peeks :: (MonadReader c m, Has a c) => (a -> b) -> m b
peeks f = reader (f . view)

{-# INLINE focusReaderT #-}
focusReaderT :: (Monad m, Has a c) => ReaderT a m r -> ReaderT c m r
focusReaderT = withReaderT view

{-# INLINE focusReader #-}
focusReader :: (Monad m, Has a c) => (a -> m b) -> ReaderT c m b
focusReader f = ReaderT (f . view)

{-# INLINE[2] focusLift #-}
focusLift :: (MonadTrans t, Monad m, MonadHas a c (t m)) => (a -> m b) -> t m b
focusLift f = lift . f =<< peek

{-# INLINE[2] focusBase #-}
focusBase :: (MonadBase t m, MonadHas a c m) => (a -> t b) -> m b
focusBase f = liftBase . f =<< peek

{-# INLINE[2] focusIO #-}
focusIO :: (MonadIO m, MonadHas a c m) => (a -> IO b) -> m b
focusIO f = liftIO . f =<< peek

{-# RULES "focusLift/ReaderT" focusLift = focusReader #-}
{-# RULES "focusBase/ReaderT" focusBase = focusReader #-}
{-# RULES "focusIO/ReaderT" focusIO = focusReader #-}

getFieldType :: TH.Name -> TH.Name -> TH.TypeQ
getFieldType tn fn = do
  TH.VarI _ (TH.ArrowT `TH.AppT` TH.ConT tn' `TH.AppT` ft) _ _ <- TH.reify fn
  unless (tn' == tn) $ fail $ show tn ++ "." ++ show fn ++ ": field from wrong type: " ++ show tn'
  return ft

makeHasFor :: TH.Name -> [(TH.Name, TH.Type, [TH.Type])] -> TH.DecsQ
makeHasFor tn fs = concat <$> mapM
  (\(fn, ft, ts) -> concatM
    [d| instance Has $(return ft) $(return tt) where
          view = $(TH.varE fn) |]
    (\st ->
      [d| instance Has $(return st) $(return tt) where
            view = view . $(TH.varE fn) |])
    ts)
  fs
  where
  tt = TH.ConT tn
  concatM i f l = liftM2 (++) i (liftM concat $ mapM f l)

makeHasRec :: TH.Name -> [TH.Name] -> TH.DecsQ
makeHasRec tn fs = do
  TH.ClassI _ il <- TH.reify ''Has
  makeHasFor tn =<< mapM (\fn -> do
    ft <- getFieldType tn fn
    return (fn, ft, [ st
      | TH.InstanceD _ (TH.ConT hs `TH.AppT` st `TH.AppT` ft') _ <- il
      , hs == ''Has
      , ft' == ft
      ]))
    fs

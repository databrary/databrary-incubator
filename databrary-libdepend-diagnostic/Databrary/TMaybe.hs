{-# LANGUAGE KindSignatures, DataKinds, GADTs, RankNTypes #-}
module Databrary.TMaybe
  ( TMaybe(..)
  , unTMaybe
  , fromTJust
  , AMaybe(..)
  , aMaybe
  , unAMaybe
  ) where

data TMaybe (t :: Bool) a where
  TNothing :: TMaybe False a
  TJust :: a -> TMaybe True a

unTMaybe :: TMaybe t a -> Maybe a
unTMaybe TNothing = Nothing
unTMaybe (TJust a) = Just a

instance Functor (TMaybe hs) where
  fmap _ TNothing = TNothing
  fmap f (TJust a) = TJust (f a)

fromTJust :: TMaybe True a -> a
fromTJust (TJust a) = a

data AMaybe a = forall t . AMaybe (TMaybe t a)

aMaybe :: Maybe a -> AMaybe a
aMaybe Nothing = AMaybe TNothing
aMaybe (Just a) = AMaybe (TJust a)

unAMaybe :: AMaybe a -> Maybe a
unAMaybe (AMaybe TNothing) = Nothing
unAMaybe (AMaybe (TJust a)) = Just a

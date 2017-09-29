{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Mail
  ( MonadMail
  , sendMail
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (getCurrentTime)
import Network.Mail.Mime

import Databrary.Has
import Databrary.Model.Party
import Databrary.Service.Log

type MonadMail c m = (MonadLog c m)

-- |Wrap text to the given line length where possibleby changing some ' ' to '\n'.
-- >>> wrapText 10 $ TL.pack "hello there this is a test wherethereareexactlyvery long.\nLines.\n\nThing with multiple.\n"
-- "hello\nthere this\nis a test\nwherethereareexactlyvery\nlong.\nLines.\n\nThing with\nmultiple.\n"
wrapText :: Int64 -> TL.Text -> TL.Text
wrapText n = TL.unlines . concatMap wrap . TL.lines where
  wrap s
    | short s = [s]
    | (h:l) <- TL.breakOnAll " " s = let (p,r) = fb h l in p : wrap (TL.tail r)
    | otherwise = [s]
  fb p [] = p
  fb p (h@(t,_):l)
    | short t = fb h l
    | otherwise = p
  short s = TL.compareLength s n <= EQ

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

mailHeader :: TL.Text
mailHeader = TL.empty

mailFooter :: TL.Text
mailFooter = "\n\
  \Sincerely,\n\
  \The Databrary Team\n\
  \-- \n\
  \Databrary\n\
  \196 Mercer Street, Suite 807\n\
  \212-998-5536\n\
  \contact@databrary.org\n\
  \databrary.org\n"

sendMail :: MonadMail c m => [Either BS.ByteString Account] -> [Either BS.ByteString Account] -> T.Text -> TL.Text -> m ()
sendMail [] [] _ _ = return ()
sendMail to cc subj body = do
  t <- liftIO getCurrentTime
  focusIO $ logMsg t $ "mail " <> BS.intercalate ", " (map (either id accountEmail) to) <> ": " <> TE.encodeUtf8 subj
  liftIO $ renderSendMail $ addPart
    [Part "text/plain; charset=utf-8" None Nothing [] $ TLE.encodeUtf8 $ mailHeader <> wrapText 78 body <> mailFooter] baseMail
    { mailTo = map addr to
    , mailCc = map addr cc
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing (TE.decodeLatin1 e)
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just $ partyName $ partyRow p) (TE.decodeLatin1 email)


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Monad
--import Control.Monad.IO.Class
--import Data.String.Class
import Reflex
import Reflex.Dom
--import System.Exit
import Data.Text(Text)
import Data.Typeable (Typeable)
import Safe (lastMay)

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main =
  mainWidget $ do
      el "h3" $ text "reflex-frp ghcjs 11.20"
      (addRowsButton, _) <- el' "button" (text "Add rows")
      let buttonWithE = table <$ domEvent Click addRowsButton
      let colNames = ["a","b","c","d", "e"]
      _ <- elClass "table" "table" $ do
        el "thead" . el "tr" $ forM_ colNames $ \n ->
            el "th" (text n)
        el "tbody" $ do
          void $ widgetHold blank $ ffor buttonWithE $ \entries -> do
              forM_ entries $ \Entry{..} -> do
                let cells = [entrya, entryb, T.pack.show $ entryc, T.pack.show $ entryd, T.pack.show $ entrye]
                el "tr" . forM_ cells $ \cell -> do
                  el "td" $ text $ cell

          case lastMay table of
            Nothing -> pure ()
            Just Entry{..} -> do
              let cells = [entrya, entryb, T.pack.show $ entryc, T.pack.show $ entryd, T.pack.show $ entrye]
              el "tr" . forM_ cells $ \cell -> do
                el "td" $ text $ cell
      el "h3" $ text $ decodeUtf8 . BSL.toStrict . encode . head $ table
      el "h3" $ text $ T.pack.show $ decodedEntry
      return ()


data Foo =
    Foo
  | Bar
  | Baz String
  deriving (Show, Typeable)

decodedEntry :: Either String Entry
decodedEntry = eitherDecode "{\"entrya\":\"Some\",\"entryb\":\"Text here\",\"entryc\":1.23,\"entryd\":424242,\"entrye\":{\"tag\":\"Baz\",\"contents\":\"Moar\"}}"

data Entry = Entry
  { entrya :: Text
  , entryb :: Text
  , entryc :: Double
  , entryd :: Int
  , entrye :: Foo
  } deriving (Show, Typeable, Generic)

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry

table :: [Entry]
table = replicate 1000 Entry {entrya = "Some", entryb = "Text here", entryc = 1.23, entryd = 424242, entrye = Baz "Moar"}

$(deriveJSON defaultOptions ''Foo)
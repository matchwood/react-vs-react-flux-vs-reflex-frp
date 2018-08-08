{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude
import Miso
import Miso.String (ms)
import Data.Aeson
import Data.Aeson.TH
import Data.Typeable (Typeable)
import GHC.Generics
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8)

-- | Type synonym for an application model
type Model = [Entry]

-- | Sum type for application events
data Action
  = AddMany
  | NoAction

  deriving (Show, Eq)

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoAction -- initial action to be executed on application load
    model  = []                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoAction m = noEff m
updateModel AddMany _ = noEff table

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
      h3_ [] [text "miso ghcjs"]
    , button_ [ onClick AddMany ] [ text "Add rows"]
    , table_ [] [
        thead_ [] [tr_ [] $ map (\t -> th_ [] [text t]) ["a","b","c","d", "e"]]
        , tbody_ [] (toRows x)]
    , h3_ [] [text . ms . decodeUtf8 . BSL.toStrict . encode . head $ table]
    , h3_ [] [text . ms . T.pack . show  $ decodedEntry]
   ]


toRows :: Model -> [View Action]
toRows m = map toRow m

toRow :: Entry -> View Action
toRow Entry{..} = tr_ [] $ map (\t -> td_ [] [text . ms $ t]) [
    entrya
  , entryb
  , T.pack . show $ entryc
  , T.pack . show $ entryd
  , T.pack . show $ entrye
  ]

data Foo =
    Foo
  | Bar
  | Baz String
  deriving (Eq, Show, Typeable)

decodedEntry :: Either String Entry
decodedEntry = eitherDecode "{\"entrya\":\"Some\",\"entryb\":\"Text here\",\"entryc\":1.23,\"entryd\":424242,\"entrye\":{\"tag\":\"Baz\",\"contents\":\"Moar\"}}"

data Entry = Entry
  { entrya :: Text
  , entryb :: Text
  , entryc :: Double
  , entryd :: Int
  , entrye :: Foo
  } deriving (Eq, Show, Typeable, Generic)

instance ToJSON Entry where
  toEncoding = genericToEncoding Data.Aeson.TH.defaultOptions

instance FromJSON Entry

table :: [Entry]
table = replicate 1000 Entry {entrya = "Some", entryb = "Text here", entryc = 1.23, entryd = 424242, entrye = Baz "Moar"}

$(deriveJSON Data.Aeson.TH.defaultOptions ''Foo)
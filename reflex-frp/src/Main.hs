{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Monad
--import Control.Monad.IO.Class
--import Data.String.Class
import Reflex
import Reflex.Dom
--import System.Exit
import Data.Text(Text)
import Data.Typeable (Typeable)

import qualified Data.Text as T
main :: IO ()
main =
  mainWidget $ do
      el "h3" $ text "reflex-frp ghcjs"
      (addRowsButton, _) <- el' "button" (text "Add rows")
      let buttonWithE = table <$ domEvent Click addRowsButton
      let colNames = ["a","b","c","d"]
      _ <- elClass "table" "table" $ do
        el "thead" . el "tr" $ forM_ colNames $ \n ->
            el "th" (text n)
        el "tbody" $ do
          widgetHold blank $ ffor buttonWithE $ \entries -> do
              forM_ entries $ \e -> do
                let cells = [a e, b e, T.pack.show.c $ e, T.pack.show.d $ e]
                el "tr" . forM_ cells $ \cell -> do
                  el "td" $ text $ cell
      return ()

data Entry = Entry
  { a :: Text
  , b :: Text
  , c :: Double
  , d :: Int
  } deriving (Show, Typeable)

table :: [Entry]
table = replicate 1000 Entry {a = "Some", b = "Text here", c = 1.23, d = 424242}

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
import qualified Data.Text as T
main :: IO ()
main =
  mainWidget $ do
    clicked <- delay 0.1 =<< getPostBuild
    void . widgetHold blank $ ffor clicked $ \_ -> do
      rec

        entries <- foldDyn (\_ _ -> table) [] buttonWithE
        (button, _) <- el' "button" (text "Add rows")
        let buttonWithE = domEvent Click button
        let colNames = ["a","b","c","d"]
        _ <- elClass "table" "table" $ do
          el "thead" . el "tr" $ forM_ colNames $ \n ->
              el "th" (text n)
          el "tbody" $ do
            simpleList entries $ \e -> do
              let cells = [fmap a e, fmap b e, fmap (show . c) e, fmap (show .d) e]
              el "tr" . forM_ cells $ \cell -> do
                el "td" (dynText (fmap T.pack cell))
      return ()

data Entry = Entry
  { a :: String
  , b :: String
  , c :: Double
  , d :: Int
  }

table :: [Entry]
table = replicate 1000 Entry {a = "Some", b = "Text here", c = 1.23, d = 424242}

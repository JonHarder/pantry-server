{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where


import Data.Monoid (mconcat)
import Data.Text.IO as IO
import Web.Scotty


runApp :: IO ()
runApp = scotty 8000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where

import Data.Text.IO as IO


runApp :: IO ()
runApp = IO.putStrLn "running app.."

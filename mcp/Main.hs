module Main (main) where

import Relude

import qualified Haskclaw.Mcp.Server

main :: IO ()
main = Haskclaw.Mcp.Server.runServer

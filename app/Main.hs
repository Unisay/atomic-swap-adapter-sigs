{- |
Module: Main
Description: Atomic Swap Simulator Web Server

This executable runs the interactive atomic swap simulator web interface.
Users can step through the protocol execution with real-time state visualization.
-}
module Main (main) where

import Lucid (renderBS)
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai
  ( Application
  , pathInfo
  , requestMethod
  , responseFile
  , responseLBS
  )
import Network.Wai.Handler.Warp (run)

-- Crypto and Simulator imports
import AtomicSwap.Simulator.Render (mainPage, renderStep)
import AtomicSwap.Simulator.Run (runSimulatorT)
import AtomicSwap.Simulator.State (SimulatorState, emptySimulatorState)
import AtomicSwap.Simulator.Steps
  ( executeAliceGenerateSecret
  , executeAliceKeygen
  , executeAliceMakeCommitment
  , executeBobKeygen
  )
import Data.IORef.Strict (StrictIORef)
import Data.IORef.Strict qualified as Strict

--------------------------------------------------------------------------------
-- Server Implementation -------------------------------------------------------

mkApp :: StrictIORef SimulatorState -> Application
mkApp stateRef req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) -> do
      currentState <- Strict.readIORef stateRef
      respond $ htmlResponse (mainPage currentState)
    ("POST", ["step", "alice-keygen"]) -> do
      ctx <- runSimulatorT stateRef executeAliceKeygen
      respond $ htmlResponse $ renderStep ctx "Alice generated Ed25519 keypair"
    ("POST", ["step", "bob-keygen"]) -> do
      ctx <- runSimulatorT stateRef executeBobKeygen
      respond $ htmlResponse $ renderStep ctx "Bob generated Ed25519 keypair"
    ("POST", ["step", "alice-generate-secret"]) -> do
      ctx <- runSimulatorT stateRef executeAliceGenerateSecret
      respond $ htmlResponse $ renderStep ctx "Alice generated adapter secret y"
    ("POST", ["step", "alice-make-commitment"]) -> do
      maybeCtx <- runSimulatorT stateRef executeAliceMakeCommitment
      case maybeCtx of
        Just ctx -> respond $ htmlResponse $ renderStep ctx "Alice computed commitment Y = y·B"
        Nothing ->
          respond $
            responseLBS
              status400
              [("Content-Type", "text/plain")]
              "Precondition not met: adapter secret not set"
    ("POST", ["reset"]) -> do
      Strict.writeIORef stateRef emptySimulatorState
      currentState <- Strict.readIORef stateRef
      respond $ htmlResponse (mainPage currentState)
    ("GET", ["static", fileName]) ->
      respond $
        responseFile
          status200
          [("Content-Type", "text/css")]
          ("static/" <> toString fileName)
          Nothing
    _ ->
      respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"
  where
    htmlResponse html =
      responseLBS
        status200
        [("Content-Type", "text/html; charset=utf-8")]
        (renderBS html)

main :: IO ()
main = do
  putTextLn "Starting Atomic Swap Simulator..."
  putTextLn "Visit http://localhost:8888"
  stateRef <- Strict.newIORef $! emptySimulatorState
  run 8888 (mkApp stateRef)

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
import AtomicSwap.Simulator.Render
  ( mainPage
  , renderNewTimelineEntry
  , renderStateUpdates
  )
import AtomicSwap.Simulator.Run (runSimulatorT)
import AtomicSwap.Simulator.State
  ( SimulatorState
  , detectChangedParties
  , emptySimulatorState
  )
import AtomicSwap.Simulator.Steps
  ( executeAliceGenerateSecret
  , executeAliceKeygen
  , executeAliceMakeCommitment
  , executeAliceSendPublicKey
  , executeBobKeygen
  , executeBobSendPublicKey
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
      oldState <- Strict.readIORef stateRef
      runSimulatorT stateRef executeAliceKeygen
      newState <- Strict.readIORef stateRef
      let changedParties = detectChangedParties oldState newState
      respond $ htmlResponse do
        renderNewTimelineEntry newState
        renderStateUpdates changedParties newState
    ("POST", ["step", "bob-keygen"]) -> do
      oldState <- Strict.readIORef stateRef
      runSimulatorT stateRef executeBobKeygen
      newState <- Strict.readIORef stateRef
      let changedParties = detectChangedParties oldState newState
      respond $ htmlResponse do
        renderNewTimelineEntry newState
        renderStateUpdates changedParties newState
    ("POST", ["step", "alice-generate-secret"]) -> do
      oldState <- Strict.readIORef stateRef
      runSimulatorT stateRef executeAliceGenerateSecret
      newState <- Strict.readIORef stateRef
      let changedParties = detectChangedParties oldState newState
      respond $ htmlResponse do
        renderNewTimelineEntry newState
        renderStateUpdates changedParties newState
    ("POST", ["step", "alice-make-commitment"]) -> do
      oldState <- Strict.readIORef stateRef
      success <- runSimulatorT stateRef executeAliceMakeCommitment
      if success
        then do
          newState <- Strict.readIORef stateRef
          let changedParties = detectChangedParties oldState newState
          respond $ htmlResponse do
            renderNewTimelineEntry newState
            renderStateUpdates changedParties newState
        else
          respond $
            responseLBS
              status400
              [("Content-Type", "text/plain")]
              "Precondition not met: adapter secret not set"
    ("POST", ["step", "alice-send-public-key"]) -> do
      oldState <- Strict.readIORef stateRef
      success <- runSimulatorT stateRef executeAliceSendPublicKey
      if success
        then do
          newState <- Strict.readIORef stateRef
          let changedParties = detectChangedParties oldState newState
          respond $ htmlResponse do
            renderNewTimelineEntry newState
            renderStateUpdates changedParties newState
        else
          respond $
            responseLBS
              status400
              [("Content-Type", "text/plain")]
              "Precondition not met: Alice has no public key"
    ("POST", ["step", "bob-send-public-key"]) -> do
      oldState <- Strict.readIORef stateRef
      success <- runSimulatorT stateRef executeBobSendPublicKey
      if success
        then do
          newState <- Strict.readIORef stateRef
          let changedParties = detectChangedParties oldState newState
          respond $ htmlResponse do
            renderNewTimelineEntry newState
            renderStateUpdates changedParties newState
        else
          respond $
            responseLBS
              status400
              [("Content-Type", "text/plain")]
              "Precondition not met: Bob has no public key"
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
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        , ("Pragma", "no-cache")
        , ("Expires", "0")
        ]
        (renderBS html)

main :: IO ()
main = do
  putTextLn "Starting Atomic Swap Simulator..."
  putTextLn "Visit http://localhost:8888"
  stateRef <- Strict.newIORef $! emptySimulatorState
  run 8888 (mkApp stateRef)

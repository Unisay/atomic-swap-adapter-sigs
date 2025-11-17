{- |
Module: Main
Description: Atomic Swap Simulator Web Server

This executable runs the interactive atomic swap simulator web interface.
Users can step through the protocol execution with real-time state visualization.
-}
module Main (main) where

import Lucid (Html, renderBS)
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai
  ( Application
  , Response
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
import AtomicSwap.Simulator.Run (SimulatorT, runSimulatorT)
import AtomicSwap.Simulator.State
  ( SimulatorState
  , detectChangedParties
  , emptySimulatorState
  )
import AtomicSwap.Simulator.Steps
  ( StepResult (..)
  , executeAliceGenerateNIZKProof
  , executeAliceGenerateSecret
  , executeAliceKeygen
  , executeAliceMakeCommitment
  , executeAliceSendCommitment
  , executeAliceSendNIZKProof
  , executeAliceSendPublicKey
  , executeBobKeygen
  , executeBobSendPublicKey
  , executeBobVerifyNIZKProof
  )
import Data.IORef.Strict (StrictIORef)
import Data.IORef.Strict qualified as Strict

--------------------------------------------------------------------------------
-- HTML Response Helper --------------------------------------------------------

htmlResponse :: Html () -> Response
htmlResponse html =
  responseLBS
    status200
    [ ("Content-Type", "text/html; charset=utf-8")
    , ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
    , ("Pragma", "no-cache")
    , ("Expires", "0")
    ]
    (renderBS html)

--------------------------------------------------------------------------------
-- State-Diffing Combinator ----------------------------------------------------

{- | Execute a step handler with automatic state-diffing and rendering

This combinator encapsulates the state-diffing pattern:
1. Read old state
2. Execute handler
3. Read new state (if succeeded)
4. Detect changed parties
5. Render timeline + changed panels (or error response)

All handlers return StepResult for uniform interface.
-}
executeWithStateDiff
  :: StrictIORef SimulatorState
  -> SimulatorT StepResult
  -> (Response -> IO a)
  -> IO a
executeWithStateDiff stateRef handler respond = do
  oldState <- Strict.readIORef stateRef
  result <- runSimulatorT stateRef handler
  case result of
    StepOk -> do
      newState <- Strict.readIORef stateRef
      let changedParties = detectChangedParties oldState newState
      respond $ htmlResponse do
        renderNewTimelineEntry newState
        renderStateUpdates changedParties newState
    StepFailed errorMsg ->
      respond $
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          ("Precondition not met: " <> encodeUtf8 errorMsg)

--------------------------------------------------------------------------------
-- Server Implementation -------------------------------------------------------

mkApp :: StrictIORef SimulatorState -> Application
mkApp stateRef req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) -> do
      currentState <- Strict.readIORef stateRef
      respond $ htmlResponse (mainPage currentState)
    ("POST", ["step", "alice-keygen"]) ->
      executeWithStateDiff stateRef executeAliceKeygen respond
    ("POST", ["step", "bob-keygen"]) ->
      executeWithStateDiff stateRef executeBobKeygen respond
    ("POST", ["step", "alice-generate-secret"]) ->
      executeWithStateDiff stateRef executeAliceGenerateSecret respond
    ("POST", ["step", "alice-make-commitment"]) ->
      executeWithStateDiff stateRef executeAliceMakeCommitment respond
    ("POST", ["step", "alice-send-public-key"]) ->
      executeWithStateDiff stateRef executeAliceSendPublicKey respond
    ("POST", ["step", "bob-send-public-key"]) ->
      executeWithStateDiff stateRef executeBobSendPublicKey respond
    ("POST", ["step", "alice-send-commitment"]) ->
      executeWithStateDiff stateRef executeAliceSendCommitment respond
    ("POST", ["step", "alice-generate-nizk-proof"]) ->
      executeWithStateDiff stateRef executeAliceGenerateNIZKProof respond
    ("POST", ["step", "alice-send-nizk-proof"]) ->
      executeWithStateDiff stateRef executeAliceSendNIZKProof respond
    ("POST", ["step", "bob-verify-nizk-proof"]) ->
      executeWithStateDiff stateRef executeBobVerifyNIZKProof respond
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

main :: IO ()
main = do
  putTextLn "Starting Atomic Swap Simulator..."
  putTextLn "Visit http://localhost:8888"
  stateRef <- Strict.newIORef $! emptySimulatorState
  run 8888 (mkApp stateRef)

{- |
Module: Main
Description: Atomic Swap Simulator Web Server

This executable runs the interactive atomic swap simulator web interface.
Users can step through the protocol execution with real-time state visualization.
-}
module Main (main) where

import Lucid (Html, renderBS)
import Network.HTTP.Types (status200, status204, status400, status404)
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
  , mkSimulatorState
  , resetToStep
  )
import AtomicSwap.Simulator.Steps
  ( StepResult (..)
  , executeAliceCompleteSignature
  , executeAliceCreatePreSignature
  , executeAliceGenerateNIZKProof
  , executeAliceGenerateSecret
  , executeAliceKeygen
  , executeAliceMakeCommitment
  , executeAlicePrepareTransaction
  , executeAlicePublishPreSignature
  , executeAliceSendCommitment
  , executeAliceSendNIZKProof
  , executeAliceSendPublicKey
  , executeAliceVerifyBobPreSignature
  , executeBobCompleteSignature
  , executeBobCreatePreSignature
  , executeBobExtractSecret
  , executeBobKeygen
  , executeBobPrepareTransaction
  , executeBobPublishPreSignature
  , executeBobSendPublicKey
  , executeBobVerifyAlicePreSignature
  , executeBobVerifyNIZKProof
  )
import AtomicSwap.Simulator.Types (Asset (..), Quantity (..), StepIndex (..))
import Data.IORef.Strict (StrictIORef)
import Data.IORef.Strict qualified as Strict
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Random (randomRIO)

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
    ("POST", ["step", "alice-prepare-transaction"]) ->
      executeWithStateDiff stateRef executeAlicePrepareTransaction respond
    ("POST", ["step", "alice-create-pre-signature"]) ->
      executeWithStateDiff stateRef executeAliceCreatePreSignature respond
    ("POST", ["step", "alice-publish-pre-signature"]) ->
      executeWithStateDiff stateRef executeAlicePublishPreSignature respond
    ("POST", ["step", "bob-verify-alice-pre-signature"]) ->
      executeWithStateDiff stateRef executeBobVerifyAlicePreSignature respond
    ("POST", ["step", "bob-prepare-transaction"]) ->
      executeWithStateDiff stateRef executeBobPrepareTransaction respond
    ("POST", ["step", "bob-create-pre-signature"]) ->
      executeWithStateDiff stateRef executeBobCreatePreSignature respond
    ("POST", ["step", "bob-publish-pre-signature"]) ->
      executeWithStateDiff stateRef executeBobPublishPreSignature respond
    ("POST", ["step", "alice-verify-bob-pre-signature"]) ->
      executeWithStateDiff stateRef executeAliceVerifyBobPreSignature respond
    ("POST", ["step", "alice-complete-signature"]) ->
      executeWithStateDiff stateRef executeAliceCompleteSignature respond
    ("POST", ["step", "bob-extract-secret"]) ->
      executeWithStateDiff stateRef executeBobExtractSecret respond
    ("POST", ["step", "bob-complete-signature"]) ->
      executeWithStateDiff stateRef executeBobCompleteSignature respond
    ("POST", ["reset"]) -> do
      -- Generate new random amounts for the swap (ensuring they're different)
      (apples, bananas) <- generateDifferentAmounts
      Strict.writeIORef stateRef (mkSimulatorState apples bananas)
      currentState <- Strict.readIORef stateRef
      respond $ htmlResponse (mainPage currentState)
    ("POST", ["reset-to-step", stepIdxText]) -> do
      -- Parse step index
      case TR.decimal stepIdxText of
        Right (idx, "") -> do
          currentState <- Strict.readIORef stateRef
          let newState = resetToStep (StepIndex idx) currentState
          Strict.writeIORef stateRef newState
          respond $ htmlResponse (mainPage newState)
        _ ->
          respond $
            responseLBS
              status400
              [("Content-Type", "text/plain")]
              "Invalid step index"
    ("GET", ["static", "fonts", fileName]) ->
      let contentType = case fileName of
            _ | ".woff2" `T.isSuffixOf` fileName -> "font/woff2"
            _ | ".woff" `T.isSuffixOf` fileName -> "font/woff"
            _ -> "application/octet-stream"
       in respond $
            responseFile
              status200
              [("Content-Type", contentType)]
              ("static/fonts/" <> toString fileName)
              Nothing
    ("GET", ["static", "icons", fileName]) ->
      respond $
        responseFile
          status200
          [("Content-Type", "image/svg+xml")]
          ("static/icons/" <> toString fileName)
          Nothing
    ("GET", ["static", fileName]) ->
      respond $
        responseFile
          status200
          [("Content-Type", "text/css")]
          ("static/" <> toString fileName)
          Nothing
    ("GET", ["favicon.ico"]) ->
      respond $ responseLBS status204 [] ""
    _ ->
      respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | Generate two different random amounts
generateDifferentAmounts :: IO (Quantity 'Apple, Quantity 'Banana)
generateDifferentAmounts = do
  applesInt <- randomRIO (2, 100 :: Int)
  bananasInt <- randomRIO (2, 100 :: Int)
  if applesInt == bananasInt
    then generateDifferentAmounts -- Retry if equal
    else
      pure
        ( Quantity @'Apple (fromIntegral applesInt)
        , Quantity @'Banana (fromIntegral bananasInt)
        )

main :: IO ()
main = do
  putTextLn "Starting Atomic Swap Simulator..."
  putTextLn "Visit http://localhost:8888"
  -- Generate random swap amounts (2-100 range, ensuring they're different)
  (apples, bananas) <- generateDifferentAmounts
  putTextLn $
    "Swap agreement: Alice offers "
      <> show apples
      <> " apples for Bob's "
      <> show bananas
      <> " bananas"
  stateRef <- Strict.newIORef $! mkSimulatorState apples bananas
  run 8888 (mkApp stateRef)

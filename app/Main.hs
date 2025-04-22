{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Web.Scotty
import QuizData ( Question(..), initialQuestions )
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Data.IORef
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ( object, (.=), FromJSON, decode )
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400, status500)
import Control.Monad (when)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Encoding (decodeUtf8')
import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as BS8
import Network.Wai (requestHeaders)

instructionHTML :: Text
instructionHTML = "<!DOCTYPE html><html><head><title>Haiku Quiz API</title><style>body{font-family:sans-serif;line-height:1.6;background-color:#f0f0f0;color:#333;max-width:600px;margin:2em auto;padding:1em;border:1px solid #ccc;border-radius:5px}code{background-color:#e8e8e8;padding:0.2em 0.4em;border-radius:3px;font-family:monospace}footer{text-align:center;margin-top:1em;font-size:0.8em;font-style:italic;color:#aaa}a{color:#333}</style></head><body><h1>🌸 Haiku Quiz API</h1><p>Browser access not intended.</p><p><em>Please use <code>curl</code> or another API client.</em></p><hr><p><strong>Endpoints:</strong></p><ul><li><code>GET /question</code> - Get a new haiku question.</li><li><code>POST /answer</code> - Submit answer (JSON: <code>{'submittedAnswer': 'guess'}</code>).</li><li><code>POST /reset</code> - Reset the current question state.</li></ul><hr><p>Found on <a href='https://github.com/philsrfn/port575'>GitHub</a></p></body><footer>Made by <a href='https://github.com/philsrfn'>Phil Serafin</a></footer></html>"

isBrowser :: ActionM Bool
isBrowser = do
    uaHeader <- lookup "User-Agent" . requestHeaders <$> request
    let browserKeywords = ["Mozilla", "Chrome", "Safari", "Firefox", "Edge", "Opera"]
    -- Check if any keyword is present in the decoded User-Agent string
    return $ case decodeUtf8' <$> uaHeader of
               Just (Right uaText) -> any (`isInfixOf` T.unpack uaText) browserKeywords
               _                   -> False -- Treat missing header or decoding error as non-browser

-- Helper to run API action or serve HTML based on User-Agent
handleRequest :: ActionM () -> ActionM ()
handleRequest apiAction = do
    browser <- isBrowser
    if browser
        then html instructionHTML
        else apiAction

data AnswerRequest = AnswerRequest { submittedAnswer :: T.Text } deriving (Show, Generic)
instance FromJSON AnswerRequest
type AppState = IORef (Maybe Question)
main :: IO ()
main = do
  when (null initialQuestions) $ do
    putStrLn "Error: No questions defined in QuizData.hs! Exiting."
    fail "Initial question list is empty."
  putStrLn "Starting Haiku Quiz Server on port 3000..."
  appState <- newIORef Nothing
  scotty 3000 (app appState)
app :: AppState -> ScottyM ()
app appState = do
  middleware logStdoutDev

  -- GET /: Serve HTML to browsers, JSON otherwise
  get "/" $ handleRequest $ 
    json $ object ["message" .= ("Welcome to the Haiku Quiz! GET /question for a challenge." :: String)]

  -- GET /question: Serve HTML to browsers, JSON otherwise
  get "/question" $ handleRequest $ do
    when (null initialQuestions) $ do 
        status status500
        json $ object ["error" .= ("Server error: No questions available." :: String)]
        finish
    idx <- liftIO $ randomRIO (0, length initialQuestions - 1)
    let selectedQ = initialQuestions !! idx
    liftIO $ writeIORef appState (Just selectedQ)
    json $ object ["haiku" .= haiku selectedQ]

  -- POST /answer: Serve HTML to browsers, JSON otherwise
  post "/answer" $ handleRequest $ do
    reqBody <- body
    case decode reqBody :: Maybe AnswerRequest of
      Nothing -> do
        status status400
        json $ object ["error" .= ("Invalid JSON body. Expected {\"submittedAnswer\": \"your_guess\"}" :: String)]
      Just (AnswerRequest submittedAns) -> do 
        let submittedAnsText = T.toLower submittedAns
        maybeCurrentQ <- liftIO $ readIORef appState
        case maybeCurrentQ of
          Nothing -> do
            status status400
            json $ object ["error" .= ("No question active. Please GET /question first." :: String)]
          Just currentQ -> do
            let correctAnswer = T.toLower $ answer currentQ
            let responseHaiku = if submittedAnsText == correctAnswer
                                  then onCorrect currentQ
                                  else onWrong currentQ
            liftIO $ writeIORef appState Nothing 
            json $ object ["result" .= responseHaiku]

  -- POST /reset: Serve HTML to browsers, JSON otherwise
  post "/reset" $ handleRequest $ 
    liftIO (writeIORef appState Nothing) >> json (object ["message" .= ("Quiz state reset." :: String)])

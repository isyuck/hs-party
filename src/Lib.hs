{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( main
  ) where

import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( forever
                                                , liftM2
                                                , void
                                                )
import qualified Data.Text                     as T
import           Network.WebSockets             ( ClientApp
                                                , receiveData
                                                , sendTextData
                                                )
import           Network.WebSockets.Connection  ( Connection )
import           Sound.OSC.FD                  as O
import           Wuss

channel = "YOUR CHAN"
botuser = "YOUR BOT"
oauth = "YOUR OAUTH"

data TwitchMessage = TwitchMessage
  { username :: String
  , userid   :: String -- TODO int
  , content  :: String
  }
  deriving Show

main :: IO ()
main = runSecureClient "irc-ws.chat.twitch.tv" 443 "/" ws

-- format and send a String to twitch
sendChatMessage :: Network.WebSockets.Connection.Connection -> String -> IO ()
sendChatMessage c m = do
  sendTextData c $ T.pack ("PRIVMSG #" ++ channel ++ " :" ++ m)

-- format and send a TwitchMessage out over osc, with the "/code" path
sendOscCode :: Transport t => t -> TwitchMessage -> IO ()
sendOscCode r m = sendMessage r
  $ Message "/code" [string $ username m, string $ drop 3 $ content m]

-- wrap some twitch init stuff
initalise :: Network.WebSockets.Connection.Connection -> IO ()
initalise c = do
  putStrLn "connected to twitch!"
  mapM_
    (sendTextData c . T.pack)
    [ "PASS " ++ oauth
    , "NICK " ++ botuser
    , "JOIN #" ++ channel
    , "CAP REQ :twitch.tv/tags"
    ]
  sendChatMessage c "tidal-party haskell has entered"

-- turn the raw received message from the twitch api into a
-- list of T.Texts we actually care about. what we actually
-- care about is defined in 'wanted'
filterTags :: T.Text -> Maybe [T.Text]
filterTags tags = if (length ft >= 3) then Just ft else Nothing
 where
  ft =
    map (T.stripStart . T.drop 1 . T.dropWhile (/= '=') . snd)
      $ filter (\(w, t) -> T.isInfixOf w t)
      $ liftM2 (,) wanted
      $ T.split (== ';') tags
  wanted = ["user-type", "display-name", "user-id"]

-- turn the raw received message from the twitch api into a TwitchMessage
-- NOTE that this depends on the order that twitch sends its return messages
parseTwitchMessage :: T.Text -> Maybe TwitchMessage
parseTwitchMessage t = case filterTags t of
  Nothing       -> Nothing
  Just filtered -> Just $ TwitchMessage un uid msg
   where
    msg =
      T.unpack $ T.dropEnd 2 $ T.drop 2 $ snd $ T.breakOn " :" $ filtered !! 0
    un  = T.unpack $ filtered !! 1
    uid = T.unpack $ filtered !! 2

ws :: ClientApp ()
ws twitch = do

  initalise twitch
  oscIn  <- udpServer "127.0.0.1" 6012
  oscOut <- openUDP "127.0.0.1" 6011

  sendMessage oscOut $ Message "/ping" []

  -- parse messages from twitch and send them to tidal-listener
  void . forkIO . forever $ do
    latest <- receiveData twitch
    case parseTwitchMessage latest of
      Just parsed -> do
        -- commands
        case takeWhile (/= ' ') $ content parsed of
          "!t"      -> sendOscCode oscOut parsed
          "!getcps" -> sendMessage oscOut $ Message "/cps" []
          _         -> return ()
      Nothing -> return ()

  -- handle received osc from tidal-listener
  void . forkIO . forever $ do
    latest   <- recvMessage oscIn
    received <- case latest of
      -- pong case
      Just (Message "/pong" []) ->
        return $ Just "received pong from tidal-listener!"

      -- cps case
      Just (Message "/cps" [Float cps]) ->
        return $ Just ("the cps is " ++ show cps)

      -- error case
      Just (Message "/code/error" [ASCII_String a_ident, ASCII_String a_code])
        -> return
          $  Just
          $  "@"
          ++ (ascii_to_string a_ident)
          ++ " error! "
          ++ (ascii_to_string a_code)

      -- TODO whatever else it can send back
      Nothing -> return Nothing
      _       -> return Nothing

    case received of
      Just x  -> sendChatMessage twitch x
      Nothing -> return ()

  let loop = do
        _ <- getLine
        loop
  loop

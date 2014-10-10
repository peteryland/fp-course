module Network.Server.Chat.Chat where

import Network.Server.Common.Line
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Data.IORef(atomicModifyIORef)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))

type Chat a =
  IORefLoop Integer a

data ChatCommand =
  Chat String
  | Incr
  | Add String
  | Unknown String
  deriving (Eq, Show)

incr ::
  Chat Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef e (\n -> (n + 1, n + 1))

add ::
  String ->
  Chat Integer
add i =
  do e <- readEnvval
     let val = read i in liftIO $ atomicModifyIORef e (\n -> (n + val, n + val))

chat ::
  IO a
chat =
  iorefLoop 0 (return ()) (process . chatCommand)

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand ::
  String
  -> ChatCommand
chatCommand z =
  Unknown z `fromMaybe` msum [
                               Chat <$> trimPrefixThen "CHAT" z
                             , Add <$> trimPrefixThen "ADD" z
                             , Incr <$ trimPrefixThen "INCR" z
                             ]

process ::
  ChatCommand
  -> Chat ()
process cmd =
  case cmd of
    Chat s -> do allClientsButThis ! "> " ++ s; return ()
    Incr -> do x <- incr; allClients ! "> counter is at " ++ show x; return ()
    Add i -> do x <- add i; allClients ! "> counter is at " ++ show x; return ()
    Unknown x -> do pPutStrLn $ "> huh? Don't understand: " ++ x; return ()

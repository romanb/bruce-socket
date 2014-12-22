{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE RecordWildCards    #-}

-- | A unified socket API for sending and receiving @Bruce@ protocol messages
-- through UNIX domain datagram sockets or TCP connections.
--
-- In the standard case you just want to send @Bruce@ messages to a local
-- @Bruce@ daemon via its UNIX domain datagram socket, e.g.:
--
-- @
--   sock <- datagramSocket defaultSocketPath defaultBufferSize
--   connect sock
--   send sock (Message ...)
--   close sock
-- @
module Network.Bruce.Socket
    ( -- * Creating Sockets
      Socket
    , defaultSocketPath
    , defaultBufferSize
    , datagramSocket
    , streamSocket
    , close

      -- * Client
    , connect
    , send

      -- * Server
    , bind
    , listen
    , accept
    , recv
    , ParseException (..)

      -- * Protocol Types
    , module P
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad (void)
import Data.ByteString.Builder
import Data.Monoid
import Data.Serialize
import Data.Typeable
import Network.Bruce.Protocol as P
import Network.Bruce.Socket.Peek

import qualified Data.ByteString           as BS
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB

-- | A socket for sending and receiving @Bruce@ protocol messages.
data Socket
    = DatagramSocket
        { _addr :: !N.SockAddr
        , _sock :: !N.Socket
        }
    | StreamSocket
        { _addr :: !N.SockAddr
        , _sock :: !N.Socket
        }

-- | A common socket path for 'datagramSocket's:
-- @\/var\/run\/bruce\/bruce.socket@
defaultSocketPath :: FilePath
defaultSocketPath = "/var/run/bruce/bruce.socket"

-- | A common buffer size for 'datagramSocket's: @212992@
defaultBufferSize :: Int
defaultBufferSize = 212992

-- | @datagramSocket p n@ creates a new @Bruce@ UNIX domain datagram 'Socket'
-- with a send and receive buffer of size @n@ which thus also determines the
-- maximum size of datagrams that can be sent or received through the socket.
datagramSocket :: FilePath -> Int -> IO Socket
datagramSocket fp buf = do
    sock <- N.socket N.AF_UNIX N.Datagram N.defaultProtocol
    N.setSocketOption sock N.SendBuffer buf
    return $! DatagramSocket (N.SockAddrUnix fp) sock

-- | @streamSocket h p@ creates a new @Bruce@ TCP 'Socket'.
streamSocket :: N.HostName -> N.PortNumber -> IO Socket
streamSocket host (N.PortNum port) = do
    N.AddrInfo{..} <- head <$> resolve
    sock <- N.socket addrFamily addrSocketType addrProtocol
    return $! StreamSocket addrAddress sock
  where
    resolve = N.getAddrInfo (Just hints) (Just host) (Just $ show port)
    hints   = N.defaultHints
            { N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
            , N.addrFamily     = N.AF_INET
            , N.addrSocketType = N.Stream
            }

-- | Close a @Bruce@ 'Socket'.
close :: Socket -> IO ()
close = N.close . _sock

-------------------------------------------------------------------------------
-- Client

-- | Connect to a @Bruce@ protocol server through the given 'Socket'.
connect :: Socket -> IO ()
connect s = N.connect (_sock s) (_addr s)

-- | Send a @Bruce@ 'Message' to a server through the given 'Socket'.
send :: Socket -> Message -> IO ()
send (DatagramSocket _ s) = void . NB.send s . encode
send (StreamSocket   _ s) = NB.sendAll s . encode

-------------------------------------------------------------------------------
-- Server

-- | Bind a @Bruce@ 'Socket' to its configured address.
bind :: Socket -> IO ()
bind s = N.bind (_sock s) (_addr s)

-- | Listen for connections on a @Bruce@ 'Socket'.
--
-- /Note: This is a no-op for datagram sockets./
listen :: Socket -> Int -> IO ()
listen (DatagramSocket _ _) _ = return ()
listen (StreamSocket   _ s) n = N.listen s n

-- | Accept a connection on a @Bruce@ 'Socket'.
--
-- /Note: This is just @return@ for datagram sockets./
accept :: Socket -> IO Socket
accept s@DatagramSocket{} = return s
accept (StreamSocket _ s) = do
    (sock, addr) <- N.accept s
    return $! StreamSocket addr sock

-- | Receive a single 'Message' from the given 'Socket'.
-- When the decoding of a message fails, a 'ParseException' is thrown.
recv :: Socket -> IO Message
recv (DatagramSocket _  sock) = do
    size <- parse (runGet decodeSize) =<< peek sock 4
    body <- BS.drop 4 <$> NB.recv sock (fromIntegral size + 4)
    parse (runGet decodeBody) body
recv (StreamSocket addr sock) = do
    size <- parse (runGet decodeSize) =<< NB.recv sock 4
    body <- consume (fromIntegral size) mempty
    parse (runGetLazy decodeBody) body
  where
    consume n bb = do
        bs <- NB.recv sock (min n 4096)
        let len = BS.length bs
        let bs' = bb <> byteString bs
        if | len == 0  -> throwIO $ ConnectionClosed addr
           | len == n  -> return  $! toLazyByteString bs'
           | otherwise -> consume (n - len) bs'

parse :: (a -> Either String b) -> a -> IO b
parse f = either (throwIO . ParseException) return . f

newtype ParseException = ParseException String
    deriving (Eq, Typeable)

instance Exception ParseException

instance Show ParseException where
    show (ParseException e) = "bruce-socket: failed to parse: " ++ e

data ConnectionClosed = ConnectionClosed !N.SockAddr
    deriving (Eq, Typeable)

instance Exception ConnectionClosed

instance Show ConnectionClosed where
    show (ConnectionClosed a) = "bruce-socket: connection closed: " ++ show a

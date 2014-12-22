{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Bruce.Socket.Peek (peek) where

import Data.ByteString.Internal
import Foreign hiding (peek)
import Foreign.C.Types
import GHC.IO.Exception (IOErrorType (..))
import Network.Socket
import Network.Socket.Internal
import System.IO.Error (ioeSetErrorString, mkIOError)

#include <sys/socket.h>

foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

fMSG_PEEK :: CInt
fMSG_PEEK = #const MSG_PEEK

peek :: Socket -> Int -> IO ByteString
peek sock nbytes
    | nbytes < 0 = ioError (mkInvalidRecvArgError "Network.Bruce.Socket.Datagram.peek")
    | otherwise  = createAndTrim nbytes $ peekInner sock nbytes

peekInner :: Socket -> Int -> Ptr Word8 -> IO Int
peekInner sock nbytes ptr =
    fmap fromIntegral $
        throwSocketErrorWaitRead sock "recv" $
        c_recv fd (castPtr ptr) (fromIntegral nbytes) fMSG_PEEK
  where
    fd = fdSocket sock

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString
    (mkIOError InvalidArgument loc Nothing Nothing)
    "non-positive length"

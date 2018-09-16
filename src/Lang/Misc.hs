module Lang.Misc (toShortBS,toString,toString') where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS

toShortBS :: String -> BS.ShortByteString
toShortBS =  BS.toShort . BC.pack

toString :: BS.ShortByteString -> String
toString = BC.unpack . BS.fromShort

toString' :: BC.ByteString -> String
toString' = BC.unpack
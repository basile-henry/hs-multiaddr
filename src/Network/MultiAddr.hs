{-# LANGUAGE LambdaCase #-}

{-|
Module      : Network.MultiAddr
Description : Network address format
Copyright   : (c) 2016 Basile Henry
License     : MIT
Maintainer  : basile.henry@hotmail.com
Stability   : experimental

MultiAddr is the implemenation of the network address format described by jbenet <https://github.com/jbenet/multiaddr>

MultiAddr is a standard way to represent addresses that:

    * support any standard network protocols
    * self-describe (include protocols)
    * have a binary packed format
    * have a nice string representation
    * encapsulate well

For example, to get a MultiAddr from a string:

@
let multiaddr = fromJust $ fromString "\/ip4\/127.0.0.1\/tcp\/4001\/ipfs\/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8"
@

Then to encode it to the binary packed format:

@
let binaryFormat = encode multiaddr
@

To extract the address of the IPv4 (ip4) protocol as a 'String' from multiaddr:

@
let address = fromJust $ findAddressString IP4 multiaddr
@

-}
module Network.MultiAddr (
    MultiAddr,
    Address,
    Protocol (..),
    Name,
    Code,
    Size (..),
    -- | = As a string
    fromString,
    toString,
    -- | = As a binary packed format
    encode,
    decode,
    -- | = Utils
    getName,
    getCode,
    getSize,
    fromName,
    fromCode,
    findAddress,
    findAddressString
) where

import           Data.Bytes.Get         (getByteString, isEmpty, runGetS)
import           Data.Bytes.Put         (runPutS)
import           Data.Bytes.Serial      (deserialize, serialize)
import           Data.Bytes.VarInt      (VarInt (..))
import           Data.ByteString        (ByteString, append, concat, empty,
                                         length, pack, unpack)
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58,
                                         encodeBase58)
import qualified Data.ByteString.UTF8   as UTF8
import           Data.Char              (toLower)
import           Data.IP                (fromIPv4, fromIPv6b, toIPv4, toIPv6b)
import           Data.List              (find)
import           Data.List.Split        (splitOn)
import           Data.Serialize.Get     (Get)
import           Data.Word              (Word16)
import           Prelude                hiding (concat, length)

-- | A self describing network address
type MultiAddr = [(Protocol, Address)]

-- | A network protocol
data Protocol  = IP4
               | TCP
               | UDP
               | DCCP
               | IP6
               | SCTP
               | UDT
               | UTP
               | IPFS
               | HTTP
               | HTTPS
               | WebSockets
               -- Onion
               deriving (Eq, Show)

-- | Data that goes with a 'Protocol'
type Address   = ByteString

-- | Name of a 'Protocol'
type Name      = String

-- | Code of a 'Protocol'
type Code      = Int

-- | Size of a 'Protocol'
--
-- A size is either variable or a given number of bits
data Size      = Variable
               | Size Int
               deriving (Eq, Show)

parseAddress :: Protocol -> String -> Maybe Address
parseAddress IP4  = (pack . map fromIntegral . fromIPv4 <$>) . readMaybe
parseAddress TCP  = (fromPort <$>) . readMaybe
parseAddress UDP  = (fromPort <$>) . readMaybe
parseAddress DCCP = (fromPort <$>) . readMaybe
parseAddress IP6  = (pack . map fromIntegral . fromIPv6b <$>) . readMaybe
parseAddress SCTP = (fromPort <$>) . readMaybe
parseAddress IPFS = (putVarAddrBytes <$>) . decodeBase58 bitcoinAlphabet . UTF8.fromString
parseAddress _    = \_ -> Nothing

-- | Parse a 'String' to its corresponding 'MultiAddr'
--
-- If invalid input returns 'Nothing'
fromString :: String -> Maybe MultiAddr
fromString ('/':cs) = sequence . fromString' . filter (/= "") . splitOn "/" $ cs
    where
        fromString' :: [String] -> [Maybe (Protocol, ByteString)]
        fromString' []     = []
        fromString' (x:xs) = case fromName x of
            Nothing  -> [Nothing]
            (Just p) -> case (getSize p, xs) of
                (Size 0, _     ) -> Just (p, empty) : fromString' xs
                (_     , []    ) -> [Nothing]
                (_     , (y:ys)) -> sequence (p, parseAddress p y) : fromString' ys
fromString _        = Nothing

showAddress :: Protocol -> Address -> String
showAddress IP4  addr = show . toIPv4 . map fromIntegral . unpack $ addr
showAddress TCP  addr = show . toPort $ addr
showAddress UDP  addr = show . toPort $ addr
showAddress DCCP addr = show . toPort $ addr
showAddress IP6  addr = show . toIPv6b . map fromIntegral . unpack $ addr
showAddress SCTP addr = show . toPort $ addr
showAddress IPFS addr = case runGetS getVarAddrBytes addr of
    (Left  x) -> error x
    (Right x) -> UTF8.toString $ encodeBase58 bitcoinAlphabet x
showAddress _    _    = ""

-- | Given a valid 'MultiAddr', show its 'String' representation
toString :: MultiAddr -> String
toString = concatMap toString'
    where
        toString' :: (Protocol, ByteString) -> String
        toString' (p, a)
            | getSize p == Size 0 = '/' : getName p
            | otherwise           = '/' : getName p ++ '/' : showAddress p a

fromPort :: Word16 -> ByteString
fromPort = runPutS . serialize

toPort :: ByteString -> Word16
toPort bytes = case runGetS deserialize bytes of
    (Left x)  -> error x
    (Right x) -> x

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

toBytes :: (Protocol, Address) -> ByteString
toBytes (p, a) = append protoBytes addrBytes
    where
        protoBytes :: ByteString
        protoBytes = putVar . fromIntegral . getCode $ p

        addrBytes :: ByteString
        addrBytes
            | getSize p == Variable = putVarAddrBytes a
            | otherwise             = a

-- | Given a valid 'MultiAddr', it will be encoded to the corresponding strict 'ByteString'
encode :: MultiAddr -> ByteString
encode = concat . map toBytes

putVar :: VarInt Int -> ByteString
putVar = runPutS . serialize

putVarAddrBytes :: ByteString -> Address
putVarAddrBytes bytes = append (putVar . fromIntegral . length $ bytes) bytes

getVarAddrBytes :: Get Address
getVarAddrBytes = getByteString . fromIntegral =<< (deserialize :: Get (VarInt Int))

addressBytes :: Size -> Get Address
addressBytes (Size x) = getByteString $ fromIntegral $ div x 8
addressBytes Variable = getVarAddrBytes

getProtocol :: Get (Maybe Protocol)
getProtocol = fromCode . fromIntegral <$> (deserialize :: Get (VarInt Int))

fromBytes :: Get (Maybe MultiAddr)
fromBytes = getProtocol >>= \case
        Nothing  -> return Nothing
        (Just p) -> do
            a <- addressBytes $ getSize p
            isEmpty >>= \case
                True -> return $ Just [(p, a)]
                _    -> (((p, a) :) <$>) <$> fromBytes

-- | Tries to decode from a strict 'ByteString' to a 'MultiAddr'
--
-- If invalid input, returns 'Nothing'
decode :: ByteString -> Maybe MultiAddr
decode bytes = case runGetS fromBytes bytes of
    (Left  _) -> Nothing
    (Right x) -> x

-- | To get the name of a 'Protocol' as a 'Name'
getName :: Protocol -> Name
getName = map toLower . show

-- | Get the 'Code' of a 'Protocol'
getCode :: Protocol -> Code
getCode IP4        = 4
getCode TCP        = 6
getCode UDP        = 17
getCode DCCP       = 33
getCode IP6        = 41
getCode SCTP       = 132
getCode UDT        = 301
getCode UTP        = 302
getCode IPFS       = 421
getCode HTTP       = 480
getCode HTTPS      = 443
getCode WebSockets = 477
-- getCode Onion      = 444

-- | To get the 'Size' of a 'Protocol'
getSize :: Protocol -> Size
getSize IP4        = Size 32
getSize TCP        = Size 16
getSize UDP        = Size 16
getSize DCCP       = Size 16
getSize IP6        = Size 128
getSize SCTP       = Size 16
getSize UDT        = Size 0
getSize UTP        = Size 0
getSize IPFS       = Variable
getSize HTTP       = Size 0
getSize HTTPS      = Size 0
getSize WebSockets = Size 0
-- getSize Onion      = Size 10

-- | Get a 'Protocol' from it's 'Name'
fromName :: Name -> Maybe Protocol
fromName "ip4"        = Just IP4
fromName "tcp"        = Just TCP
fromName "udp"        = Just UDP
fromName "dccp"       = Just DCCP
fromName "ip6"        = Just IP6
fromName "sctp"       = Just SCTP
fromName "udt"        = Just UDT
fromName "utp"        = Just UTP
fromName "ipfs"       = Just IPFS
fromName "http"       = Just HTTP
fromName "https"      = Just HTTPS
fromName "websockets" = Just WebSockets
-- fromName "onion"      = Just Onion
fromName _            = Nothing

-- | Get a 'Protocol' from it's 'Code'
fromCode :: Code -> Maybe Protocol
fromCode 4   = Just IP4
fromCode 6   = Just TCP
fromCode 17  = Just UDP
fromCode 33  = Just DCCP
fromCode 41  = Just IP6
fromCode 132 = Just SCTP
fromCode 301 = Just UDT
fromCode 302 = Just UTP
fromCode 421 = Just IPFS
fromCode 480 = Just HTTP
fromCode 443 = Just HTTPS
fromCode 477 = Just WebSockets
-- fromCode 444 = Just Onion
fromCode _   = Nothing

-- | Get the address corresponding to the first occurence of a 'Protocol' in a 'MultiAddr'
findAddress :: Protocol -> MultiAddr -> Maybe ByteString
findAddress proto = (snd <$>) . find ((== proto) . fst)

-- | Get the address corresponding to the first occurence of a 'Protocol' in a 'MultiAddr' as a 'String'
findAddressString :: Protocol -> MultiAddr -> Maybe String
findAddressString proto = (showAddress proto . snd <$>) . find ((== proto) . fst)

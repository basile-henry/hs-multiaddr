{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

{-|
Module      : Network.Multiaddr
Description : Network address format
Copyright   : (c) 2016 Basile Henry
License     : MIT
Maintainer  : basile.henry@hotmail.com
Stability   : experimental

Network.Multiaddr is the implementation of multiaddr, the network address format described by jbenet <https://github.com/jbenet/multiaddr>

multiaddr is a standard way to represent addresses that:

    * support any standard network protocols
    * self-describe (include protocols)
    * have a binary packed format
    * have a nice string representation
    * encapsulate well

For example, to get a 'Multiaddr' from a 'String':

@
let multiaddr = read "\/ip4\/127.0.0.1\/tcp\/4001\/ipfs\/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8" :: Multiaddr
@

Then to encode it to the binary packed format:

@
let binaryFormat = encode multiaddr
@

-}
module Network.Multiaddr (
    -- * Multiaddr types
    Multiaddr (..),
    MultiaddrPart (..),
    Multihash (..),
    -- * As a string
    show,
    read,
    -- * As a binary packed format
    encode,
    decode,
    -- * Utils
    encapsulate,
    decapsulate,
    startsWith
) where

import           Control.Applicative          (many, some, (<|>))
import           Control.Monad                (unless, void)
import           Data.Bytes.Get               (getByteString, runGetS)
import           Data.Bytes.Put               (runPutS)
import           Data.Bytes.Serial            (deserialize, serialize)
import           Data.Bytes.VarInt            (VarInt (..))
import           Data.ByteString              (ByteString, append, pack, unpack)
import qualified Data.ByteString              as B
import           Data.ByteString.Base58       (bitcoinAlphabet, decodeBase58,
                                               encodeBase58)
import qualified Data.ByteString.UTF8         as UTF8
import           Data.IP                      (IPv4, IPv6, fromIPv4, fromIPv6b,
                                               toIPv4, toIPv6b)
import           Data.Serialize.Get           (Get)
import           Data.Word                    (Word16)
import           GHC.Generics                 (Generic)
import           Text.ParserCombinators.ReadP (ReadP, char, readP_to_S,
                                               readS_to_P, string)

-- | A self describing network address
newtype Multiaddr = Multiaddr { parts :: [MultiaddrPart] } deriving (Eq, Generic)

instance Show Multiaddr where
    show (Multiaddr m) = concatMap show m

instance Read Multiaddr where
    readsPrec _ = readP_to_S $ do
        mparts <- some $ readS_to_P reads
        void $ many (char '/')
        return $ Multiaddr mparts

-- | A part of the 'Multiaddr'
data MultiaddrPart = IP4  { ip4  :: IPv4 }
                   | TCP  { tcp  :: Word16 }
                   | UDP  { udp  :: Word16 }
                   | DCCP { dccp :: Word16 }
                   | IP6  { ip6  :: IPv6 }
                   | SCTP { sctp :: Word16 }
                   | UDT
                   | UTP
                   | IPFS { ipfs :: Multihash }
                   | HTTP
                   | HTTPS
                   | WebSockets
                   -- Onion
                   deriving (Eq, Generic)

instance Show MultiaddrPart where
    show (IP4  i)   = "/ip4/"  ++ show i
    show (TCP  p)   = "/tcp/"  ++ show p
    show (UDP  p)   = "/udp/"  ++ show p
    show (DCCP p)   = "/dccp/" ++ show p
    show (IP6  i)   = "/ip6/"  ++ show i
    show (SCTP p)   = "/sctp/" ++ show p
    show UDT        = "/udt"
    show UTP        = "/utp"
    show (IPFS h)   = "/ipfs/" ++ show h
    show HTTP       = "/http"
    show HTTPS      = "/https"
    show WebSockets = "/websockets"

instance Read MultiaddrPart where
    readsPrec _ = readP_to_S $
            parseAddr IP4        "ip4"
        <|> parseAddr TCP        "tcp"
        <|> parseAddr UDP        "udp"
        <|> parseAddr DCCP       "dccp"
        <|> parseAddr IP6        "ip6"
        <|> parseAddr SCTP       "sctp"
        <|> parse     UDT        "udt"
        <|> parse     UTP        "utp"
        <|> parseAddr IPFS       "ipfs"
        <|> parse     HTTP       "http"
        <|> parse     HTTPS      "https"
        <|> parse     WebSockets "websockets"

sep :: ReadP ()
sep = void $ some $ char '/'

protoString :: String -> ReadP ()
protoString s = sep *> void (string s)

readAddr :: Read a => ReadP a
readAddr = readS_to_P reads

parseAddr :: Read a => (a -> MultiaddrPart) -> String -> ReadP MultiaddrPart
parseAddr c s = c <$> (protoString s *> sep *> readAddr)

parse :: MultiaddrPart -> String -> ReadP MultiaddrPart
parse c s = c <$ (protoString s)

-- | An IPFS hash
newtype Multihash = Multihash { multihash :: ByteString } deriving (Eq, Generic)

instance Show Multihash where
    show (Multihash h) = UTF8.toString . encodeBase58 bitcoinAlphabet $ h

instance Read Multihash where
    readsPrec _ =
        (\case
            Nothing -> []
            Just m  -> [(Multihash m, "")])
        . decodeBase58 bitcoinAlphabet
        . UTF8.fromString

-- | Size of a 'MultiaddrPart'
-- A size is either variable or a given number of bits
data Size      = Variable
               | Size Int
               deriving (Eq, Show)

fromPort :: Word16 -> ByteString
fromPort = runPutS . serialize

fromList :: [Int] -> ByteString
fromList = pack . map fromIntegral

putVar :: VarInt Int -> ByteString
putVar = runPutS . serialize

putVarAddrBytes :: ByteString -> ByteString
putVarAddrBytes b = append (putVar . fromIntegral . B.length $ b) b

toBytes :: MultiaddrPart -> ByteString
toBytes (IP4  i)   = append (putVar 4  ) (fromList . fromIPv4 $ i)
toBytes (TCP  p)   = append (putVar 6  ) (fromPort p)
toBytes (UDP  p)   = append (putVar 17 ) (fromPort p)
toBytes (DCCP p)   = append (putVar 33 ) (fromPort p)
toBytes (IP6  i)   = append (putVar 41 ) (fromList . fromIPv6b $ i)
toBytes (SCTP p)   = append (putVar 132) (fromPort p)
toBytes UDT        =         putVar 301
toBytes UTP        =         putVar 302
toBytes (IPFS h)   = append (putVar 421) (putVarAddrBytes $ multihash h)
toBytes HTTP       =         putVar 480
toBytes HTTPS      =         putVar 443
toBytes WebSockets =         putVar 477

-- | Given a valid 'Multiaddr', it will be encoded to the corresponding strict 'ByteString'
encode :: Multiaddr -> ByteString
encode (Multiaddr m) = B.concat . map toBytes $ m

getVarAddrBytes :: Get ByteString
getVarAddrBytes = getByteString . fromIntegral =<< (deserialize :: Get (VarInt Int))

addressBytes :: Size -> Get ByteString
addressBytes (Size x) = getByteString $ fromIntegral $ div x 8
addressBytes Variable = getVarAddrBytes

protoBytes :: VarInt Int -> Get ()
protoBytes i = do
    p <- deserialize :: Get (VarInt Int)
    unless (i == p) $ fail "Wrong protocol code"

bytes :: MultiaddrPart -> VarInt Int -> Get MultiaddrPart
bytes c i = c <$ (protoBytes i)

bytesAddr :: (ByteString -> MultiaddrPart) -> VarInt Int -> Size -> Get MultiaddrPart
bytesAddr c i s = c <$> (protoBytes i *> addressBytes s)

toList :: ByteString -> [Int]
toList = map fromIntegral . unpack

toPort :: ByteString -> Word16
toPort = either error id . runGetS deserialize

-- | Tries to decode from a strict 'ByteString' to a 'Multiaddr'
decode :: ByteString -> Either String Multiaddr
decode = runGetS $
    Multiaddr <$> some
           (bytesAddr (IP4  . toIPv4 . toList)  4   (Size 32 )
        <|> bytesAddr (TCP  . toPort)           6   (Size 16 )
        <|> bytesAddr (UDP  . toPort)           17  (Size 16 )
        <|> bytesAddr (DCCP . toPort)           33  (Size 16 )
        <|> bytesAddr (IP6  . toIPv6b . toList) 41  (Size 128)
        <|> bytesAddr (SCTP . toPort)           132 (Size 16 )
        <|> bytes     UDT                       301
        <|> bytes     UTP                       302
        <|> bytesAddr (IPFS . Multihash)        421 Variable
        <|> bytes     HTTP                      480
        <|> bytes     HTTPS                     443
        <|> bytes     WebSockets                477)

-- | Encapsulate the second 'Multiaddr' by the first 'Multiaddr'
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate a b = Multiaddr $ parts a ++ parts b

-- | Remove the first 'Multiaddr' from the beginning of the second 'Multiaddr'
decapsulate :: Multiaddr -> Multiaddr -> Maybe Multiaddr
decapsulate a b
    | startsWith a b = Just . Multiaddr . drop (length . parts $ a) $ parts b
    | otherwise      = Nothing

-- | Check if the second 'Multiaddr' starts with the first 'Multiaddr'
startsWith :: Multiaddr -> Multiaddr -> Bool
startsWith (Multiaddr []) _ = True
startsWith _ (Multiaddr []) = False
startsWith (Multiaddr (a:as)) (Multiaddr (b:bs))
    | a == b    = startsWith (Multiaddr as) (Multiaddr bs)
    | otherwise = False

-- | Get the address corresponding to the first occurence of a 'Protocol' in a 'Multiaddr'
-- findAddress :: Protocol -> Multiaddr -> Maybe ByteString
-- findAddress proto (Multiaddr m) = (snd <$>) . find ((== proto) . fst) $ m

-- | Get the address corresponding to the first occurence of a 'Protocol' in a 'Multiaddr' as a 'String'
-- findAddressString :: Protocol -> Multiaddr -> Maybe String
-- findAddressString proto (Multiaddr m) = (showAddress proto . snd <$>) . find ((== proto) . fst) $ m


||| Types for the ZRTP Packet Format |||

-- The sequence number is incremented for each packet sent
data SeqNum : Nat -> Type where
  SN : (n : Nat) -> SeqNum n

-- The magic cookie uniquely identifies a ZRTP Packet
-- and always contrains the hex string
data MCookie : String -> String -> Type where
  MC : MCookie "ZRTP" "0x5a525450" 

-- The source identifier is the SSRC number of the RTP stream
-- to which the ZRTP packet relates (this means nothing to me!)
data SourceId : Nat -> Type where
  SID : (n : Nat) -> SourceId n

-- The cyclic redundancy check is calculated across the entire
-- ZRTP packet, exluding the CRC field. If a message fails the 
-- check it is discarded.
data CRC : Nat -> Type where
  C : (n : Nat) -> CRC n

||| HASHES |||
-- The Hash algorithm
data Hash = 
  S256
  | S384
  | N256
  | N384

-- The list of hashes in order that can be negotiated
data HashTypes = HT (List Hash)

-- The message authentication code - keyed hash based on
-- the negociated 'HashType'
data MAC

-- TODO: Understand Hash negociation steps.
data HMAC
data NegociatedHash
data NegociatedMAC


||| ZRTP MESSSAGES |||

data HashImage
data PreAmble
data MsgLength
data Flags

-- TODO: Check up on ZID and ZRTP Endpoints!
-- unique identifier for ZRTP Endpoint
data ZID

-- Identifies protocol version
data ZRTPVersion = Ver String

-- Identifies venfor and release for client identifier string
data CIC = CI String String

-- The block cipher algorithm found in the hello message
-- and the commit message
data Cipher =
  AES1
  | AES2
  | AES3
  | TWOFS1
  | TWOFS2
  | TWOFS3

-- The auth tag for a ZRTP endpoint.
data AuthTag =
  HS32
  | HS80
  | SK32
  | SK64

-- The various key agreement types
data KeyAgreement =
  DH3K
  | DH2K
  | EC25
  | EC38
  | EC52
  | PRSH
  | MULT

-- The encoding types for the short authentication string.
data SAS = B32 | B256

-- The ways to sign an SAS.
data Sig = PGP | X509

-- TODO: what is this!?
data HVI
data PVR
data Nonce
data KeyID

RS1IDR : Type
RS1IDR = Hash

RS2IDR : Type
RS2IDR = Hash

AuxSecretIDR : Type
AuxSecretIDR = Hash

PBXSecretIDR : Type
PBXSecretIDR = Hash

-- TODO: Check the DH hash HVI
data Commit : KeyAgreement -> Type where
  DH : HashImage -> ZID -> Hash -> Cipher -> AuthTag -> 
       (k: KeyAgreement) -> SAS -> HVI -> MAC -> Commit k
  PS : HashImage -> ZID -> Hash -> Cipher -> AuthTag -> 
       SAS -> Nonce -> MAC -> Commit k
  MS : HashImage -> ZID -> Hash -> Cipher -> AuthTag -> 
       SAS -> Nonce -> KeyID -> MAC -> Commit PRSH

-- The set of ZRTP Primitives and associated msg information
data ZRTPPrim =
  Hello ZRTPVersion CIC HashImage Flags (List Hash) (List Cipher)
    (List AuthTag) (List KeyAgreement) (List SAS) MAC
  | HelloAck
  | Com (Commit k)
  | DHPart1 
  | DHPart2
  | Confirm1
  | Confirm2
  | Conf2Ack
  | Error
  | ErrorAck
  | GoClear
  | ClearAck
  | SASRelay
  | RelayAck
  | Ping
  | PingAck2

data ZRTPMsg =
  ZRTP PreAmble MsgLength ZRTPPrim









||| ZRTP Product Type representing the packet |||
data ZRTPPacket =
  ZP (SeqNum n) (MCookie s s') (SourceId m) ZRTPPrim (CRC k)








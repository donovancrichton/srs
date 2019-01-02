import Data.Bits
import Data.Buffer

%default total

-- This is very gross, but until I take time to look over
-- the C FFI, this will have to do!
bin2dec : List Char -> (acc : Integer) -> Integer
bin2dec [] acc = acc
bin2dec (x :: xs) acc = 
  let n = cast (cast x {to=Int}) {to=Integer} - 48 -- ASCII
      p = n * pow 2 (length xs)
      acc' = acc + p
  in bin2dec xs acc'

implementation Cast (List Bits8) (Bits 256) where
  cast = int2bits . str2int . concatstr . bits2str
    where
      bits2str : (List Bits8) -> List String
      bits2str = \bs => map b8ToBinString bs

      concatstr : List String -> String
      concatstr = \ss => foldl (++) "" ss

      str2int : String -> Integer
      str2int = \s => bin2dec (unpack s) 0

      int2bits : Integer -> Bits 256
      int2bits = \k => intToBits k {n=256}

-- nonce256 tries to read a 32 byte integer from a file
-- at 'path' and returns just the integer or Nothing
--
-- Note: | is syntactic surgar for terminating cases
-- in the Monadic sequence.
nonce256 : String -> IO $ Maybe $ Bits 256
nonce256 path = do
  -- open dev/urandom or exit on failure
  Right randFile <- openFile path Read
  | Left err => do
                  putStrLn (show err)
                  pure Nothing
  -- create a buffer with 32 bytes or exit on failure
  Just buffer <- newBuffer 32
  | Nothing => do
                  putStrLn "Failed to create 32-byte buffer!"
                  pure Nothing
  -- read the stream from the file
  rawBuffer <- readBufferFromFile randFile buffer 32
  closeFile randFile
  -- store as a list of bytes
  bs <- bufferData rawBuffer
  -- convert to a binary string representation
  let bits = cast bs {to=Bits 256}
  pure (Just bits)

main : IO ()
main = do
  bits <- nonce256 "/dev/urandom"
  pure ()


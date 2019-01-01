import Data.Bits
import Data.Buffer

%default total

binToDec : List Char -> (acc : Integer) -> Integer
binToDec [] acc = acc
binToDec (x :: xs) acc = 
  let n = cast (cast x {to=Int}) {to=Integer} - 48
      p = n * pow 2 (length (pack xs))
      acc' = acc + p
  in binToDec xs acc'

implementation Cast (List Bits8) (Bits 256) where
  cast xs = bits
    where
      strs : List String
      strs = map b8ToBinString xs

      str : String
      str = foldl (++) "" strs

      int : Integer
      int = binToDec (unpack str) 0

      bits : Bits 256
      bits = intToBits int {n=256}

-- nonce256 tries to read a 32 byte integer from a file
-- at 'path' and returns just the integer or Nothing
--
-- Note: | is syntactic surgar for terminating cases
-- in the Monadic sequence.
nonce256 : String -> IO (Maybe (Bits 256))
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


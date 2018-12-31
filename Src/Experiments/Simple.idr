import Data.Bits
import Data.Buffer

%default total


-- Note: | is syntactic surgar for terminating cases
-- in the Monadic sequence.
main : IO ()
main = do
  -- open dev/urandom or exit on failure
  Right randFile <- openFile "/dev/urandom" Read 
  | Left err => putStrLn (show err)
  -- create a buffer with 32 bytes or exit on failure
  Just buffer <- newBuffer 32
  | Nothing => putStrLn "Failed to create 32-byte buffer!"
  -- read the stream from the file
  rawBuffer <- readBufferFromFile randFile buffer 32
  -- store as a list of bytes
  bs <- bufferData rawBuffer
  -- convert to a binary string representation
  let nonceStr = foldl (++) "" (map b8ToBinString bs)
  let nonceInt = cast {to = Integer} nonceStr
  putStrLn (show (binToDec nonceInt))
  putStrLn (show (length nonceStr))



import Data.Bits
import Data.Buffer
import Data.Vect

%default total

test : Bits 256
test = MkBits 256

main : IO ()
main = do
 -- Make a new buffer
 Just b <- newBuffer 32 | 
   Nothing => putStrLn "Error could not create bufer!"
 Right h <- openFile "/dev/urandom" Read | 
   Left e => putStrLn (show e)
 -- fill the buffer with 32 byte chunks = 256 bits
 bs <- readBufferFromFile h b 32
 -- extract the data
 bs' <- bufferData bs
 -- convert to vector
 let bits = fromList bs'
 let b = head bs'
 putStrLn "done!"

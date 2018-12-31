import Data.Bits
import Control.ST
import Control.ST.Random

%default total

main : ST m () [rnd ::: Random]
main = do 
  rs <- getRandom x
  pure ()

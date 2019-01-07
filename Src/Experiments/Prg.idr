import Data.Vect


-- 32 Bit values and functions that operate on them. Assuming
-- constant time for all operations
data Value : Type where
  Val         : Bits32 -> Value 
  Add         : Value -> Value -> Value 
  Sub         : Value -> Value -> Value 
  Xor         : Value -> Value -> Value 
  RightRotate : Value -> Value -> Value 
  RightShift  : Value -> Value -> Value 

-- program assignment and control flow
data Prg : Nat -> Type where
  Halt   : Prg 1
  AssC   : {k : Nat} -> (name : String) -> Value -> 
                      (cont : Prg k) -> Prg (S k)
  Cond   : {k, n, m : Nat} -> (cond: Prg n) -> (true : Prg k) -> 
                              (false : Prg k) -> (cont : Prg m) -> 
                              Prg (k + n + m)
  Do     : {k, n, m : Nat} -> (value : Prg n) -> (iter : Prg k) -> 
                              (cont : Prg m) -> Prg (k * n + m)
  Skip   : {k : Nat} -> Prg k -> Prg (S k)

{-
-- init the constant 32 bit hashes for sha256
initHashValues : Prg n -> Prg (9 + n)
initHashValues p =  
  AssA "hs" 8
    (Val 0x6a09e667 :: Val 0xbb67ae85 :: Val 0x3c6ef372 ::
     Val 0xa54ff53a :: Val 0x510e527f :: Val 0x9b05688c ::
     Val 0x1f83d9ab :: Val 0x5be0cd19 :: Nil) p

-- init the round constants for sha256
initRoundConsts : Prg n -> Prg (128 + n)
initRoundConsts p =
  AssC "k0" (Val 0x428a2f98) ( 
  AssC "k1" (Val 0x71374491) (
  AssC "k2" (Val 0xb5c0fbcf) (
  AssC "k3" (Val 0xe9b5dba5) (
  AssC "k4" (Val 0x3956c25b) (
  AssC "k5" (Val 0x59f111f1) (
  AssC "k6" (Val 0x923f82a4) (
  AssC "k7" (Val 0xab1c5ed5) (
  AssC "k8" (Val 0xd807aa98) (
  AssC "k9" (Val 0x12835b01) (
  AssC "k10" (Val 0x243185be) (
  AssC "k11" (Val 0x550c7dc3) (
  AssC "k12" (Val 0x72be5d74) (
  AssC "k13" (Val 0x80deb1fe) (
  AssC "k14" (Val 0x9bdc06a7) (
  AssC "k15" (Val 0xc19bf174) (
  AssC "k16" (Val 0xe49b69c1) (
  AssC "k17" (Val 0xefbe4786) (
  AssC "k18" (Val 0x0fc19dc6) (
  AssC "k19" (Val 0x240ca1cc) (
  AssC "k20" (Val 0x2de92c6f) (
  AssC "k21" (Val 0x4a7484aa) (
  AssC "k22" (Val 0x5cb0a9dc) (
  AssC "k23" (Val 0x76f988da) (
  AssC "k24" (Val 0x983e5152) (
  AssC "k25" (Val 0xa831c66d) (
  AssC "k26" (Val 0xb00327c8) (
  AssC "k27" (Val 0xbf597fc7) (
  AssC "k28" (Val 0xc6e00bf3) (
  AssC "k29" (Val 0xd5a79147) (
  AssC "k30" (Val 0x06ca6351) (
  AssC "k31" (Val 0x14292967) (
  AssC "k32" (Val 0x27b70a85) (
  AssC "k33" (Val 0x2e1b2138) (
  AssC "k34" (Val 0x4d2c6dfc) (
  AssC "k35" (Val 0x53380d13) (
  AssC "k36" (Val 0x650a7354) (
  AssC "k37" (Val 0x766a0abb) (
  AssC "k38" (Val 0x81c2c92e) (
  AssC "k39" (Val 0x92722c85) (
  AssC "k40" (Val 0xa2bfe8a1) (
  AssC "k41" (Val 0xa81a664b) (
  AssC "k42" (Val 0xc24b8b70) (
  AssC "k43" (Val 0xc76c51a3) (
  AssC "k44" (Val 0xd192e819) (
  AssC "k45" (Val 0xd6990624) (
  AssC "k46" (Val 0xf40e3585) (
  AssC "k47" (Val 0x106aa070) (
  AssC "k48" (Val 0x19a4c116) (
  AssC "k49" (Val 0x1e376c08) (
  AssC "k50" (Val 0x2748774c) (
  AssC "k51" (Val 0x34b0bcb5) (
  AssC "k52" (Val 0x391c0cb3) (
  AssC "k53" (Val 0x4ed8aa4a) (
  AssC "k54" (Val 0x5b9cca4f) (
  AssC "k55" (Val 0x682e6ff3) (
  AssC "k56" (Val 0x748f82ee) (
  AssC "k57" (Val 0x78a5636f) (
  AssC "k58" (Val 0x84c87814) (
  AssC "k69" (Val 0x8cc70208) (
  AssC "k60" (Val 0x90befffa) (
  AssC "k61" (Val 0xa4506ceb) (
  AssC "k62" (Val 0xbef9a3f7) ( 
  AssC "k63" (Val 0xc67178f2) 
    p)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- Padd a 256 bit input with 256 bits as per sha256
pad256 : Prg n -> Prg (16 + n)
pad256 p = 
  AssC "m8" (Val 0x80000000) (
    AssC "m9" (Val 0x00000000) (
      AssC "m10" (Val 0x00000000) (
        AssC "m11" (Val 0x00000000) (
          AssC "m12" (Val 0x00000000) (
            AssC "m13" (Val 0x00000000) (
              AssC "m14" (Val 0x00000000) (
                AssC "m15" (Val 0x00000000) p)))))))


sha256of256 : (n : Nat ** Prg n)
sha256of256 = (_ ** initHashValues (initRoundConsts Halt))
-}

import Data.Vect


-- 32 Bit values and functions that operate on them. Assuming
-- constant time for all operations
data Value : Type where
  Error       : Value
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
  AssA   : {k : Nat} -> (name : String) -> (index : Nat) ->
                        (val : Value) ->
                        (cont : Prg k) -> Prg (S k)
  Cond   : {k, m, n : Nat} -> (cond: Prg n) -> (true : Prg k) -> 
                              (false : Prg k) -> (cont : Prg m) -> 
                              Prg (k + n + m)
  Do     : {k, m, n : Nat} -> (counts : Nat) -> (iter : Prg k) -> 
                              (cont : Prg m) -> Prg (k * n + m)
  Skip   : {k : Nat} -> Prg k -> Prg (S k)

getVal : Prg n -> Value
getVal Halt = Error
getVal (AssC name x cont) = x
getVal (AssA name index val cont) = val
getVal (Cond cond true false cont) = Error
getVal (Do counts iter cont) = Error
getVal (Skip x) = Error


-- init the constant 32 bit hashes for sha256
initHashValues : Prg n -> Prg (8 + n)
initHashValues p =  
  AssA "hs" 0 (Val 0x6a09e667) (
  AssA "hs" 1 (Val 0xbb67ae85) (
  AssA "hs" 2 (Val 0x3c6ef372) (
  AssA "hs" 3 (Val 0xa54ff53a) (
  AssA "hs" 4 (Val 0x510e527f) (
  AssA "hs" 5 (Val 0x9b05688c) (
  AssA "hs" 6 (Val 0x1f83d9ab) (
  AssA "hs" 7 (Val 0x5be0cd19) p)))))))

{-
-- init the round constants for sha256
initRoundConsts : Prg n -> Prg (S (n + 64))
initRoundConsts p =
  AssA "ks" [Val 0x428a2f98, Val 0x71374491, Val 0xb5c0fbcf,
             Val 0xe9b5dba5, Val 0x3956c25b, Val 0x59f111f1,
             Val 0x923f82a4, Val 0xab1c5ed5, 
             Val 0xd807aa98, Val 0x12835b01, Val 0x243185be, 
             Val 0x550c7dc3, Val 0x72be5d74, Val 0x80deb1fe, 
             Val 0x9bdc06a7, Val 0xc19bf174, 
             Val 0xe49b69c1, Val 0xefbe4786, Val 0x0fc19dc6, 
             Val 0x240ca1cc, Val 0x2de92c6f, Val 0x4a7484aa, 
             Val 0x5cb0a9dc, Val 0x76f988da,
             Val 0x983e5152, Val 0xa831c66d, Val 0xb00327c8,
             Val 0xbf597fc7, Val 0xc6e00bf3, Val 0xd5a79147,
             Val 0x06ca6351, Val 0x14292967, 
             Val 0x27b70a85, Val 0x2e1b2138, Val 0x4d2c6dfc, 
             Val 0x53380d13, Val 0x650a7354, Val 0x766a0abb, 
             Val 0x81c2c92e, Val 0x92722c85, 
             Val 0xa2bfe8a1, Val 0xa81a664b, Val 0xc24b8b70, 
             Val 0xc76c51a3, Val 0xd192e819, Val 0xd6990624, 
             Val 0xf40e3585, Val 0x106aa070,
             Val 0x19a4c116, Val 0x1e376c08, Val 0x2748774c,
             Val 0x34b0bcb5, Val 0x391c0cb3, Val 0x4ed8aa4a,
             Val 0x5b9cca4f, Val 0x682e6ff3, 
             Val 0x748f82ee, Val 0x78a5636f, Val 0x84c87814, 
             Val 0x8cc70208, Val 0x90befffa, Val 0xa4506ceb, 
             Val 0xbef9a3f7, Val 0xc67178f2] p

-- Padd a 256 bit input with 256 bits as per sha256
pad256 : (ms : Vect 8 Value) -> (cont : Prg n) -> Prg (S (n + 16))
pad256 ms p = 
  AssA "ms" [index 0 ms, index 1 ms, index 2 ms, index 3 ms, 
             index 4 ms, index 4 ms, index 6 ms, index 7 ms,
             Val 0x80000000, Val 0x00000000, Val 0x00000000, 
             Val 0x00000000, Val 0x00000000, Val 0x00000000, 
             Val 0x00000000, Val 0x00000100] p

-- process the single 512 bit chunk as per sha256
process : Vect 16 value -> Prg n -> Prg k
process ms p = ?wait
  where
    f' : Value -> Value
    f' x = Xor (RightRotate x (Val 7)) (RightRotate x (Val 18))

    f : Value -> Value
    f x = Xor (f' x) (RightShift x (Val 3))

    g' : Value -> Value
    g' y = Xor (RightRotate y (Val 17)) (RightRotate y (Val 19))

    g : Value -> Value
    g y = Xor (g' y) (RightShift y (Val 10))

    fg : Value -> Value -> Value -> Value -> Value
    fg = \n, m, x, y => Add (Add (Add n (f x)) n) (g y)


             


sha256of256 : (n : Nat ** Prg n)
sha256of256 = (_ ** (initHashValues (initRoundConsts 
              (pad256 [Val 116101, Val 115116, Val 0, 
                      Val 0, Val 0, Val 0, Val 0, Val 0] 
                      Halt))))

-}

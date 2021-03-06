import Data.Buffer

%default total

-----------------------CONNECTIVES---------------------------
Iff : {p, q : Type} -> Type
Iff {p} {q} = (p -> q, q -> p)

syntax [p] "<-->" [q] = Iff {p} {q} 

iff_sym : (p <--> q) -> (q <--> p)
iff_sym (a, b) = (b, a)


----------------PRELUDE REIMPLEMENTATIONS--------------------

-- This is the helper function for 'myDiv' taken right
-- out of Prelude.Nat.idr
div' : Nat -> Nat -> Nat -> Nat
div' Z centre right = Z
div' (S left) centre right = 
  if lte centre right 
  then Z
  else S (div' left (minus centre (S right)) right)

-- This is the same as myDiv from the prelude, I've
-- reimplemented it here so I can prove things about
-- the helper function div', with a view towards submitting
-- a pull request in Nat.idr with the associated proofs.
myDiv : Nat -> (y: Nat) -> Not (y = Z) -> Nat
myDiv left Z p = void (p Refl)
myDiv left (S right) _ = div' left left right



-----------------------------LEMMAS---------------------------

-- boolean reflection of 'lte'
lteReflectsLTE : (n, m : Nat) -> (True = lte n m) <--> LTE n m
lteReflectsLTE n m = (to n m, fro n m)
  where
    to : (k, j : Nat) -> (prf : True = lte k j) -> LTE k j
    to Z j prf = LTEZero {right=j}
    to (S i) j prf = 
      let rec = \p => to i j p
      in ?check

    fro : (k, j : Nat) -> LTE k j -> True = lte k j
    fro Z j prf = Refl
    fro (S i) j prf = ?test

divNNZisSZ : (k : Nat) -> div' k k Z = k
divNNZisSZ Z = Refl
divNNZisSZ (S k) = 
  let rec = divNNZisSZ k
  in rewrite minusZeroRight k in rewrite rec in Refl

divCentreZIsZ : (k : Nat) -> div' k Z k = Z
divCentreZIsZ Z = Refl
divCentreZIsZ (S k) = Refl

divNNKisNotGT : (n, m : Nat) -> LTE (div' n n m) n
divNNKisNotGT Z Z = lteRefl
divNNKisNotGT Z (S k) = lteRefl
divNNKisNotGT (S k) Z = rewrite minusZeroRight k in
                          rewrite divNNZisSZ k in lteRefl
divNNKisNotGT (S k) (S j) with (lte k j) proof p
  divNNKisNotGT (S k) (S j) | True = LTEZero
  divNNKisNotGT (S k) (S j) | False = ?later

minusNNalwaysZ : (n : Nat) -> minus n n = Z
minusNNalwaysZ Z = Refl
minusNNalwaysZ (S k) = 
  let rec = minusNNalwaysZ k
  in rewrite rec in Refl

succNeverLT : (n : Nat) -> lte (S n) n = False
succNeverLT Z = Refl
succNeverLT (S k) = 
  let rec = succNeverLT k
  in rewrite rec in Refl

-----------------------------PROOFS---------------------------

divByItselfAlwaysSZ : (n : Nat) -> (prf : Not (n = 0)) -> 
                      myDiv n n prf = S Z
divByItselfAlwaysSZ Z prf = void (prf Refl)
divByItselfAlwaysSZ (S Z) prf = Refl
divByItselfAlwaysSZ (S k) prf = 
  rewrite succNeverLT k in 
    rewrite minusNNalwaysZ k in
      rewrite divCentreZIsZ k in Refl

divIsNotGT : (n, m : Nat) -> (prf : Not (m = 0)) -> 
             LTE (myDiv n m prf) n
divIsNotGT n Z prf = void (prf Refl)
divIsNotGT n (S Z) prf = rewrite divNNZisSZ n in lteRefl
divIsNotGT n (S k) prf = 
  let rec = \p => divIsNotGT n k p
  in ?wait

------------------------------CODE----------------------------
BufferError : Type
BufferError = String

data Error = FE FileError | BE BufferError

implementation Cast String (List Bits8) where
  cast s = map c2b8 cs
    where 
      cs : List Char
      cs = unpack s

      c2b8 : Char -> Bits8
      c2b8 = fromInteger . (cast {to=Integer}) . (cast {to=Int})

nonce256 : String -> IO $ Either Error $ List Bits8
nonce256 path = do
  -- Try to open the file at 'path' as read only.
  Right file <- openFile path Read
  | Left error => pure (Left (FE error))
  -- Try to create a new buffer of 32 bytes.
  Just newBuffer <- newBuffer 32
  | Nothing => pure (Left (BE "New buffer could not be created!"))
  -- Populate buffer from file
  buffer <- readBufferFromFile file newBuffer 32
  -- return list of bytes
  bytes <- bufferData buffer
  pure $ Right bytes

{-
-- TODO: move from List Bits8 to Vect n Bits8 to gain proofs
--       about the length of byte vectors
sha256 : String -> (Nat, Nat)
sha256 s = (bitLength, padLength)
  where
    h0 : List Bits8
    h0 = [0x6a,0x90,0xe6,0x67]
    h1 : List Bits8
    h1 = [0xbb,0x67,0xa,0xe85]
    h2 : List Bits8
    h2 = [0x3c,0x6e,0xf3,0x72]
    h3 : List Bits8
    h3 = [0xa5,0x4f,0xf5,0x3a]
    h4 : List Bits8
    h4 = [0x51,0x0e,0x52,0x7f]
    h5 : List Bits8
    h5 = [0x9b,0x05,0x68,0x8c]
    h6 : List Bits8
    h6 = [0x1f,0x83,0xd9,0xab]
    h7 : List Bits8
    h7 = [0x5b,0xe0,0xcd,0x19]

    hs : List (List Bits8)
    hs = [h0,h1,h2,h3,h4,h5,h6,h7]

    k0 : List Bits8
    k0 = [0x42,0x8a,0x2f,0x98]
    k1 : List Bits8
    k1 = [0x71,0x37,0x44,0x91]
    k2 : List Bits8
    k2 = [0xb5,0xc0,0xfb,0xcf]
    k3 : List Bits8
    k3 = [0xe9,0xb5,0xdb,0xa5]
    k4 : List Bits8
    k4 = [0x39,0x56,0xc2,0x5b]
    k5 : List Bits8
    k5 = [0x59,0xf1,0x11,0xf1]
    k6 : List Bits8
    k6 = [0x92,0x3f,0x82,0xa4]
    k7 : List Bits8
    k7 = [0xab,0x1c,0x5e,0xd5]
    k8 : List Bits8
    k8 = [0xd8,0x07,0xaa,0x98]
    k9 : List Bits8
    k9 = [0x12,0x83,0x5b,0x01]
    k10 : List Bits8
    k10 = [0x24,0x31,0x85,0xbe]
    k11 : List Bits8
    k11 = [0x55,0x0c,0x7d,0xc3]
    k12 : List Bits8
    k12 = [0x72,0xbe,0x5d,0x74]
    k13 : List Bits8
    k13 = [0x80,0xde,0xb1,0xfe]
    k14 : List Bits8
    k14 = [0x9b,0xdc,0x06,0xa7]
    k15 : List Bits8
    k15 = [0xc1,0x9b,0xf1,0x74]
    k16 : List Bits8
    k16 = [0xe4,0x9b,0x69,0xc1]
    k17 : List Bits8
    k17 = [0xef,0xbe,0x47,0x86]
    k18 : List Bits8
    k18 = [0x0f,0xc1,0x9d,0xc6]
    k19 : List Bits8
    k19 = [0x24,0x0c,0xa1,0xcc]
    k20 : List Bits8
    k20 = [0x2d,0xe9,0x2c,0x6f]
    k21 : List Bits8
    k21 = [0x4a,0x74,0x84,0xaa]
    k22 : List Bits8
    k22 = [0x5c,0xb0,0xa9,0xdc]
    k23 : List Bits8
    k23 = [0x76,0xf9,0x88,0xda]
    k24 : List Bits8
    k24 = [0x98,0x3e,0x51,0x52]
    k25 : List Bits8
    k25 = [0xa8,0x31,0xc6,0x6d]
    k26 : List Bits8
    k26 = [0xb0,0x03,0x27,0xc8]
    k27 : List Bits8
    k27 = [0xbf,0x59,0x7f,0xc7]
    k28 : List Bits8
    k28 = [0xc6,0xe0,0x0b,0xf3]
    k29 : List Bits8
    k29 = [0xd5,0xa7,0x91,0x47]
    k30 : List Bits8
    k30 = [0x06,0xca,0x63,0x51]
    k31 : List Bits8
    k31 = [0x14,0x29,0x29,0x67]
    k32 : List Bits8
    k32 = [0x27,0xb7,0x0a,0x85]
    k33 : List Bits8
    k33 = [0x2e,0x1b,0x21,0x38]
    k34 : List Bits8
    k34 = [0x4d,0x2c,0x6d,0xfc]
    k35 : List Bits8
    k35 = [0x53,0x38,0x0d,0x13]
    k36 : List Bits8
    k36 = [0x65,0x0a,0x73,0x54]
    k37 : List Bits8
    k37 = [0x76,0x6a,0x0a,0xbb]
    k38 : List Bits8
    k38 = [0x81,0xc2,0xc9,0x2e]
    k39 : List Bits8
    k39 = [0x92,0x72,0x2c,0x85]
    k40 : List Bits8
    k40 = [0xa2,0xbf,0xe8,0xa1]
    k41 : List Bits8
    k41 = [0xa8,0x1a,0x66,0x4b]
    k42 : List Bits8
    k42 = [0xc2,0x4b,0x8b,0x70]
    k43 : List Bits8
    k43 = [0xc7,0x6c,0x51,0xa3]
    k44 : List Bits8
    k44 = [0xd1,0x92,0xe8,0x19]
    k45 : List Bits8
    k45 = [0xd6,0x99,0x06,0x24]
    k46 : List Bits8
    k46 = [0xf4,0x0e,0x35,0x85]
    k47 : List Bits8
    k47 = [0x10,0x6a,0xa0,0x70]
    k48 : List Bits8
    k48 = [0x19,0xa4,0xc1,0x16]
    k49 : List Bits8
    k49 = [0x1e,0x37,0x6c,0x08]
    k50 : List Bits8
    k50 = [0x27,0x48,0x77,0x4c]
    k51 : List Bits8
    k51 = [0x34,0xb0,0xbc,0xb5]
    k52 : List Bits8
    k52 = [0x39,0x1c,0x0c,0xb3]
    k53 : List Bits8
    k53 = [0x4e,0xd8,0xaa,0x4a]
    k54 : List Bits8
    k54 = [0x5b,0x9c,0xca,0x4f]
    k55 : List Bits8
    k55 = [0x68,0x2e,0x6f,0xf3]
    k56 : List Bits8
    k56 = [0x74,0x8f,0x82,0xee]
    k57 : List Bits8
    k57 = [0x78,0xa5,0x63,0x6f]
    k58 : List Bits8
    k58 = [0x84,0xc8,0x78,0x14]
    k59 : List Bits8
    k59 = [0x8c,0xc7,0x02,0x08]
    k60 : List Bits8
    k60 = [0x90,0xbe,0xff,0xfa]
    k61 : List Bits8
    k61 = [0xa4,0x50,0x6c,0xeb]
    k62 : List Bits8
    k62 = [0xbe,0xf9,0xa3,0xf7]
    k63 : List Bits8
    k63 = [0xc6,0x71,0x78,0xf2]

    ks : List (List Bits8)
    ks = [k0,k1,k2,k3,k4,k5,k6,k7,
          k8,k9,k10,k11,k12,k13,k15,k15,
          k16,k17,k18,k19,k20,k21,k22,k23,
          k24,k25,k26,k27,k28,k29,k30,k31,
          k32,k33,k34,k35,k36,k37,k38,k39,
          k40,k41,k42,k43,k44,k45,k46,k47,
          k48,k49,k50,k51,k52,k53,k54,k55,
          k56,k57,k58,k59,k60,k61,k62,k63]
    
    bitLength : Nat
    bitLength = 8 * length s

    padLength : Nat
    padLength = l
      where
        l : Nat
        l = bitLength

        k : (n: Nat) -> Nat
        k n with (isLTE (S n) 512) proof p
          k n | Yes prf =
            let prf2 = lteSuccLeft prf
            in 512 - n
          k n | No contra = ?wait
            where
              m : Nat
              m = ((myDiv n 512 SIsNotZ) + 1) * 512

              prf : LTE 512 n
              prf = notLTImpliesGTE contra

              prf2 : LTE (S n) m
              prf2 = ?alsowait

-}

main : IO ()
main = do
  Right bits <- nonce256 "/dev/urandom"
  | Left err => putStrLn "Somehow got err"
  putStrLn $ show bits


  



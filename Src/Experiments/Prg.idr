import Data.Vect
import Data.Bits

%default total

--------------------------AXIOMS---------------------------

-- Addition of 32 Bit Integers with overflow is associative
postulate 
addB32Assoc : (x, y, z: Bits32) ->
 prim__addB32 (prim__addB32 x y) z =
    prim__addB32 x (prim__addB32 y z)
-----------------------------------------------------------

Env : Type
Env = String -> Bits32

emptyEnv : Env
emptyEnv = \x => 0

updateEnv : String -> Bits32 -> Env -> Env
updateEnv s v g = \s' => if s == s' 
                         then v
                         else g s'

--updateEnvArr : String -> Nat -> Maybe String -> Env -> Env
--updateEnvArr s i v g = ?test

-- program assignment and control flow
data Prg : Nat -> Type where
  Halt   : Prg (S Z)
  AssC   : {k : Nat} -> (name : String) -> (val : Bits32) -> 
                        (cont : Prg k) -> Prg (S k)
  AssV   : {k : Nat} -> (name1 : String) -> (name2 : String) ->
                        (cont : Prg k) -> Prg (S k)             
  AssA   : {k : Nat} -> (name : String) -> (index : Nat) -> 
                        (var : String) -> (cont : Prg k) ->
                        Prg (S k)
  UnOp   : {k : Nat} -> (name : String) -> 
           (op : Bits32 -> Bits32) -> (x : String) ->
           (cont : Prg k) -> Prg (S k)
  BinOp  : {k : Nat} -> (name : String) -> 
           (op : Bits32 -> Bits32 -> Bits32) -> (x : String) -> 
           (y : String) -> (cont : Prg k) -> Prg (S k)
  Do     : {k, m : Nat} -> (iter : Prg m) -> (n : Nat) -> 
                           (cont : Prg k) -> Prg (S ((n * m) + k))
  Cond   : {k, n : Nat} -> (pred : String) -> (true : Prg n) ->
                        (false : Prg n) -> (cont : Prg k) ->
                        Prg (S (n + k))

-- evaluation
eval : Prg n -> Env -> Env
eval Halt g = g
eval (AssC name val cont) g = 
  eval cont $ updateEnv name (val) g
eval (AssV name1 name2 cont) g =
  eval cont $ updateEnv name1 (g name2) g
eval (AssA name index var cont) g =
  eval cont $ updateEnv (name ++ (show index)) (g var) g
eval (UnOp name f x cont) g =
  eval cont $ updateEnv name (f (g x)) g
eval (BinOp name f x y cont) g = 
  eval cont $ updateEnv name (f (g x) (g y)) g
eval (Do iter Z cont) g = eval cont g
eval (Do iter (S k) cont) g = assert_total $ 
  eval cont $ eval (Do iter k cont) (eval iter g)
eval (Cond pred true false cont) g = 
  case (g pred) of
    0        => eval cont $ eval true g
    _        => eval cont $ eval false g

-- a test program for addition
testAdd : Prg 4
testAdd = (AssC "a2" 2 (AssV "a1" "x" 
            (BinOp "x" (+) "a1" "a2" Halt)))

-- a test program for iteration
testDo : Prg 14
testDo = (Do testAdd 3 Halt)

-- i := 0; x := 0; for i in 0 to 9; x := i + x
testSum : Prg 44
testSum = ieq0 (xeq0 do10)
  where
    ieq0 : Prg k -> Prg (S k)
    ieq0 = \p => AssC "i" 0 p

    xeq0 : Prg k -> Prg (S k)
    xeq0 = \p => AssC "x" 0 p

    xplusi : Prg k -> Prg (S k)
    xplusi = \p => BinOp "x" (+) "x" "i" p

    yeq1 : Prg k -> Prg (S k)
    yeq1 = \p => AssC "y" 1 p

    iplus1 : Prg k -> Prg (S k)
    iplus1 = \p => BinOp "i" (+) "i" "y" p

    do10 : Prg 42
    do10 = Do (xplusi (yeq1 (iplus1 Halt))) 10 Halt
      
              
-------------------- Simple Proofs ------------------------

-- A proof that evaluating testAdd on any env for "x"
-- returns x + 2.

prgAddDoesAdd : (g : Env) -> 
  (eval Main.testAdd g) "x" = (g "x") + 2
prgAddDoesAdd g = Refl


-- A proof that evaluating testDo on any env for "x"
-- returns x + 6
prgDoDoesDo : (g : Env) -> 
  (eval Main.testDo g) "x" = (g "x") + 6
prgDoDoesDo g = 
  let prf = addB32Assoc (g "x") 2 2
      prf2 = addB32Assoc (g "x") 4 2
  in rewrite prf in prf2
  
-- A proof that testSum sums the numbers between 0 and 9
-- regardless of the given environment.
prgTestSumDoesSum : (g : Env) -> (eval Main.testSum g) "x" = 45
prgTestSumDoesSum g = Refl

prgSHA256 : Bits32 -> Bits32 -> Bits32 -> Bits32 -> Bits32 -> 
            Bits32 -> Bits32 -> Bits32 -> Prg ?test
prgSHA256 m1 m2 m3 m4 m5 m6 m7 m8 = ?hold


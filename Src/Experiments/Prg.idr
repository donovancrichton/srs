import Data.Vect
import Data.Bits

%default total

Env : Type
Env = List (String, Bits32)

lookup : (name : String) -> (env : Env) -> Maybe Bits32
lookup s [] = Nothing
lookup s ((s', v) :: svs) = if s == s'
                            then (Just v) 
                            else Main.lookup s svs

replace : (name : String) -> (val : Bits32) -> (env : Env) -> Env
replace s v [] = []
replace s v ((s', v') :: svs) = if s == s'
                                then (s, v) :: replace s v svs
                                else (s', v') :: replace s v svs

update : (name : String) -> (val : Bits32) -> (env : Env) -> Env
update name val g = 
  case (Main.lookup name g) of
    Nothing => (name, val) :: g
    (Just v) => replace name val g

-- program assignment and control flow
data Prg : Nat -> Type where
  Halt   : Prg (S Z)
  AssC   : {k : Nat} -> (name : String) -> (val : Bits32) -> 
                        (cont : Prg k) -> Prg (S k)
  AssA   : {k : Nat} -> (name : String) -> (index : Nat) -> 
                        (val : Bits32) -> (cont : Prg k) ->
                        Prg (S k)
  Add    : {k : Nat} -> (name: String) -> (x : Bits32) -> 
                        (y : Bits32) -> (cont : Prg k) -> 
                        Prg (S k)
  Xor    : {k : Nat} -> (name : String) -> (x : Bits32) ->
                        (y : Bits32) -> (cont : Prg k) -> 
                        Prg (S k)
  Not    : {k : Nat} -> (name : String) -> (x : Bits32) -> 
                        (cont : Prg k) -> Prg (S k)
  RightShift : {k : Nat} -> (name : String) -> (x : Bits32) ->
                            (count : Bits32) -> 
                            (cont : Prg k) -> Prg (S k)
  RightRotate : {k : Nat} -> (name : String) -> (x : Bits32) ->
                             (count : Bits32) -> (cont : Prg k) -> 
                             Prg (S k)
  Do     : {k, m : Nat} -> (iter : Prg m) -> (n : Nat) -> 
                           (cont : Prg k) -> Prg (S ((n * m) + k))
  Cond   : {k, n : Nat} -> (pred : Bits32) -> (true : Prg n) ->
                        (false : Prg n) -> (cont : Prg k) ->
                        Prg (S (n + k))

eval : Prg n -> Env -> Env
eval Halt g = g
eval (AssC s v c) g = eval c $ update s v g
eval (AssA s i v c) g = eval c $ update (s ++ (show i)) v g
eval (Add s x y c) g = eval c $ update s (prim__addB32 x y) g
eval (Xor s x y c) g =
  assert_total $ eval c $ update s (prim__xorB32 x y) g
eval (Not s x c) g = 
  eval c $ update s (prim__subB32 4294967296 x) g
eval (RightShift s x i c) g = 
  assert_total $ eval c $ update s (prim__ashrB32 x i) g
eval (RightRotate s x i c) g = 
  let val = \y => prim__orB32 (prim__shlB32 x i) y
  eval c $ update s (x * (Prelude.pow 2 i)) g
eval (Do i Z c) g = eval c g
eval (Do i (S k) c) g = ?test
eval (Cond p t f c) g = 
  if not (p == 0) 
  then eval c $ eval t g
  else eval c $ eval f g










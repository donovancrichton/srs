import Data.Vect
import Data.Bits

%default total

Env : Type
Env = String -> Maybe Bits32

emptyEnv : Env
emptyEnv = \x => Nothing

updateEnv : String -> Bits32 -> Env -> Env
updateEnv s v g = \s' => if s == s' 
                         then Just v
                         else g s'

{-
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
-}
-- program assignment and control flow
data Prg : Nat -> Type where
  Halt   : Prg (S Z)
  AssC   : {k : Nat} -> (name : String) -> (val : Bits32) -> 
                        (cont : Prg k) -> Prg (S k)
  AssV   : {k : Nat} -> (name1 : String) -> (name2 : String) ->
                        (cont : Prg k) -> Prg (S k)             
--  AssA   : {k : Nat} -> (name : String) -> (index : Nat) -> 
--                        (val : Bits32) -> (cont : Prg k) ->
--                        Prg (S k)
  -- Consider replacing functions on values with a generic
  -- operation on variables
  Add    : {k : Nat} -> (name: String) -> (x : String) -> 
                        (y : String) -> (cont : Prg k) -> 
                        Prg (S k)
--  Xor    : {k : Nat} -> (name : String) -> (x : String) ->
--                        (y : Bits32) -> (cont : Prg k) -> 
--                        Prg (S k)
--  Not    : {k : Nat} -> (name : String) -> (x : Bits32) -> 
--                        (cont : Prg k) -> Prg (S k)
--  RightShift : {k : Nat} -> (name : String) -> (x : Bits32) ->
--                            (count : Bits32) -> 
--                            (cont : Prg k) -> Prg (S k)
--  RightRotate : {k : Nat} -> (name : String) -> (x : Bits32) ->
--                             (count : Bits32) -> (cont : Prg k) -> 
--                             Prg (S k)
--  Do     : {k, m : Nat} -> (iter : Prg m) -> (n : Nat) -> 
--                           (cont : Prg k) -> Prg (S ((n * m) + k))
--  -- consider replacing predicate with a variable name
--  Cond   : {k, n : Nat} -> (pred : Bits32) -> (true : Prg n) ->
--                        (false : Prg n) -> (cont : Prg k) ->
--                        Prg (S (n + k))
{-

{-
eval : Prg n -> Env -> Env
eval Halt g = g
eval (AssC s v c) g = eval c $ update s v g
eval (AssA s i v c) g = eval c $ update (s ++ (show i)) v g
eval (AssV s1 s2 c) g = ?check
eval (Add s x y c) g = eval c $ update s (prim__addB32 x y) g
eval (Xor s x y c) g = 
  assert_total $ eval c $ update s (prim__xorB32 x y) g
eval (Not s x c) g = 
  let val = prim__subB32 (prim__subB32 0 x) 1
  in eval c $ update s val g
eval (RightShift s x i c) g = 
  assert_total $ eval c $ update s (prim__ashrB32 x i) g
eval (RightRotate s x i c) g = 
  let rhs = \y => prim__orB32 (prim__ashrB32 x i) y
      rr = rhs (prim__shlB32 x (prim__subB32 32 i))
  in eval c $ update s rr g
-- TODO: replace assert_total with assert_smaller
eval (Do i Z c) g = (eval c g)
eval (Do i (S k) c) g = assert_total $ eval (Do i k c) (eval i g)
eval (Cond p t f c) g = 
  if not (p == 0) 
  then eval c $ eval t g
  else eval c $ eval f g
-}
-}

{-
eval : Prg n -> Env -> Env
eval Halt g = g
eval (AssC name val cont) g = eval cont $ updateEnv name val g
eval (AssV name1 name2 cont) g = 
  eval cont $ updateEnv name1 (g name2) g
eval (Add name x y cont) g = 
  eval cont $ updateEnv name (g x + g y) g
-}

testProgram : Prg 4
testProgram = (AssC "a2" 2 (AssV "a1" "x" (Add "x" "a1" "a2" 
                Halt)))

--prf : (g : Env) -> (eval Main.testProgram g) "x" = (g "x") + 2
--prf g = Refl


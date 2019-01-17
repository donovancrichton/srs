import Data.Vect
import Data.Bits

%default total

Env : Type
Env = String -> Bits32

ArrEnv : Type
ArrEnv = String -> Nat -> Bits32


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
-- consider implementing this
-- eval : Prg n -> (Env, ArrEnv) -> (Env, ArrEnv)
eval : Prg n -> Env -> Env
eval Halt g = g
eval (AssC name val cont) g = 
  eval cont $ updateEnv name val g
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
    0 => eval cont $ eval true g
    _ => eval cont $ eval false g

-- a test program for addition
testAdd : Prg 4
testAdd = (AssC "a2" 2 (AssV "a1" "x" (BinOp "x" (+) "a1" "a2" 
                Halt)))

-- a test program for iteration
testDo : Prg 14
testDo = 
  (Do testAdd 3 Halt)

prgAddDoesAdd : (g : Env) -> 
  (eval Main.testAdd g) "x" = (g "x") + 2
prgAddDoesAdd g = Refl

addXZeroIsX : (x : Bits32) -> prim__addB32 0 x = x
addXZeroIsX x = ?add

someLemma : (x, y, z : Bits32) -> 
            prim__addB32 (prim__addB32 x y) z = prim__addB32 x (y + z)
someLemma x y z = ?test2

prgDoDoesDo : (g : Env) -> 
  (eval Main.testDo g) "x" = (g "x") + 6
prgDoDoesDo g = ?test





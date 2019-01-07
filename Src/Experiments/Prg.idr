


data Prg : Nat -> Type where
  Halt : Prg 1
  Assn : {k : Nat} -> varName -> value -> (cont : Prg k) -> 
         Prg (S k)
  Cond : {k, n, m : Nat} -> (cond: Prg n) -> (true : Prg k) -> 
         (false : Prg k) -> (cont : Prg m) -> Prg (k + n + m)
  Do   : {k, n, m : Nat} -> (value : Prg n) -> (iter : Prg k) -> 
         (cont : Prg m) -> Prg (k * n + m)
  Skip : {k : Nat} -> Prg k -> Prg (S k)




open Nat


def mysum (i : Nat) (n : Nat) (x : Nat -> Nat) :=
  if i = 0 then x 0 * x n
  else x i * x n + mysum (i - 1) n x

def mainsum (n : Nat) (x : Nat -> Nat) :=
  mysum n n x

#eval mysum 1 1 succ
#eval mainsum 1 succ

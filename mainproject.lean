
open Nat

-- Task 1.1: Formalize the recursive definition of the catalan numbers.

mutual

  def mysum (i : Nat) (n : Nat) (x : Nat -> Nat) :=
    if i = 0 then x 0 * x n
    else x i * x (n - i) + mysum (i - 1) n x

  def mainsum (n : Nat) (x : Nat -> Nat) :=
    mysum n n x

  def catalan (n : Nat) :=
    if n = 0 then 0
    else mainsum (n - 1) catalan

end

-- Task 1.2: Formalize the concept of plane trees.

inductive plane_tree : Type
| leaf
| node : (T1 T2 : plane_tree) -> plane_tree
deriving Repr

#eval plane_tree.leaf

-- Task 1.3: Formalize the concept of full binary trees.

inductive full_bin_tree : Type
| leaf
| node : (T1 T2 : full_bin_tree) -> full_bin_tree
deriving Repr



-- Task 1.4: Construct the type of full binary trees with n nodes,
--            not counting the leaves.

-- Task 1.5: Define the type of ballot sequences of length n.

inductive ballot_sequence : Type
|
|

-- Task 2.1: Construct a bijection.

-- Task 2.2: Construct a bijection between full binary trees and
--            the type Fin(Cn).

-- Task 2.3: Construct a bijection.

-- Task 2.4: Construct the rotating isomorphism, which is the
--            isomorphism between plane trees and full binary trees.

-- Task 2.5: Prove that Bin(2n, n) is divisible by n + 1.

-- Task 2.6: Prove that C_n = 1 / (n + 1) * Bin(2n, n).

-- Task 2.7: Construct an isomorphism.

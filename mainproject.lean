
open Nat

-- Task 1.1: Formalize the recursive definition of the catalan numbers.

mutual

  def mysum (i : Nat) (n : Nat) (x : Nat -> Nat) :=
    if i = 0 then x 0 * x n
    else x i * x (n - i) + mysum (i - 1) n x

  def mainsum (n : Nat) (x : Nat -> Nat) :=
    mysum n n x

  def catalan (n : Nat) :=
    if n = 0 then 1
    else mainsum (n - 1) catalan

end

-- Task 1.2: Formalize the concept of plane trees.

inductive plane_tree
| leaf
| node : List plane_tree -> plane_tree

-- Task 1.3: Formalize the concept of full binary trees.

inductive full_bin_tree : Type
| leaf
| node : (T1 T2 : full_bin_tree) -> full_bin_tree
deriving Repr

#check full_bin_tree.leaf
#eval full_bin_tree.node full_bin_tree.leaf full_bin_tree.leaf

-- Task 1.4: Construct the type of full binary trees with n nodes,
--            not counting the leaves.

-- Task 1.5: Define the type of ballot sequences of length n.

inductive vote
| A
| B

open vote

def valid_sequence (votes : List vote) : Bool :=
  votes.length % 2 = 0 ∧
  let n := votes.length / 2
  let (amount_of_A, amount_of_B, check) := votes.foldl (λ (amount : Nat × Nat × Bool) v =>
    match v with
    | A => (amount.1 + 1, amount.2.1, amount.2.2 ∧ (amount.2.1 ≤ amount.1 + 1))
    | B => (amount.1, amount.2.1 + 1, amount.2.2 ∧ (amount.2.1 + 1 ≤ amount.1)))
    (0, 0, True)
  amount_of_A = n ∧ amount_of_B = n ∧ check

def valid_ballot_sequence : Type := { votes : List vote // valid_sequence votes }

-- Task 2.1: Construct a bijection.

-- Task 2.2: Construct a bijection between full binary trees and
--            the type Fin(Cn).

-- Task 2.3: Construct a bijection.

-- Task 2.4: Construct the rotating isomorphism, which is the
--            isomorphism between plane trees and full binary trees.

-- Task 2.5: Prove that Bin(2n, n) is divisible by n + 1.

-- Task 2.6: Prove that C_n = 1 / (n + 1) * Bin(2n, n).

-- Task 2.7: Construct an isomorphism.

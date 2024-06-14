
import Mathlib

open Nat
open Set

-- Task 1.1: Formalize the recursive definition of the catalan numbers.

mutual

  def my_sum (i : Nat) (n : Nat) (x : Nat -> Nat) :=
    if i = 0 then x 0 * x n
    else x i * x (n - i) + my_sum (i - 1) n x

  def main_sum (n : Nat) (x : Nat -> Nat) :=
    my_sum n n x

  def my_catalan (n : Nat) :=
    if n = 0 then 1
    else main_sum (n - 1) my_catalan

end

#eval my_catalan 0  --1
#eval my_catalan 1  --1
#eval my_catalan 2  --2
#eval my_catalan 3  --5
#eval my_catalan 4  --14

-- Task 1.2: Formalize the concept of plane trees.

inductive plane_tree
| leaf
| node : List plane_tree -> plane_tree
deriving Repr

#check plane_tree.leaf
#eval plane_tree.node [plane_tree.leaf, plane_tree.leaf]

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

-- Task 2.3: Construct a bijection between lists of plane trees and
--            plane trees.

def plane_tree_to_list (T : plane_tree) : List plane_tree :=
  match T with
  | plane_tree.leaf => []
  | plane_tree.node Ts => Ts

#eval plane_tree_to_list plane_tree.leaf
#eval plane_tree_to_list (plane_tree.node [plane_tree.leaf, plane_tree.leaf])

def plane_tree_from_list (Ts : List plane_tree) : plane_tree :=
  match Ts with
  | [] => plane_tree.leaf
  | _ => plane_tree.node Ts

#eval plane_tree_from_list []
#eval plane_tree_from_list [plane_tree.leaf, plane_tree.leaf]

-- Task 2.4: Construct the rotating isomorphism, which is the
--            isomorphism between plane trees and full binary trees.

-- Task 2.5: Prove that Bin(2n, n) is divisible by n + 1.

-- Task 2.6: Prove that C_n = 1 / (n + 1) * Bin(2n, n).

theorem explicit_catalan (n : Nat) : Nat :=
  (Nat.choose (2 * n) n) / (n + 1)

theorem binomial_divisibility (n : ℕ) : (Nat.choose (2 * n) n) % (n + 1) = 0 := by
  have h : (n + 1) * explicit_catalan n = Nat.choose (2 * n) n


-- Task 2.7: Construct an isomorphism.

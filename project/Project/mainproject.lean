
import Mathlib

open Nat
open Set

-- Task 1.2: Formalize the concept of plane trees.

inductive plane_tree
| leaf
| node : List plane_tree -> plane_tree
deriving Repr

#check plane_tree.leaf
#eval plane_tree.node [plane_tree.leaf, plane_tree.leaf]

-- Task 1.3: Formalize the concept of full binary trees.

inductive full_binary_tree : Type
| leaf
| node : (T1 T2 : full_binary_tree) -> full_binary_tree
deriving Repr

#check full_binary_tree.leaf
#eval full_binary_tree.node full_binary_tree.leaf full_binary_tree.leaf

-- Task 1.4: Construct the type of full binary trees with n nodes,
--            not counting the leaves.

def internal_nodes : full_binary_tree → Nat
| .leaf => 0
| .node T1 T2 => 1 + internal_nodes T1 + internal_nodes T2

#check internal_nodes (full_binary_tree.node full_binary_tree.leaf
  full_binary_tree.leaf)
#eval internal_nodes (full_binary_tree.node full_binary_tree.leaf
  full_binary_tree.leaf) -- 1

def n_full_binary_tree (n : Nat) : Type :=
  { tree : full_binary_tree // internal_nodes tree = n }

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

def valid_ballot_sequence : Type :=
  { votes : List vote // valid_sequence votes }

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

#eval plane_tree_to_list (plane_tree_from_list [])
-- []
#eval plane_tree_to_list (plane_tree_from_list [plane_tree.leaf, plane_tree.leaf])
-- [plane_tree.leaf, plane_tree.leaf]
#eval plane_tree_from_list (plane_tree_to_list plane_tree.leaf)
-- plane_tree.leaf
#eval plane_tree_from_list (plane_tree_to_list
  (plane_tree.node [plane_tree.leaf, plane_tree.leaf]))
-- plane_tree.node [plane_tree.leaf, plane_tree.leaf])

-- Task 2.5: Prove that Bin(2n, n) is divisible by n + 1.

-- def binomial (n k : Nat) : Nat :=
--   if k ≤ n then
--     factorial n / (factorial k * factorial (n - k))
--   else 0
--
-- #eval binomial 4 2  -- 6
--
-- theorem binomial_divisibility (n : Nat) : choose (2 * n) n % (n + 1) = 0 := by
--   have t1 : factorial n * factorial (n - n) * choose n n = factorial n := by
--     rw [Nat.mul_comm, <- Nat.mul_assoc]
--     exact (Nat.choose_mul_factorial_mul_factorial (Nat.le_refl n))
--   have t2 : 0 < n -> n ! = n * (n - 1) ! := by
--     match n with
--     | 0 => intro h
--            contradiction
--     | m + 1 => intro h
--                simp
--                rw [Nat.factorial]
--   have t3 : n * choose (2 * n) n = n * factorial (2 * n) / (factorial n) * (factorial n) := by

-- Construct an isomorphism between Fin(C_n) and ballot sequences of length 2n.

-- We construct a bijection between ballot sequences of length 2n and Dyck paths of length 2n,
-- since Dyck paths are counted by Catalan numbers.

inductive move
| right
| up

open move

def valid_dyck_path (moves : List move) : Bool :=
  moves.length % 2 = 0 ∧
  let n := moves.length / 2
  let (amount_of_A, amount_of_B, check) := moves.foldl (λ (amount : Nat × Nat × Bool) v =>
    match v with
    | right => (amount.1 + 1, amount.2.1, amount.2.2 ∧ (0 ≤ amount.1 + 1 - amount.2.1))
    | up => (amount.1, amount.2.1 + 1, amount.2.2 ∧ (0 ≤ amount.1 - (amount.2.1 + 1))))
    (0, 0, True)
  amount_of_A = n ∧ amount_of_B = n ∧ check

def dyck_path : Type :=
  { moves : List move // valid_dyck_path moves }

def bs_to_dyck (seq : List vote) : List move :=
  match seq with
  | [] => []
  | x :: xs => match x with
    | A => right :: (bs_to_dyck xs)
    | _ => up :: (bs_to_dyck xs)

def bs_from_dyck (seq : List move) : List vote :=
  match seq with
  | [] => []
  | x :: xs => match x with
    | right => A :: (bs_from_dyck xs)
    | _ => B :: (bs_from_dyck xs)

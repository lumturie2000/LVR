
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

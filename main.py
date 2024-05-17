
def find_variables(clauses, variables):

    number_of_clauses = len(clauses)
    number_of_variables = len(variables)

    if number_of_clauses == 0:
        return clauses, variables, True

    for cl in range(number_of_clauses):

        clause = clauses[cl]
        check = [clause[ch] for ch in range(len(clause)) if ch != 0]

        if len(check) == 1:

            index = int(check[0])
            value = clause[index]

            if variables[index] == -1 * value:
                return clauses, variables, False

            variables[index] = value

            clauses = clauses[:cl] + clauses[cl+1:]

            number_of_clauses -= 1

            delete = []

            for a in range(number_of_clauses):

                acl = clauses[a]

                if acl[index] == value:
                    delete.append(a)
                elif acl[index] == -1 * value:
                    clauses[a][index] = 0
                    if all(val == 0 for val in clauses[a]):
                        return clauses, variables, False

            clauses = [clauses[clause_no] for clause_no in range(number_of_clauses) if clause_no not in delete]

            return variables(clauses, variables)

    for clause_no in range(number_of_clauses):
        for var_no in range(number_of_variables):

            var_val = clauses[clause_no][var_no]

            if var_val == 0:
                continue

            temp_clauses = clauses.copy()
            temp_variables = variables.copy()

            temp_variables[var_no] = var_val
            temp_clauses = temp_clauses[:clause_no] + temp_clauses[clause_no+1:]

            no_temp_clauses = number_of_clauses - 1

            incorrect = False
            delete = []

            for a in range(no_temp_clauses):

                acl = temp_clauses[a]

                if acl[var_no] == var_val:
                    delete.append(a)
                elif acl[var_no] == -1 * var_val:
                    temp_clauses[a][var_no] = 0
                    if all(val == 0 for val in temp_clauses[a]):
                        incorrect = True
                        break

            if incorrect:
                continue

            temp_clauses = [temp_clauses[x] for x in range(len(temp_clauses)) if x not in delete]

            tcl, tvar, found = find_variables(temp_clauses, temp_variables)

            if found:
                return tcl, tvar, found

    return clauses, variables, False


name = "test"
task_name = name + ".txt"
solution_name = name + "_solution.txt"

with open(task_name) as data:
    lines = data.read().splitlines()

info_row = lines[2].split(" ")
no_columns = int(info_row[2])
no_rows = int(info_row[3])

starting_clauses = []

for i in range(3, no_columns + 3):

    one_clause = lines[i].split(" ")[:-1]
    clause_with_values = [0 for k in range(no_columns + 1)]

    for j in range(one_clause):
        number = int(one_clause[j])
        if number > 0:
            clause_with_values[number] = 1
        else:
            clause_with_values[-1 * number] = -1

    starting_clauses.append(clause_with_values)


starting_variables = [0 for var in range(no_columns + 1)]

final_clauses, final_variables, found_solution = find_variables(starting_clauses, starting_variables)

if found_solution:
    variables_index_values = [str(index * final_variables[index]) for index in
                              range(len(final_variables)) if final_variables[index] != 0]
    variables_string = " ".join(variables_index_values)
    print(variables_string)
else:
    print(False)


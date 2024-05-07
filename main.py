
with open('test.txt') as d:
    lines = d.read().splitlines()

info_row = lines[2].split(" ")
no_variables = int(info_row[2])
no_rows = int(info_row[3])

clauses = []

for i in range(3, no_rows + 3):

    clause = lines[i].split(" ")[:-1]
    actual_clause = [0 for k in range(no_variables + 1)]

    for j in range(len(clause)):
        number = int(clause[j])
        if number > 0:
            actual_clause[number] = 1
        else:
            actual_clause[-1 * number] = -1

    clauses.append(actual_clause)

print(clauses)


def find_variables(clauses, final_variables):

    while len(clauses) > 0:

        flag = False

        for cl in range(len(clauses)):

            if len(clauses[cl]) == 1:

                value = clauses[cl][0]
                final_variables[cl] = clauses[cl][0]
                clauses = clauses[:cl] + clauses[cl+1:]

                flag = True

                # fix how we delete the unneeded clauses

                delete = []

                for a in range(len(clauses)):

                    acl = clauses[a]

                    if acl[cl] == value:
                        delete.append(a)
                        #clauses = clauses[:a] + clauses[a + 1:]
                    elif acl[cl] == -1 * value:
                        acl[cl] = 0
                        clauses[a] = acl

                clauses = [clauses[x] for x in range(len(clauses)) if x not in delete]

            elif not (1 in clauses[cl] or -1 in clauses[cl]):

                return clauses, final_variables, False

        if not flag:

            for g in range(len(clauses)):

                for h in range(no_variables):

                    var_no = h
                    var_val = clauses[g][h]

                    if var_val == 0:
                        continue

                    temp_clauses = clauses.copy()
                    temp_variables = final_variables.copy()

                    temp_variables[var_no] = var_val
                    temp_clauses = temp_clauses[1:]

                    delete = []

                    for a in range(len(temp_clauses)):

                        acl = temp_clauses[a]

                        if acl[var_no] == var_val:
                            delete.append(a)
                            #temp_clauses = temp_clauses[:a] + temp_clauses[a + 1:]
                        elif acl[var_no] == -1 * var_val:
                            acl[var_no] = 0
                            temp_clauses[a] = acl

                    temp_clauses = [temp_clauses[x] for x in range(len(temp_clauses)) if x not in delete]

                    tcl, tvar, found = find_variables(temp_clauses, temp_variables)

                    if found:
                        return tcl, tvar, True

            return clauses, final_variables, False


variables = [0 for var in range(no_variables + 1)]

final_variables = find_variables(clauses, variables)

print(final_variables)



from Numberjack import *
from validator import read_instance_day3, MAX_NUMBER_INTERVIEWS, MAX_ACCEPTABLE_PREF_VALUE
import csv


def get_day2_model(all_student_preferences, company_data):
    num_students = len(all_student_preferences)
    assert num_students > 0
    num_companies = len(all_student_preferences[0])
    assert len(company_data) == num_companies

    model = Model()
    all_student_rows, all_student_variables = [], []
    obj1_variables, obj1_coefficients = [], []

    for student_id, student_preferences in enumerate(all_student_preferences):
        num_expected_interviews = MAX_NUMBER_INTERVIEWS
        num_valid_preferences = sum(
            1 if pref_val < MAX_ACCEPTABLE_PREF_VALUE else 0 for pref_val in student_preferences
        )
        num_expected_interviews = min(num_expected_interviews, num_valid_preferences)

        if num_expected_interviews == 0:
            print "Ignoring student", student_id+1
            all_student_rows.append([])
            continue

        # FD variable 0..num_companies-1 deciding which company this student interviews, for
        # each one of their 'num_expected_interviews'
        student_variables = VarArray(
            num_expected_interviews, num_companies, "student%d" % (student_id+1)
        )
        all_student_rows.append(student_variables)
        all_student_variables.extend([x for x in student_variables])

        # Each of the assignments for a student must be different, redundant with ordering below
        model += AllDiff(student_variables)

        # Break symmetry by enforcing ordering on the row
        for i in range(len(student_variables)-1):
            model += student_variables[i] < student_variables[i+1]

        student_regret_vars, student_regret_coeffs = [], []
        for company_id, preference_value in enumerate(student_preferences):
            for cell in student_variables:
                obj1_variables.append(cell == company_id)
                obj1_coefficients.append(preference_value)

                student_regret_vars.append(cell == company_id)
                student_regret_coeffs.append(preference_value)

        # Enforce that the regret is 1 (i.e. the sum of pref == vbregret+1)
        virtual_best_regret = sum(sorted(student_preferences)[:num_expected_interviews])
        model += Sum(student_regret_vars, student_regret_coeffs) <= virtual_best_regret + 1

    # Cardinality constraint limiting the number of students assigned to each company
    gcc_cap_limits = {}
    for company_id, (dissappointment_cost, min_assignment, max_assignment, attendance_cost) in \
            enumerate(company_data):
        gcc_cap_limits[company_id] = [min_assignment, max_assignment]
    model += Gcc(all_student_variables, gcc_cap_limits)

    obj = Sum(obj1_variables, obj1_coefficients)
    model += Minimise(obj)

    return model, all_student_rows, obj


def output_assignments(all_student_rows, filename):
    with open(filename, "wt") as f:
        writer = csv.writer(f)
        writer.writerow(["StudentNr", "CompanyNr"])
        for student_id, student_row in enumerate(all_student_rows):
            for assigned_variable in student_row:
                # +1 for one-based indexing instead of zero-based
                writer.writerow([student_id + 1, assigned_variable.get_value() + 1])


def main(param):
    all_student_preferences, company_data = read_instance_day3(param['instance'])

    model, all_student_rows, obj = get_day2_model(all_student_preferences, company_data)

    if param['verbosity'] >= 3:
        print model

    solver = model.load(param['solver'])
    solver.setVerbosity(param['verbosity'])
    solver.setTimeLimit(param['timelimit'])
    solver.solve()

    if solver.is_sat():
        print "SAT"
        print "Obj:", obj.get_value()
        for row in all_student_rows:
            print row
        output_assignments(all_student_rows, param['solution'])
    elif solver.is_unsat():
        print "UNSAT"
    else:
        print "Unknown"


if __name__ == '__main__':
    default = {
        'instance': 'testproblems/2/preferences.csv',
        'solution': 'njsolution-day2.csv',
        'solver': 'CPLEX',
        'verbosity': 1,
        'timelimit': 60,
    }
    param = input(default)
    main(param)

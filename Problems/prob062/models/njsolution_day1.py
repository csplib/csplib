from Numberjack import *
from validator import read_instance_day1, MAX_PREFERENCE_VALUE
import csv


def get_model_day1(all_student_preferences, company_cap_limits, num_interviews_each):
    num_students = len(all_student_preferences)
    num_companies = len(all_student_preferences[0])

    model = Model()

    # Matrix of FD variables 0..num_companies-1, for each student and each of their interviews,
    # what company they are assigned to.
    assignments = Matrix(num_students, num_interviews_each, num_companies)

    for row in assignments.row:
        # Each of the assignments for a student must be different, redundant with ordering below
        model += AllDiff(row)

        # Break symmetry by enforcing ordering on the row
        for i in range(len(row)-1):
            model += row[i] < row[i+1]

    # Cardinality constraint limiting the number of students assigned to each company
    gcc_cap_limits = dict(
        (company_id, [0, cap_limit]) for company_id, cap_limit in enumerate(company_cap_limits)
    )
    model += Gcc(assignments.flat, gcc_cap_limits)

    obj1_scaling = (MAX_PREFERENCE_VALUE * num_interviews_each) + 1  # Factor to scale objective 1
    obj1_variables, obj1_coefficients = [], []
    obj2_components = []  # a list of the preference values for each student
    for row, preferences in zip(assignments.row, all_student_preferences):
        student_obj2_vars, student_obj2_coeffs = [], []
        for company_id, preference_value in enumerate(preferences):
            for cell in row:
                # Components for the total preference value, scaled up above objective 2
                obj1_variables.append(cell == company_id)
                obj1_coefficients.append(preference_value * obj1_scaling)

                # Components for linear sum of this student's preference value
                student_obj2_vars.append(cell == company_id)
                student_obj2_coeffs.append(preference_value)

        obj2_components.append(Sum(student_obj2_vars, student_obj2_coeffs))

    # Variable for the second objective, i.e. the maximum preference value across students
    obj2 = Variable(0, obj1_scaling, 'obj2')
    for obj2_component in obj2_components:
        # At least as big as each student's preference value
        model += obj2 >= obj2_component

    obj1 = Sum(obj1_variables, obj1_coefficients)
    obj = obj1 + obj2
    model += Minimise(obj)

    return model, assignments, obj, obj1, obj2, obj1_scaling


def output_assignments(assignments, filename):
    with open(filename, "wt") as f:
        writer = csv.writer(f)
        writer.writerow(["StudentNr", "CompanyNr"])
        for student_id, student_row in enumerate(assignments.row):
            for assigned_variable in student_row:
                # +1 for one-based indexing instead of zero-based
                writer.writerow([student_id + 1, assigned_variable.get_value() + 1])


def main(param):
    all_student_preferences, company_cap_limits = read_instance_day1(param['instance'])
    num_interviews_each = param['num_interviews_each']
    model, assignments, obj, obj1, obj2, obj1_scaling = get_model_day1(
        all_student_preferences, company_cap_limits, num_interviews_each
    )

    if param['verbosity'] >= 3:
        print model

    solver = model.load(param['solver'])
    solver.setVerbosity(param['verbosity'])
    solver.solve()

    if solver.is_sat():
        print "SAT"
        print assignments
        print "Obj:", obj.get_value()
        obj1_value = obj1.get_value()
        print "Obj1:", obj1_value, "(%d * %d)" % (int(obj1_value / obj1_scaling), obj1_scaling)
        print "Obj2:", obj2.get_value()
        output_assignments(assignments, param['solution'])
    elif solver.is_unsat():
        print "UNSAT"
    else:
        print "Unknown"


if __name__ == '__main__':
    default = {
        'instance': 'testproblems/1/preferences.csv',
        'solution': 'test-njsolution.csv',
        'num_interviews_each': 3,
        'solver': 'CPLEX',
        'verbosity': 1,
    }
    param = input(default)
    main(param)

/**
 * Date: 2024
 * Compilation: g++ nqueens_z3.cpp -lz3
 */
#include "z3++.h"

using namespace z3;

int main() {
    std::cout<<"N-Queens"<<std::endl;

    // Context
    unsigned int n = 8;
    context c;

    // Variables declaration
    expr_vector x(c);
    for (unsigned i = 0; i < n; i++) {
        std::stringstream x_name;
        x_name << "x_" << i;
        x.push_back(c.int_const(x_name.str().c_str()));
    }

    solver s(c);

    // Domain definition
    for (unsigned i = 0; i < n; i++) {
        s.add(x[i] >= 0);
        s.add(x[i] < static_cast<int>(n));
    }

    // Constraints
    // Lines constraint
    s.add(distinct(x));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j) {
                // Diago constraint
                s.add(x[i] != x[j] - abs(j - i));
                s.add(x[i] != x[j] + abs(j - i));
            }
        }
    }

    std::cout << s.check() << std::endl;

    // Search
    model m = s.get_model();


    for (unsigned i = 0; i < n; i++) {
        std::cout << m.eval(x[i]) << " ";
    }
    return 0;
}

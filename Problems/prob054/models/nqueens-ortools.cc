//Copyright 2018 Maria Andreina Francisco Rodriguez (andreina@comp.nus.edu.sg)
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//http ://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.



#include "ortools/base/commandlineflags.h"
#include "ortools/constraint_solver/constraint_solveri.h"

DEFINE_int32(
	size, 0,
	"Size of the problem. If equal to 0, will test several sizes.");


namespace operations_research {

	void printSolutionArray(std::vector<IntVar*> arrayOfVars);

	void nqueens(int64 numQueens) {
		// Instantiate the solver.
		Solver solver("nQueens");
		//const int64 numQueens = 13;

		// Decision variables

		std::vector<IntVar*> board;
		solver.MakeIntVarArray(numQueens, 0, numQueens - 1, "Board", &board);

		// Constraints
		// Each queen must be on a different row and column 
		solver.AddConstraint(solver.MakeAllDifferent(board));

		// Each queen must be on a different diagonal
		std::vector<IntVar*> diag1(numQueens);
		std::vector<IntVar*> diag2(numQueens);

		for (int i = 0; i < numQueens; i++) {
			diag1[i] = solver.MakeSum(board[i], i)->Var();
			diag2[i] = solver.MakeSum(board[i], -i)->Var();
		}

		solver.AddConstraint(solver.MakeAllDifferent(diag1));
		solver.AddConstraint(solver.MakeAllDifferent(diag2));


		// Branching heuristics
		DecisionBuilder* const db = solver.MakePhase(board,
			Solver::CHOOSE_MIN_SIZE,
			Solver::ASSIGN_CENTER_VALUE);

		// Search!
		solver.Solve(db);

		int numSolutions = 0;

		// Print
		while (solver.NextSolution()) {
			numSolutions++;
			std::cout << "Solution " << numSolutions << " :";
			printSolutionArray(board);
		}

		const int64 elapsedTime = solver.wall_time();

		std::cout << "Total number of solutions: " << numSolutions << "\n";
		std::cout << "Total elapsed time: " << elapsedTime << " milliseconds.\n";

	}

	// Prints the values of an array of variables between squre brakets 
	void printSolutionArray(std::vector<IntVar*> arrayOfVars) {
		int i = 0;
		int size = arrayOfVars.size();
		std::cout << "[";
		while (i < size) {
			std::cout << arrayOfVars[i]->Value();
			i++;
		}
		std::cout << "]\n";
	}
}



int main(int argc, char** argv) {
	gflags::ParseCommandLineFlags(&argc, &argv, true);
	if (FLAGS_size != 0) {
		std::cout << "Solving for a board of size: " << FLAGS_size << "\n";
		operations_research::nqueens(FLAGS_size);
	}
	else {
		for (int size = 4; size < 10; size++) {
			std::cout << "Solving for a board of size: " << size << "\n";
			operations_research::nqueens(size);
		}
	}
	return 0;
}
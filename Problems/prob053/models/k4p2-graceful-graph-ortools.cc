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

#include "ortools/constraint_solver/constraint_solveri.h"

namespace operations_research {

	void printSolutionArray(std::vector<IntVar*> arrayOfVars);

	void gracefulGraph() {
		// Instantiate the solver.
		Solver solver("k4p2");
		const int64 numEdges = 16;

		// Create nodes
		std::vector<IntVar*> front;
		solver.MakeIntVarArray(4, 0, numEdges, "nodes", &front);

		// Create "back" nodes
		std::vector<IntVar*> back;
		solver.MakeIntVarArray(4, 0, numEdges, "Back nodes", &back);

		std::vector<IntVar*> allnodes;
		allnodes.insert(allnodes.end(), front.begin(), front.end());
		allnodes.insert(allnodes.end(), back.begin(), back.end());

		// The label of each node must be different
		solver.AddConstraint(solver.MakeAllDifferent(allnodes));

		// Edges
		std::vector<IntVar*> edges;
		solver.MakeIntVarArray(numEdges, 1, numEdges, "Edges", &edges);

		// Front
		solver.AddConstraint(solver.MakeEquality(edges[0], solver.MakeAbs(solver.MakeDifference(front[0], front[1]))));
		solver.AddConstraint(solver.MakeEquality(edges[1], solver.MakeAbs(solver.MakeDifference(front[0], front[2]))));
		solver.AddConstraint(solver.MakeEquality(edges[2], solver.MakeAbs(solver.MakeDifference(front[1], front[3]))));
		solver.AddConstraint(solver.MakeEquality(edges[3], solver.MakeAbs(solver.MakeDifference(front[2], front[3]))));

		// Front diagonals
		solver.AddConstraint(solver.MakeEquality(edges[4], solver.MakeAbs(solver.MakeDifference(front[0], front[3]))));
		solver.AddConstraint(solver.MakeEquality(edges[5], solver.MakeAbs(solver.MakeDifference(front[1], front[2]))));

		// Back
		solver.AddConstraint(solver.MakeEquality(edges[6], solver.MakeAbs(solver.MakeDifference(back[0], back[1]))));
		solver.AddConstraint(solver.MakeEquality(edges[7], solver.MakeAbs(solver.MakeDifference(back[0], back[2]))));
		solver.AddConstraint(solver.MakeEquality(edges[8], solver.MakeAbs(solver.MakeDifference(back[1], back[3]))));
		solver.AddConstraint(solver.MakeEquality(edges[9], solver.MakeAbs(solver.MakeDifference(back[2], back[3]))));

		// Back diagonals
		solver.AddConstraint(solver.MakeEquality(edges[10], solver.MakeAbs(solver.MakeDifference(back[0], back[3]))));
		solver.AddConstraint(solver.MakeEquality(edges[11], solver.MakeAbs(solver.MakeDifference(back[1], back[2]))));

		// Connecting edges
		solver.AddConstraint(solver.MakeEquality(edges[12], solver.MakeAbs(solver.MakeDifference(front[0], back[0]))));
		solver.AddConstraint(solver.MakeEquality(edges[13], solver.MakeAbs(solver.MakeDifference(front[1], back[1]))));
		solver.AddConstraint(solver.MakeEquality(edges[14], solver.MakeAbs(solver.MakeDifference(front[2], back[2]))));
		solver.AddConstraint(solver.MakeEquality(edges[15], solver.MakeAbs(solver.MakeDifference(front[3], back[3]))));

		//MakeAbsEquality

		// The labels of all the edges must be different
		solver.AddConstraint(solver.MakeAllDifferent(edges));

		std::vector<IntVar*> allvars;

		allvars.insert(allvars.end(), edges.begin(), edges.end());
		allvars.insert(allvars.end(), allnodes.begin(), allnodes.end());


		// Branching heuristics
		DecisionBuilder* const db1 = solver.MakePhase(allnodes,
			Solver::CHOOSE_MAX_REGRET_ON_MIN,//CHOOSE_MIN_SIZE,
			Solver::ASSIGN_MIN_VALUE);

		// Search!
		solver.Solve(db1);

		int numSolutions = 0;

		// Print
		while (solver.NextSolution()) {
			numSolutions++;
			std::cout << "Solution " << numSolutions << " :";
			printSolutionArray(allnodes);
		}

		const int64 elapsedTime = solver.wall_time();

		std::cout << "Total number of solutions: " << numSolutions << "\n";
		std::cout << "Total elapsed time: " << elapsedTime << " milliseconds.\n";


	} // gracefulGraph

	  // Prints the values of a vector of IntVar between square brakets.
	  // Elements of the vector are separated by white spaces.
	void printSolutionArray(std::vector<IntVar*> arrayOfVars) {
		int i = 0;
		int size = arrayOfVars.size();
		std::cout << "[ ";
		while (i < size) {
			std::cout << arrayOfVars[i]->Value() << " ";
			i++;
		}
		std::cout << "]\n";
	}

} // namespace operations_research

int main(int argc, char** argv) {
	operations_research::gracefulGraph();
	return 0;
} // main
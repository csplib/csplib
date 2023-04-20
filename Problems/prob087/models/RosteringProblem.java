
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.IntVar;

/**
 * @author Sven Löffler, BTU Cottbus-Senftenberg
 * @version 1.0 A Rotating Rostering Problem see the specification in the CSPLib
 *          (https://www.csplib.org/) for more details.
 */
public class RosteringProblem {

	// Early, late, night + day off
	private static final int NUMBER_OF_SHIFTS = 3;

	// The number of days of each week
	private static final int DAYS_PER_WEEK = 7;

	/**
	 * The main method which set the parameters and starts the rostering problem.
	 * The parameters create the Rostering-8-M-2-4 instance with M presented in the
	 * CSPLib.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {

		// w is the number of weeks (here 8)
		int w = 8;

		// the shift requirement matrix, a 4 times 7 matrix, including at M_i,j the
		// needed employees for shift (i-1) and day j (shift types are 0-indexed, the
		// matrix M is 1-indexed)
		int[][] m = { { 2, 2, 2, 2 }, { 2, 2, 2, 2 }, { 2, 2, 2, 2 }, { 2, 2, 2, 2 }, { 2, 2, 2, 2 }, { 4, 2, 1, 1 },
				{ 4, 2, 1, 1 } };

		// is the minimum number of days in a row with the same shift (here 2)
		int sMin = 2;

		// is the maximum number of days in a row with the same shift (here 4)
		int sMax = 4;

		plan(w, m, sMin, sMax);

	}

	/**
	 * Create and solve the Rostering problem given by the parameters.
	 * 
	 * @param weeks         is the number of weeks
	 * @param shiftCapacity a 4 times 7 matrix, including at M_i,j the needed
	 *                      employees for shift (i-1) and day j (shift types are
	 *                      0-indexed, the matrix M is 1-indexed)
	 * @param sMin          is the minimum number of days in a row with the same
	 *                      shift
	 * @param sMax          is the maximum number of days in a row with the same
	 *                      shift
	 */
	public static void plan(int weeks, int[][] shiftCapacity, int sMin, int sMax) {

		// 1- Create the model
		Model model = new Model("Choco4 Model");

		// 2- Create the variables
		IntVar[][] plan2D = model.intVarMatrix("Plan2D", weeks, DAYS_PER_WEEK, 0, NUMBER_OF_SHIFTS);
		IntVar[] plan = flatten(plan2D);
		IntVar[][] plan2DT = transpose(plan2D);

		// 3- Post constraints
		staffingCapacity(model, plan2DT, shiftCapacity);

		// values for n = 7
		int saturday = 5;
		int sunday = 6;

		weekdaysWithSameShift(model, plan2D, saturday, sunday);

		minimumShiftLengthIs(model, plan, sMin);

		maximumShiftLength(model, plan, sMax);

		minNFreeDaysInMWeeks(model, plan, 2, 2, weeks);

		admissibleShiftOrders(model, plan);

		model.getSolver().limitSolution(1);

		// 4- start the solver
		while (model.getSolver().solve()) {

			System.out.println(model.getSolver().getSolutionCount());

			// show current solution
			for (int i = 0; i < plan2D.length; i++) {
				for (int j = 0; j < plan2D[0].length; j++) {
					System.out.print(plan2D[i][j].getValue() + ", ");
				}
				System.out.println();
			}
			System.out.println();
		}

		model.getSolver().printStatistics();

	}

	/**
	 * flatten a regular 2D array into an 1D array
	 * 
	 * @param array the 2D array
	 * @return the coresponding 1D array
	 */
	public static IntVar[] flatten(IntVar[][] array) {
		List<IntVar> list = new ArrayList<IntVar>();
		for (IntVar[] i : array) {
			list.addAll(Arrays.asList(i));
		}
		return (IntVar[]) list.toArray(new IntVar[0]);
	}

	/**
	 * transpose an array
	 * 
	 * @param array the original array
	 * @return the transpoesd array
	 */
	public static IntVar[][] transpose(IntVar[][] array) {
		int rows = array.length;
		int cols = array[0].length;
		IntVar[][] arrayT = new IntVar[cols][rows];
		for (int i = 0; i < cols; i++) {
			for (int j = 0; j < rows; j++) {
				arrayT[i][j] = array[j][i];
			}
		}
		return arrayT;
	}

	/**
	 * The CshiftRepetitions 2 constraints
	 *
	 * @param model The model to which the constraints are added
	 * @param plan  The array of variables of the model
	 * @param sMax  The maximum number of shift repetitions in a row
	 */
	public static void maximumShiftLength(Model model, IntVar[] plan, int sMax) {
		int days = plan.length;
		for (int currentDay = 0; currentDay < days; currentDay++) {

			IntVar[] equalDays = new IntVar[sMax];
			for (int i = 0; i < sMax; i++) {
				equalDays[i] = plan[(currentDay + i) % days];
			}

			model.ifThen(model.allEqual(equalDays),
					model.arithm(plan[currentDay], "!=", plan[(currentDay + sMax) % days]));
		}
	}

	/**
	 * Create the CshiftRepetitions 1 constraints
	 *
	 * @param model The model to which the constraints are added
	 * @param plan  The array of variables of the model
	 * @param sMin  The minimum number of shift repetitions in a row
	 */
	public static void minimumShiftLengthIs(Model model, IntVar[] plan, int sMin) {
		int days = plan.length;
		for (int currentDay = 0; currentDay < days; currentDay++) {

			IntVar[] equalDays = new IntVar[sMin];
			for (int i = 0; i < sMin; i++) {
				equalDays[i] = plan[(currentDay - i + days) % days];
			}

			model.ifThen(model.arithm(plan[currentDay], "!=", plan[(currentDay + 1) % days]),
					model.allEqual(equalDays));
		}
	}

	/**
	 * Create the CrestDays constraints
	 *
	 * @param model         The model to which the constraints are added
	 * @param plan          The array of variables of the model
	 * @param nDays         The needed number of rest days
	 * @param mWeeks        The number of weeks in which the rest days are needed
	 * @param numberOfWeeks The number of weeks of the problem
	 */
	public static void minNFreeDaysInMWeeks(Model model, IntVar[] plan, int nDays, int mWeeks, int numberOfWeeks) {

		// build data structure and propagate constraint to express, that
		// for each two weeks there are 2 days free at minimum

		int days = DAYS_PER_WEEK * mWeeks;

		for (int currentDay = 0; currentDay < DAYS_PER_WEEK; currentDay++) {
			IntVar[] arrayOfTwoWeeks = new IntVar[DAYS_PER_WEEK * mWeeks];

			for (int day = 0; day < mWeeks * DAYS_PER_WEEK; day++) {
				arrayOfTwoWeeks[day] = plan[(currentDay * DAYS_PER_WEEK + day) % days];
			}

			model.count(0, arrayOfTwoWeeks, model.intVar(nDays, DAYS_PER_WEEK * mWeeks)).post();
		}

	}

	/**
	 * Create the CequalDays constraints
	 *
	 * @param model     The model to which the constraints are added
	 * @param plan2D    The two dimensional array of variables of the model
	 * @param indexDayA The first index of day of every week which should be equal
	 * @param indexDayB The second index of day of every week which should be equal
	 */
	public static void weekdaysWithSameShift(Model model, IntVar[][] plan2D, int indexDayA, int indexDayB) {
		// a stuff member has always the same shift on weekdays a and b
		int weeks = plan2D.length;
		for (int currentWeekNumber = 0; currentWeekNumber < weeks; currentWeekNumber++) {
			model.arithm(plan2D[currentWeekNumber][indexDayA], "=", plan2D[currentWeekNumber][indexDayB]).post();
		}
	}

	/**
	 * Create the CshiftOrder constraints
	 *
	 * @param model The model to which the constraints are added
	 * @param plan  The array of variables of the model
	 */
	public static void admissibleShiftOrders(Model model, IntVar[] plan) {
		int days = plan.length;

		// propagate followRelation constraint on all days
		for (int currentDay = 0; currentDay < days; currentDay++) {
			model.or(model.arithm(plan[currentDay], "<=", plan[(currentDay + 1) % days]),
					model.arithm(plan[(currentDay + 1) % days], "=", 0)).post();
		}
	}

	/**
	 * Create the CshiftRequirements constraints
	 *
	 * @param model         The model to which the constraints are added
	 * @param plan2DT       The two dimensional array of variables of the model
	 * @param shiftCapacity The shift capacity matrix
	 */
	public static void staffingCapacity(Model model, IntVar[][] plan2DT, int[][] shiftCapacity) {

		int daysPerWeek = plan2DT.length;

		int[] shiftValues = new int[NUMBER_OF_SHIFTS + 1];
		for (int i = 0; i <= NUMBER_OF_SHIFTS; i++) {
			shiftValues[i] = i;
		}

		IntVar[][] luDaysIntVar = new IntVar[shiftCapacity.length][shiftCapacity[0].length];

		for (int x = 0; x < shiftCapacity.length; x++) {
			for (int y = 0; y < shiftCapacity[0].length; y++) {
				luDaysIntVar[x][y] = model.intVar(shiftCapacity[x][y], shiftCapacity[x][y]);
			}
		}

		// propagate constraints on number of staff per shift and weekday
		for (int i = 0; i < daysPerWeek - 1; i++) {
			model.globalCardinality(plan2DT[i], shiftValues, luDaysIntVar[i], true).post();
		}
	}

}

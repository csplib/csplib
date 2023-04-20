import java.io.FileWriter;
import java.io.IOException;
import java.util.Random ;
import java.util.*;

// An instance generator for Plotting.
// Takes gridWidth, gridHeight and nbColours
// Can generate all instances with these characteristics or
// a random single grid with a variety of blocks remaining/steps.
// Enumerating all instances is generally too large a collection.
// Output labelled either with seed or a grid id for complete enumeration.

public final class PlottingInstanceGenerator {
    static int gridWidth, gridHeight, nbColours, goalBlocks ;
    static int gridId = 0 ;
    static int seed ;
    static Random random ;

    static String instanceName="";  
    static List<String> PDDLFiles=new ArrayList<String>();  
    static List<String> EPrimeFiles=new ArrayList<String>();  

    public static void main(String[] args) {
        // parse args
        if (args.length < 5 || args.length > 5) {
            System.out.println("Usage: java PIG <width> <height> <colours> <goalBlocks> <seed>") ;
            return ;
        }
        gridWidth = Integer.parseInt(args[0]) ;
        gridHeight = Integer.parseInt(args[1]) ;
        nbColours = Integer.parseInt(args[2]) ;
        goalBlocks = Integer.parseInt(args[3]) ;
        seed = Integer.parseInt(args[4]) ;

        instanceName = "Plotting_"+gridHeight+"x"+gridWidth+"_"+
                nbColours+"colours_"+
                seed+"seed_"+
                goalBlocks+"goal";
    
        // generate the file
        random = new Random(seed) ;
        generateSingleRandom(0,0, new int[gridHeight][gridWidth]) ;

        // output the generated files
        try {
            FileWriter fw = new FileWriter(instanceName+".instance") ;
            fw.write("pddl:");
            for (int i = 0; i < PDDLFiles.size(); i++) {
                fw.write(PDDLFiles.get(i));
                if (i+1 != PDDLFiles.size()) fw.write(",");
            }
            fw.write("\n");
            fw.write("eprime:");
            for (int i = 0; i < EPrimeFiles.size(); i++) {
                fw.write(EPrimeFiles.get(i));
                if (i+1 != EPrimeFiles.size()) fw.write(",");
            }
            fw.write("\n");
            fw.close();
        }
        catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    // Main usage, producing the simplest random instance.
    // We are given everything: grid size, colours, seed and goal.
    private static void generateSingleRandom(int row, int col, int[][] grid) {

        if (row >= gridHeight) {
            // Each step must remove at least one block, so number of steps
            //   is at most grid size - goalBlocks.
            for (int noSteps = 1;
                    noSteps <= gridWidth*gridHeight-goalBlocks;
                    noSteps++) {
                EPrimeFiles.add(writeEPrime(grid, noSteps));
            }
            PDDLFiles.add(writePDDL(grid));
            return ;
        }
        if (col >= gridWidth) {
            generateSingleRandom(row+1, 0, grid) ;
            return ;
        }
        grid[row][col] = random.nextInt(nbColours)+1 ;
        generateSingleRandom(row, col+1, grid) ;
    }

    // Write a PDDL instance.
    private static String writePDDL(int[][] grid) {

        String idfile = instanceName+".pddl";
        try {
            FileWriter fw = new FileWriter(idfile) ;

            // header
            fw.write("(define (problem "+idfile+")\n");
            fw.write("(:domain plotting)\n");

            // we need to define the numbers and colours
            fw.write("(:objects\n");
            // numbers
            int largestNum = Math.max(gridHeight, gridWidth);
            for(int currentNum = 1; currentNum <= largestNum; currentNum++){
                fw.write("n"+currentNum+" - number\n");
            }
            // colours
            for(int currentColour = 1; currentColour <= nbColours; currentColour++){
                fw.write("c"+currentColour+" - colour\n");
            }
            fw.write(")\n"); // closing objects

            // in the init part we need to state:
            // state of the grid
            fw.write("(:init\n");
            for (int row = 1; row <= gridHeight; row++) {
                for (int col = 1; col <= gridWidth; col++) {
                    fw.write("(coloured n"+(row)+ " n"+(col)+" c"+ grid[row-1][col-1]+")\n");
                }
            }
            // The hand always starts as a wildcard
            fw.write("(hand wildcard)\n");

            // numeric axioms 
            // successor
            for (int num = 1; num < largestNum; num++) {
                fw.write("(succ n"+ (num+1) +" n" + num + ")\n");
            }

            // predecessor
            for (int num = 1; num < largestNum; num++) {
                fw.write("(pred n"+ num +" n" + (num+1) + ")\n");
            }

            // less-than
            for (int num = 1; num <= largestNum; num++) {
                for (int num2 = num+1; num2 <= largestNum; num2++) {
                    fw.write("(lt n"+ num +" n" + num2 + ")\n");
                }
            }

            // greater-than
            for (int num = largestNum; num > 1; num--) {
                for (int num2 = num-1; num2 > 0; num2--) {
                    fw.write("(gt n"+ num +" n" + num2 + ")\n");
                }
            }

            // helpers
            fw.write("(isfirstcolumn n1)\n");
            fw.write("(islastcolumn n"+largestNum+")\n");
            fw.write("(istoprow n1)\n");
            fw.write("(isbottomrow n"+largestNum+")\n");

            // Distances
            for (int num = 1; num < largestNum; num++) {
                for (int num2 = 1; num2 < largestNum; num2++) {
                    if( Math.abs(num-num2) != 0)
                        fw.write("(distance n"+num+" n"+num2+" n"+Math.abs(num-num2)+")\n");
                }
            }
            fw.write(")\n"); // closing init

            fw.write(encodePDDLGoal()); 
            fw.write(")\n"); // closing define problem
            fw.close() ;
        }
        catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
        return idfile;
    }

    // Compact formulation of the goal
    //
    //(:goal  ;; at most 2 cells are not null, i.e., g=2
    //(exists (?x1 ?x2 ?y1 ?y2 - number)
    //    (and
    //        (or     ;; cell 1 != cell 2.
    //            (not (= ?x1 ?x2))
    //            (not (= ?y1 ?y2)))
    //        (forall (?x3 ?y3 - number)
    //            (or ; Or is one of cell 1 or cell 2, or is null
    //                (and (= ?x1 ?x3) (= ?y1 ?y3))
    //                (and (= ?x2 ?x3) (= ?y2 ?y3))
    //                (coloured ?x3 ?y3 null))))))
    private static String encodePDDLGoal(){
        String goal = "(:goal\n";

        switch (goalBlocks) {
            case 0: 
                goal += "(forall (?x1 ?y1 - number)\n";
                goal += "    (coloured ?x1 ?y1 null))\n";
                break;
            case 1: 
                goal += "(exists (?x1 ?y1 - number)\n";
                goal += "        (forall (?x2 ?y2 - number)\n";
                goal += "            (or\n";
                goal += "                (and (= ?x1 ?x2) (= ?y1 ?y2))\n";
                goal += "                (coloured ?x2 ?y2 null))))\n";
                break;
            default:
                goal += "(exists (";
                for(int i = 1; i < goalBlocks; i++){
                    goal += "?x"+i+" ?y"+i+" ";
                }
                goal += "- number)\n"+ "(and\n"; 
                for (int num = 1; num < goalBlocks; num++) {
                    for (int num2 = num+1; num2 < goalBlocks; num2++) {
                        goal += "(or (not (= ?x"+num+" ?x"+num2+")) (not (= ?y"+num+" ?y"+num2+")))\n";
                    }
                }
                goal += "(forall (?x"+goalBlocks+" ?y"+goalBlocks+" - number) (or\n";
                for (int num = 1; num < goalBlocks; num++) {
                    goal += "    (and (= ?x"+num+" ?x"+goalBlocks+") (= ?y"+num+" ?y"+goalBlocks+"))\n";
                }
                goal += "(coloured ?x"+goalBlocks+" ?y"+goalBlocks+" null)))))\n";
                break;
        }
        goal += ")\n"; // closing goal

        return goal;
    }

    // write an Essence Prime file.
    private static String writeEPrime(int[][] grid, int noSteps) {
        String paddedSteps = String.format("%03d", noSteps);
        String idfile = instanceName+"_"+paddedSteps+"steps.param";
        try {
            FileWriter fw = new FileWriter(idfile) ;
            fw.write("language ESSENCE' 1.0\n") ;
            fw.write("letting initGrid be [\n") ;
            for (int row = 0; row < gridHeight; row++) {
                fw.write("[") ;
                for (int col = 0; col < gridWidth; col++) {
                    fw.write(""+grid[row][col]) ;
                    if (col < gridWidth-1)
                        fw.write(", ") ;
                }
                if (row < gridHeight-1)
                    fw.write("], \n") ;
                else
                    fw.write("]\n") ;
            }
            fw.write("]\n") ;
            fw.write("letting goalBlocksRemaining be "+
                    goalBlocks+"\n") ;
            fw.write("letting noSteps be "+noSteps+"\n") ;
            fw.close() ;
        }
        catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
        return idfile;
    }
}

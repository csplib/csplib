/*
 * Copyright 2016 Sevle.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
  
/*  Fractions problem with JaCoP.
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  
  Created as an assignment for Advanced Artificial Intelligence graduate class of 2016 (http://www.di.uoa.gr/eng/node/1449)
*/

import org.jacop.constraints.Alldifferent;
import org.jacop.constraints.Constraint;
import org.jacop.constraints.SumWeight;
import org.jacop.constraints.XgtY;
import org.jacop.constraints.XmulYeqZ;
import org.jacop.constraints.XplusYplusQeqZ;
import org.jacop.core.IntVar;
import org.jacop.core.Store;
import org.jacop.search.DepthFirstSearch;
import org.jacop.search.IndomainMin;
import org.jacop.search.InputOrderSelect;
import org.jacop.search.PrintOutListener;
import org.jacop.search.Search;
import org.jacop.search.SelectChoicePoint;

public class Three_fractions_solver {      
    public static void main (String[] args) { 
        Store store = new Store();  // define FD store 
        
        // define finite domain variables 
        IntVar A = new IntVar(store, "A", 1, 9);
        IntVar B = new IntVar(store, "B", 1, 9);
        IntVar C = new IntVar(store, "C", 1, 9);
        IntVar D = new IntVar(store, "D", 1, 9);
        IntVar E = new IntVar(store, "E", 1, 9);
        IntVar F = new IntVar(store, "F", 1, 9);
        IntVar G = new IntVar(store, "G", 1, 9);
        IntVar H = new IntVar(store, "H", 1, 9);
        IntVar I = new IntVar(store, "I", 1, 9);
        
        //A,B,C,D,E,F,G,H,I all different
        IntVar[] digits = {A,B,C,D,E,F,G,H,I};
        IntVar[] v1 = {B, C};
        IntVar[] v2 = {E, F};
        IntVar[] v3 = {H, I};                      
        
        Constraint ctr = new Alldifferent(digits);
        store.impose(ctr);
         
        //10*B+C,10*E+F,10*H+I
        IntVar value_BC = new IntVar(store, "D1", 0, 81);
        IntVar value_EF = new IntVar(store, "D2", 0, 81);
        IntVar value_HI = new IntVar(store, "D3", 0, 81);
         
        int[] weights = { 10, 1 };
        store.impose(new SumWeight (v1, weights, value_BC));
        store.impose(new SumWeight (v2, weights, value_EF));
        store.impose(new SumWeight (v3, weights, value_HI));

        //BC*EF,HI*BC,BC*EF 
        IntVar value_BCEF = new IntVar(store, "D1-2", 0, 6561);
        IntVar value_EFHI = new IntVar(store, "D2-3", 0, 6561);
        IntVar value_HIBC = new IntVar(store, "D1-3", 0, 6561);
        
        store.impose(new XmulYeqZ(value_BC, value_EF, value_BCEF));
        store.impose(new XmulYeqZ(value_EF, value_HI, value_EFHI));
        store.impose(new XmulYeqZ(value_HI, value_BC, value_HIBC));
        
        //HI*BC*EF
        IntVar value_BCEFHI = new IntVar(store, "D1-2-3", 0, 531441);
        store.impose(new XmulYeqZ(value_HIBC, value_EF, value_BCEFHI));
        
        //A*EF*HI,D*HI*BC,G*BC*EF
        IntVar value_AEFHI = new IntVar(store, "A2-3", 0, 531441);
        IntVar value_DHIBC = new IntVar(store, "B1-3", 0, 531441);
        IntVar value_GBCEF = new IntVar(store, "C1-2", 0, 531441);         

        store.impose(new XmulYeqZ(A, value_EFHI, value_AEFHI));
        store.impose(new XmulYeqZ(D, value_HIBC, value_DHIBC));
        store.impose(new XmulYeqZ(G, value_BCEF, value_GBCEF));

        //(A*EF*HI)+(D*HI*BC)+(G*BC*EF)=(EF*HI*BC)
        store.impose(new XplusYplusQeqZ(value_AEFHI, value_DHIBC, value_GBCEF,value_BCEFHI));

        //Break symetries
        store.impose(new XgtY(G, D));
        store.impose(new XgtY(D, A));
        
        // search for a solution and print results 
        Search<IntVar> search = new DepthFirstSearch<>(); 
        SelectChoicePoint<IntVar> select = new InputOrderSelect<>(store, digits, new IndomainMin<>()); 
        search.setSolutionListener(new PrintOutListener<>()); 
        search.getSolutionListener().searchAll(true); 
        search.labeling(store, select);      
    } 
}

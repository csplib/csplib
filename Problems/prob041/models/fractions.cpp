/*
  
  Fractions problem in Gecode.

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/fractions.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/fractions.pl
  * ECLiPSE: http://www.hakank.org/eclipse/fractions.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

// Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class Fractions : public Script {
protected:

  const static int n = 9;

  IntVarArray Vars;

public:

  Fractions(const Options& opt) 
    : 
    Vars(*this, n, 1, n)
  {

    IntVar
      A(Vars[0]),
      B(Vars[1]),
      C(Vars[2]),
      D(Vars[3]),
      E(Vars[4]),
      F(Vars[5]),
      G(Vars[6]),
      H(Vars[7]),
      I(Vars[8]);

    IntVar D1(*this, 1, 81);
    IntVar D2(*this, 1, 81);
    IntVar D3(*this, 1, 81);


    distinct(*this, Vars);

    rel(*this, 
        D1 == 10*B+C &&
        D2 == 10*E+F &&
        D3 == 10*H+I &&
        A*D2*D3 + D*D1*D3 + G*D1*D2 == D1*D2*D3 &&

        // break the symmetry
        A*D2 >= D*D1 &&
        D*D3 >= G*D2 &&

        //redundant constraints
        3*A >= D1 &&
        3*G <= D2
        );


    // branching
    branch(*this, Vars, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Vars: " << Vars << endl;
  }


  // Constructor for cloning s
  Fractions(bool share, Fractions& s) : Script(share,s) {
    Vars.update(*this, share, s.Vars);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Fractions(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Fractions");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Fractions,DFS,Options>(opt);
    
  return 0;
}



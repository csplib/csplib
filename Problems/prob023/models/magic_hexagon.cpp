/*
  
  Magic Hexagon in Gecode.

  Prob023: Magic Hexagon
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
  http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/magic_hexagon.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/magic_hexagon.ecl
  * ECLiPSe: http://www.hakank.org/eclipse/magic_hexagon.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class MagicHexagon : public Script {
protected:

  static const int len = 19;

  IntVarArray x;

public:

  MagicHexagon(const Options& opt) 
    : 
    x(*this, len, 1, len)
  {

    IntVar
      a(x[0]),
      b(x[1]),
      c(x[2]),
      d(x[3]),
      e(x[4]),
      f(x[5]),
      g(x[6]),
      h(x[7]),
      i(x[8]),
      j(x[9]),
      k(x[10]),
      l(x[11]),
      m(x[12]),
      n(x[13]),
      o(x[14]),
      p(x[15]),
      q(x[16]),
      r(x[17]),
      s(x[18]);
    
    distinct(*this, x, opt.icl());

    // Not very beautiful, but experimental...
    rel(*this,
        a + b + c ==  38 &&
        d + e + f + g ==  38 &&
        h + i + j + k + l ==  38 && 
        m + n + o + p ==  38 && 
        q + r + s ==  38 && 
        a + d + h ==  38 && 
        b + e + i + m ==  38 && 
        c + f + j + n + q ==  38 && 
        g + k + o + r ==  38 && 
        l + p + s ==  38 && 
        c + g + l ==  38 && 
        b + f + k + p ==  38 && 
        a + e + j + o + s ==  38 && 
        d + i + n + r ==  38 && 
        h + m + q ==  38 && 
        
        a < c &&
        a < h &&
        a < l &&
        a < q &&
        a < s &&
        c < h
        );

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
    os << endl;

  }


  // Constructor for cloning s
  MagicHexagon(bool share, MagicHexagon& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MagicHexagon(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("MagicHexagon");

  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);

  Script::run<MagicHexagon,DFS,Options>(opt);
    
  return 0;
}



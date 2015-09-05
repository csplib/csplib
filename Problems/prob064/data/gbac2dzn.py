import sys

def transform(infile,outfile,solfile=None):
    with open(infile,'r') as f:
        with open(outfile,'w') as out:
            def readInts(txt,n):
                line = f.readline().split(":")
                if line[0]==txt:
                    return list(map(int,line[1].split()))
                else:
                    print(line,"!=",txt)
            def readInt(txt):
                line = f.readline().split(":")
                if line[0]==txt:
                    return int(line[1])
                else:
                    print(line,"!=",txt)
            def readHeader(txt):
                line = f.readline().split(":")
                if line[0]==txt:
                    return line[1]
                else:
                    print(line,"!=",txt)
            def readEmpty():
                line = f.readline().strip()
                if line != "":
                    print(line,"!= ")

            print("%",readHeader("DESCRIPTION"),file=out)
            ny = readInt("YEARS")
            npy = readInt("PERIODS_PER_YEAR")
            print("n_periods = ",ny*npy,";",file=out)
            nc = readInt("NUM_COURSES")
            print("n_courses = ",nc,";",file=out)
            nq = readInt("NUM_CURRICULA")
            print("n_curricula = ",nq,";",file=out)
            mm = readInts("MIN_MAX_COURSE_LOAD_PER_PERIOD",2)
            print("min_courses = ",mm[0],";",file=out)
            print("max_courses = ",mm[1],";",file=out)            
            np = readInt("NUM_PRECEDENCES")
            print("n_precedences = ",np,";",file=out)
            nu = readInt("NUM_UNDESIRED_PERIODS")
            print("n_undesirables = ",nu*ny,";",file=out)
            print("w1 = 1;",file=out)
            print("w2 = 1;",file=out)
            readEmpty()
            readHeader("COURSES")
            courses = {}
            loads = []
            for i in range(1,nc+1):
                line = f.readline().split()
                course = line[0]
                load = int(line[1])
                courses[course] = i
                loads += [load]
            print("course_load = ",loads,";",file=out)
            readEmpty()
            readHeader("CURRICULA")
            curricula = []
            for i in range(1,nq+1):
                line = f.readline().split()
                cur = line[0]
                n_c = int(line[1])
                curricula += [set(map(lambda c: courses[c],line[2:]))]
            print("courses_of = ",curricula,";",file=out)
            readEmpty()
            readHeader("PRECEDENCES")
            precedences = []
            for i in range(1,np+1):
                line = f.readline().split()
                precedences += [courses[line[0]],courses[line[1]]]
            print("precedes = array2d(precedences,1..2,",precedences,");",file=out)
            readEmpty()
            readHeader("UNDESIRED_PERIODS")
            undesired = []
            for i in range(1,nu+1):
                line = f.readline().split()
                for y in range(0,ny):
                    undesired += [courses[line[0]],int(line[1])+1+y*npy]
            print("undesirable = array2d(undesirables,1..2,",undesired,");",file=out)
            readEmpty()
            print("%",courses,file=out)
            print(f.readline())
            if not solfile is None:
                with open(solfile,'r') as s:
                    fixed = []
                    for i in range(nc):
                        line = s.readline().split()
                        fixed += [courses[line[0]],int(line[1])+1]
                    print("n_fixed = n_courses;",file=out)
                    print("fixed = array2d(courses,1..2,",fixed,");",file=out)
            else:
                print("n_fixed = 0;",file=out)
                print("fixed = array2d(1..0,1..2,[]);",file=out)
          
if __name__ == "__main__":
    if len(sys.argv) > 2:
        fname = sys.argv[1]
        transform(fname,fname+".dzn",sys.argv[2])
    elif len(sys.argv) > 1:
        fname = sys.argv[1]
        transform(fname,fname+".dzn")
    else:
        print("give the name of the file to transform")
            

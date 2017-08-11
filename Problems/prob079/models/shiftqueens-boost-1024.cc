// The MIT License (MIT)
// 
// Copyright (c) 2015 Christopher Jefferson
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>
#include <signal.h>

#include <boost/multiprecision/cpp_int.hpp>

using namespace boost::multiprecision;

//  g++ shiftqueens-boost-1024.cc -I/usr/local/Cellar/boost/1.57.0/include/ -o shiftqueens-boost-1024 -O3
// for i in `ls`; do  ../../shiftqueens-boost-1024 $(cat $i | tr -cd " 0-9") > ../results.d25/$i.info; done

//#define PRINT_INFO

#ifdef PRINT_INFO
#define INFO(...) printf(__VA_ARGS__);
#define PAD(x) pad(x);
#define PRINTBITS(x) printBits(x)
#else
#define INFO(...)
#define PAD(x)
#define PRINTBITS(x)
#endif

//#define PRINT_SOL

#ifdef PRINT_SOL
static int sol[100];
#endif

#include <sys/resource.h>
#include <sys/times.h>

double gettime() {
	struct rusage rusage;
	if ( getrusage( RUSAGE_SELF, &rusage ) != -1 )
		return (double)rusage.ru_utime.tv_sec +
			(double)rusage.ru_utime.tv_usec / 1000000.0;
	
	std::cerr << "Fatal error in reading time\n";
	exit(1);
}

volatile int trig = false;

void trigger_function(int /* signum */) {
  trig = true;
}

void setup_timelimit(int timeout) {
	signal(SIGXCPU, trigger_function);
	signal(SIGALRM, trigger_function);
	rlimit lim;
	lim.rlim_cur = timeout;
	lim.rlim_max = timeout + 5;
	setrlimit(RLIMIT_CPU, &lim);
}

long long node_count = 0;

static int1024_t all;
static const int1024_t zero = 0;
static const int1024_t one = 1;
static int depth;
static int count = 0;

int fillLeft[1024];
int fillRight[1024];
int skipDepth[1024];

#ifdef PRINT_SOL
void print_sol()
{
	for(int i = 0; i < depth; ++i)
	{
		if(skipDepth[i])
			printf("-");
		else
			printf("|");
		for(int j = 0; j < depth; ++j)
		{
			if(1<<j == sol[i])
				printf("#|");
			else
			  printf(" |");
		}
		printf("\n");
		for(int j = 0; j < depth; ++j)
		printf("--");
		printf("-\n");
	}
	printf("\n");
}
#endif

#ifdef PRINT_INFO
void printBits(int1024_t bits)
{
	for(int i = 0; i < depth; ++i) {
		printf(bits & one ? "1" : "0");
		bits = bits >> 1;
	}
}
#endif

void pad(int curDepth)
{ 
		for(int i=0; i < curDepth; ++i)
			printf(" ");
}

void print_info() {
        std::cout << "Solutions: " << count << " ";
        std::cout << "Nodes: " << node_count << " ";
        std::cout << "Timeout: " << trig << " ";
        std::cout << "Time: " << gettime() << "\n";
}


void tryFunc(int1024_t ld, int1024_t cols, int1024_t rd, int curDepth)
{
		node_count++;
		if(trig) {
			print_info();
			std::cout << "Timeout Reached. Exiting.\n";
			exit(0);
		}
		PAD(curDepth);
		PRINTBITS(ld);
		INFO(",");
		PRINTBITS(cols);
		INFO(",");
		PRINTBITS(rd);
		INFO(":%d ", curDepth);
	if(curDepth == depth)
	{
		INFO("\n");
		count++;
		std::cout << "Solution Found\n";
#ifdef PRINT_SOL
		print_sol();
#endif
		print_info();
		
		exit(0);
	}

	int1024_t shiftld = ld << 1;
	int1024_t shiftrd = rd >> 1;
	
	assert((shiftld & one) == 0);
	assert((shiftrd & (one << (depth - 1))) == 0);
	
	if(fillLeft[curDepth+1])
	{
		INFO("*");
		shiftld |= one;
	}
	else
	INFO(" ");
	
	if(fillRight[curDepth+1])
	{
		INFO("*");
		shiftrd |= one << (depth - 1);
	}
	else INFO(" ");
	
	// If we already put a queen on this depth, skip it.
	if(skipDepth[curDepth])
	{
		INFO(" -- skip\n");
		return tryFunc(shiftld, cols, shiftrd, curDepth + 1);
	}
	
	PRINTBITS(shiftld);
		INFO(",");
		PRINTBITS(shiftrd);
		INFO("\n");
		
	{
		int1024_t poss = ~(ld | cols | rd) & all;
		while(poss != zero) {
			int1024_t bit = poss & -poss;
			poss -= bit;
#ifdef PRINT_SOL
			sol[curDepth] = bit;
#endif
			PAD(curDepth);
			PRINTBITS(bit);
			INFO(" added\n");
			tryFunc((bit<<1)|shiftld, cols|bit, (bit>>1)|shiftrd, curDepth + 1);
		}
	}
}

int main(int argc, char** argv)
{
	// SETUP TIMELIMIT HERE
	setup_timelimit(1800);
	
	if(argc % 2 != 0)
	{
		printf("./shiftqueens n queen1x queen1y queen2x queen2y ...\n");
		exit(1);
	}
	
	depth = atoi(argv[1]);
	all = (one << depth) - 1;
	
	int1024_t cols = 0;
	int1024_t ld = 0;
	int1024_t rd = 0;
	
	printf("%d\n", argc);
	
	for(int i = 2; i < argc; i+=2)
	{
		int queenx = atoi(argv[i]);
		int queeny = atoi(argv[i+1]);
		// printf("%d,%d\n", queenx, queeny);
		
		if(skipDepth[queeny]) {
			printf("inconsistent y\n");
			exit(1);
		}
		
		skipDepth[queeny] = 1;
#ifdef PRINT_SOL
		sol[queeny] = one << queenx;
#endif

		int1024_t colpos = one << queenx;
		if((cols & colpos)!=zero) {
			printf("Inconsistent start\n");
			exit(1);
		}
		cols |= colpos;
		
		{
			int rightintersect = queenx + queeny;
			if(rightintersect < depth)
			{
				int1024_t newrd = one << rightintersect;
				if((rd & newrd)!=zero) {
					printf("inconsistent diagonal\n");
					exit(1);
				}
				rd |= newrd;
			}
			else
			{
					rightintersect -= (depth - 1);
					if(fillRight[rightintersect]) {
						printf("inconsistent diagonal - type 2\n");
						exit(1);
					}
					fillRight[rightintersect] = 1;
			}
		}
	
		{
			int leftintersect = queenx - queeny;
			if(leftintersect >= 0)
			{
				int1024_t newld = one << leftintersect;
				if((ld & newld)!=zero) {
					printf("inconsistent left diagonal\n");
					exit(1);
				}
				ld |= newld;
			}
			else
			{
					leftintersect *= -1;
					if(fillLeft[leftintersect]) {
						printf("inconsistent left diagonal - type 2\n");
						exit(1);
					}
					fillLeft[leftintersect] = 1;
			}
		}
}

	
	count = 0;
	tryFunc(ld, cols, rd, 0);
	std::cout << "No solution" << "\n";
	print_info();
	exit(0);
}

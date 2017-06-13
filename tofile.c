#include <stdio.h>
#include <math.h>
#include <time.h>

#define PI 3.1415926
int main() {
	time_t t1 = time(NULL);
	time_t t2 = t1;
	double cycles = 0;
	double period = 0.00382995;  // = 1/261.1, the frequency of middle C
	double density = 1;  // 1mil outputs per cycle
	while(1) {
		t2 = time(NULL);
		if(difftime(t2, t1) >= (period * cycles * density)) {
			fputc((int) 100*sin(difftime(t2, t1)), stdout);
			cycles += density;
		}
	}
	return 0;
}



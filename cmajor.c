#include <stdio.h>
#include <math.h>
#include <time.h>

#define MAX 32768
#define SF 44100
#define PI 3.1415926

int main() {
	double note = 261.1;
		for(double t = 0; t < SF; t++) {
			fputc((int) MAX*sin(2 * PI * t / note), stdout);
		}
}

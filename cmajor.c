#include <stdio.h>
#include <math.h>
#include <time.h>

#define MAX 32768
#define SF 44100
#define PI 3.1415926

double freq(int n) {
	double power = (n - 49) / 12;
	double base = pow(2, power);
	return base * 440;
}

int main() {
	double note = freq(71);
	while(1) {
		for(double t = 0; t < SF; t++) {
			double v = 2 * PI * t / note;
			fputc((int) MAX*sin(v), stdout);
		}
	}
	return 0;
}



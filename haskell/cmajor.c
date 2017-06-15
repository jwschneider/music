#include <stdio.h>
#include <math.h>
#include <time.h>

#define MAX 65536
#define SF 44100
#define PI 3.1415926

double freq(int n) {
	double power = ((double) (n - 49)) / 12;
	double base = pow(2, power);
	return base * 440;
}

int main() {
	FILE *out = fopen("c4.out", "w");
	double note = freq(40);

		for(double t = 0; t < SF*2; t++) {
			double v = 2 * PI * t / note;
			int vp = (int) MAX*sin(v);
			fprintf(stdout, "%d\n", vp);
		}
	fclose(out);
	return 0;
}



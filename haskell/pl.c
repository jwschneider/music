#include <stdio.h>
#include <stdlib.h>

int main() {
	char *buffer = malloc(sizeof(char) * 10);
	while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
		fputc(atoi(buffer), stdout);
	}
	free(buffer);
}
		

#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a");  // WARN: file may be never closed

	if (b)
		fclose(fp);
	else
		fprintf(fp, "Testing...\n");
	
	fclose(fp); // WARN: might be closeing already closed file handle fp
}

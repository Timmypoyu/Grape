// string.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *get_char(char *a, int b) {
	
	if (b >= strlen(a)) {
		exit(1);
	}
	static char str[2]= "\0";
	str[0] = a[b];
	return str;
}


/*
int main() {
	char *test = "hello world";
	printf("%s", get_char(test, 0));
	printf("%s", get_char(test, 1));
	printf("%s", get_char(test, 2));
	printf("%s", get_char(test, 3));
	printf("%s", get_char(test, 4));
	printf("%s", get_char(test, 5));
	printf("%s", get_char(test, 6));
	return 0;
}
*/


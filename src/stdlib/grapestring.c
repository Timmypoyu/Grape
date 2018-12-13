// string.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grapestring.h"

char *getChar (int b, char *a) {
	
	if (b >= strlen(a)) {
		exit(1);
	}
	static char str[2]= "\0";
	str[0] = a[b];
	return str;
}

int str_size(char *a) {
	
	return strlen(a);
}



/* int main() { */
/* 	char *test = "hello world"; */
/* 	/1* printf("%s", get_char(test, 0)); *1/ */
/* 	/1* printf("%s", get_char(test, 1)); *1/ */
/* 	/1* printf("%s", get_char(test, 2)); *1/ */
/* 	/1* printf("%s", get_char(test, 3)); *1/ */
/* 	/1* printf("%s", get_char(test, 4)); *1/ */
/* 	/1* printf("%s", get_char(test, 5)); *1/ */
/* 	/1* printf("%s", get_char(test, 6)); *1/ */
/* 	printf("%d", str_size(test)); */
/* 	return 0; */
/* } */



// string.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "grapestring.h"

char *get_char(char *a, int b) {
	
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

// 0 --> false
// everything else --> true

bool str_equal(char *a, char *b) {
	if (strcmp(a, b) != 0) {
		return false;
	} else {
		return true;
	}
}
int main() {
	/* char *test = "hello world"; */
	/* printf("%s", get_char(test, 0)); */
	/* printf("%s", get_char(test, 1)); */
	/* printf("%s", get_char(test, 2)); */
	/* printf("%s", get_char(test, 3)); */
	/* printf("%s", get_char(test, 4)); */
	/* printf("%s", get_char(test, 5)); */
	/* printf("%s", get_char(test, 6)); */
	/* printf("%d", str_size(test)); */
	/* char *test2 = "h!"; */
	/* if (str_equal(test, test)) { */
	/* 	printf("passed1"); */
	/* } */
	/* if (!str_equal(test, test2)) { */
	/* 	printf("passed2"); */
	/* } */
	/* return 0; */
}



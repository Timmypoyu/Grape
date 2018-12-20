// string.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "grapestring.h"

char *get_char (int b, char *a) {
	
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

bool str_equal(char *a, char *b) {
	if (strcmp(a, b) != 0) {
		return false;
	} else {
		return true;
	}
}


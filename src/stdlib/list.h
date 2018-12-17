#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "types.h"

struct List *init_list();

void *list_get(int n, struct List *list);

bool isEmptyList(struct List *list);

int size(struct List *list);

void reverseList(struct List *list);

void push_list(struct List *list, void *data);

void push_front_list(void *data, struct List *list);

void pop_front_list(struct List *list);

void pop_list(struct List *list);

void removeAllNodes(struct List *list);

bool isEqual(struct ListNode *a, struct ListNode *b);

struct List *insert(void *y, struct List *list, int x);

struct List *list_remove(void *y, struct List *list);


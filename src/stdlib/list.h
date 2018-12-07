#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "types.h"

int size(struct List *list);

struct List *init_list();

void push_list(struct List *list, void *data);

void push_front_list(struct List *list, void *data);

void pop_front_list(struct List *list);

void pop_list(struct List *list);

int isEmptyList(struct List *list);

void reverseList(struct List *list);

struct List *copy(struct List *list);

void removeAllNodes(struct List *list);

bool isEqual(struct ListNode *a, struct ListNode *b);

struct List *list_remove(struct List *list, void *y);

struct List *insert(int x, struct List *list, void *y);


struct ListNode *addAfter(struct List *list, struct ListNode *prevNode, void *data);


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "types.h"

int size(struct List *list);
void initList(struct List *list);
void *popFront(struct List *list);
int isEmptyList(struct List *list);
void reverseList(struct List *list);
struct List *copy(struct List *list);
void removeAllNodes(struct List *list);
bool isEqual(struct ListNode *a, struct ListNode *b);
struct List *list_remove(struct List *list, void *y);
struct List *insert(int x, struct List *list, void *y);
struct ListNode *addBack(struct List *list, void *data);
struct ListNode *addFront(struct List *list, void *data);
struct ListNode *addAfter(struct List *list, struct ListNode *prevNode, void *data);

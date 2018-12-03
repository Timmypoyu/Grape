#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

# include "types.h"

struct Node global;

void initList(struct List *list) {
    list->head = 0;
}

int isEmptyList(struct List *list) {
    return (list->head == 0);
}


struct ListNode *addFront(struct List *list, void *data) {
    struct ListNode *node = (struct ListNode *) malloc(sizeof(struct ListNode));
    if (node == NULL) {
		return NULL;
	}

    node->data = data;
    node->next = list->head;
    list->head = node;
    
	return node;
}
/* 
 * I don't think we need to have a traverse function
void traverseList(struct List *list, void (*f)(void *)) {
    struct Node *node = list->head;
    while (node) {
		f(node->data);
		node = node->next;
    }
}
*/

void reverseList(struct List *list) {
    struct ListNode *prv = NULL;
    struct ListNode *cur = list->head;
    struct ListNode *nxt;

    while (cur) {
		nxt = cur->next;
		cur->next = prv;
		prv = cur;
		cur = nxt;
    }

    list->head = prv;
}


void *popFront(struct List *list) {
    if (isEmptyList(list)) {
		return NULL;
	} 
    
	struct ListNode *oldHead = list->head;
    list->head = oldHead->next;
    void *data = oldHead->data;
    free(oldHead);
    
	return data;
}

void removeAllNodes(struct List *list) {
    while (!isEmptyList(list)) {
		popFront(list);
	}
}

struct ListNode *addAfter(struct List *list, struct ListNode *prevNode, void *data) {
    if (prevNode == NULL) { 
		return addFront(list, data);
	}

    struct ListNode *node = (struct ListNode *)malloc(sizeof(struct ListNode));
    
	if (node == NULL) {
		return NULL;
	}

    node->data = data;
    node->next = prevNode->next;
    prevNode->next = node;
    
	return node;
}

struct ListNode *addBack(struct List *list, void *data) {
    struct ListNode *node = (struct ListNode *)malloc(sizeof(struct ListNode));
    if (node == NULL) {
		return NULL;
	}

    node->data = data;
    node->next = NULL;

    if (list->head == NULL) {
		list->head = node;
		return node;
    }
    struct ListNode *end = list->head;   
    while (end->next != NULL) {
		end = end->next;
	}
    
	end->next = node;
    
	return node;
}

struct List *copy(struct List *list) {  
	struct List *new = (struct List *)malloc(sizeof(struct List));
    struct ListNode *node = list->head;
	struct ListNode *newNode = NULL;
	initList(new);
	
    while (node) {
		newNode = addAfter(new, newNode, node->data);
		node = node->next;
	}
	reverseList(new);
	return new;
	
}

int size(struct List *list) {
	
	if (isEmptyList(list)) {
		return 0;
	}	
	int i = 1;
	struct ListNode *node = list->head;
	
	while(node->next) {
		i += 1;
		node = node->next; 
	} 
	return i;	
}

struct List *insert(int x, struct List *list, void *y) {	
	if(isEmptyList(list)) {
		return list;
	}

	struct ListNode *node = (struct ListNode *)malloc(sizeof(struct ListNode));
	
	if (node == NULL) {
		return NULL;
	}

	struct ListNode *iter_node = list->head;
	struct ListNode *insert_node = (struct ListNode *) y;   
	
	///size 
	int i = size(list);

	///if index is wrong, return original list 
	if(size(list) - 1 < x) {
		return list;
	}

	if (x > 0) {
		x -= 1;
		iter_node = iter_node->next;			
	}
	
	insert_node->data = iter_node->next->data; 
	insert_node->next = iter_node->next;
	iter_node->next = insert_node; 

	return list; 			 
}

bool isEqual(struct ListNode *a, struct ListNode *b) {
	
	if (a == NULL || b == NULL) {
		return NULL;
	}
	
	int *dataA = (int *) a->data;
	int *dataB = (int *) b->data;
	
	if (*dataA == *dataB) {
		return 1;
	}
	
	return 0; 
}

struct List *list_remove(struct List *list, void *y) {
	
	if (isEmptyList(list)) {
		return NULL;
	}

	struct ListNode *node = list->head;
	
	while (!isEqual(node, y)) {
		node = node->next;
	}
	
	if (node != NULL) {
		node->next = node->next->next;
	}

	return list;
}

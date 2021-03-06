#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "list.h"

struct Node global;

struct List *init_list() {
	struct List *list = (struct List *)malloc(sizeof(struct List));
	list->head = NULL;
	return list;
}


bool isEmptyList(struct List *list) {
    return (list->head == NULL);
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

void *list_get(int n, struct List *list){
    if( n >= size(list) ) {
        fprintf(stderr, "List Index out of bounds: given %d for list of size %d\n",n,size(list));
	exit(-1);
    }

    struct ListNode *node = list->head;
    if( n == 0 ){
       return node->data;
    }
    for(int i=0; i<n ; i++ ) {
        node = node->next;
    }
    return node->data;
}

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

void push_list(void *data, struct List *list) {
    struct ListNode *node = (struct ListNode *)malloc(sizeof(struct ListNode));
    
    node->data = data;
    node->next = NULL;

    if (list->head == NULL) {
        list->head = node;
    } else {
        struct ListNode *end = list->head;   
        while (end->next != NULL) {
	    end = end->next;
	}
	end->next = node;
    }
}

void push_front_list(void *data, struct List *list) {
    struct ListNode *node = (struct ListNode *)malloc(sizeof(struct ListNode));
 
    node->data = data;
    node->next = list->head;
    list->head = node;
    
}

void pop_front_list(struct List *list) {
        
	struct ListNode *oldHead = list->head;
    list->head = oldHead->next;
    void *data = oldHead->data;
    free(oldHead);
    
}

void pop_list(struct List *list) {
	reverseList(list);
	pop_front_list(list);
	reverseList(list);
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



void removeAllNodes(struct List *list) {
    while (!isEmptyList(list)) {
		pop_front_list(list);
	}
}

bool isEqual(struct ListNode *a, struct ListNode *b) {
	
	if (a == NULL || b == NULL) {
		return NULL;
	}
	
	int *dataA = (int *) a->data;
	int *dataB = (int *) b->data;
	
	if (*dataA == *dataB) {
		return true;
	}
	
	return 0; 
}

struct List *insert(void *y, struct List *list, int x) {	
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


struct List *list_remove(void *y, struct List *list) {
	
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

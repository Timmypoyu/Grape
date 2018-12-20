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

int isEmptyList(struct List *list) {
    return (list->head == NULL);
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

void push_list(struct List *list, void *data) {
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

void push_front_list(struct List *list, void *data) {
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

void removeAllNodes(struct List *list) {
    while (!isEmptyList(list)) {
		pop_front_list(list);
	}
}

struct List *update_at(int x, struct List *list, void *y) {	
	if(isEmptyList(list)) {
		return list;
	}

	struct ListNode *iter_node = list->head;
	
	///if index is wrong, return original list 
	if( x >= size(list) ) {
                fprintf(stderr, "List Index out of bounds: given %d for list of size %d\n",x,size(list));
                exit(-1);
        }

	if(x == 0){
		iter_node->data = y;
		return list;
	}

	for( int i = 0 ; i!=x ; i++ ) iter_node = iter_node->next;
	
	iter_node->data = y;   	

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


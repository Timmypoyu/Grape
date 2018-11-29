// Grape.c
// C Library for Grape

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// Graph and its functions

struct GraphNode {
	void *nodeData;
	struct List *edgeList;
	/* initList(edgeList); */
};
	
struct GraphEdge {
	void *weight; // should it be int*?
	struct GraphNode *from;
	struct GraphNode *to;
};

struct Graph {
	struct List *nodeList;
	/* initList(nodeList); */
};

// functions

struct Graph *GraphAddNode(struct Graph *graph, struct GraphNode *node) {
	
	return graph;
}

int GraphSize(struct Graph graph){
	
	return 0;
}

struct List *GraphLeaves(struct Graph graph) {
	
	return NULL;
}

struct List *GraphAdjacent(struct Graph graph, struct GraphNode node) {

	return NULL;
}

struct List *GraphFind(struct Graph graph, void *value) {

	return NULL;
}

bool GraphIsEmpty(struct Graph graph) {
	
	return 0;
}

void GraphSwitch(struct Graph *graph, struct GraphNode *node1, struct GraphNode *node2) {

}





struct Node {
    void *data;
    struct Node *next;
};

struct List {
    struct Node *head;
};

void initList(struct List *list) {
    list->head = 0;
}

int isEmptyList(struct List *list) {
    return (list->head == 0);
}


struct Node *addFront(struct List *list, void *data) {
    struct Node *node = (struct Node *) malloc(sizeof(struct Node));
    if (node == NULL) {
		return NULL;
	}

    node->data = data;
    node->next = list->head;
    list->head = node;
    
	return node;
}

void traverseList(struct List *list, void (*f)(void *)) {
    struct Node *node = list->head;
    while (node) {
		f(node->data);
		node = node->next;
    }
}

void reverseList(struct List *list) {
    struct Node *prv = NULL;
    struct Node *cur = list->head;
    struct Node *nxt;

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
    
	struct Node *oldHead = list->head;
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

struct Node *addAfter(struct List *list, struct Node *prevNode, void *data) {
    if (prevNode == NULL) { 
		return addFront(list, data);
	}

    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    
	if (node == NULL) {
		return NULL;
	}

    node->data = data;
    node->next = prevNode->next;
    prevNode->next = node;
    
	return node;
}

struct Node *addBack(struct List *list, void *data) {
    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL) {
		return NULL;
	}

    node->data = data;
    node->next = NULL;

    if (list->head == NULL) {
		list->head = node;
		return node;
    }
    struct Node *end = list->head;   
    while (end->next != NULL) {
		end = end->next;
	}
    
	end->next = node;
    
	return node;
}


struct List copy(struct List *list) {  
	struct List *new = (struct List *)malloc(sizeof(struct List));
    struct Node *node = list->head;
	struct Node *newNode = NULL;
	initList(new);
	
    while (node) {
		newNode = addAfter(new, newNode, node->data);
		node = node->next;
	}
	return *new;
	
}

int size(struct List *list) {
	
	if (isEmptyList(list)) {
		return 0;
	}	
	int i = 1;
	struct Node *node = list->head;
	
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

	struct Node *node = (struct Node *)malloc(sizeof(struct Node));
	
	if (node == NULL) {
		return NULL;
	}

	struct Node *iter_node = list->head;
	struct Node *insert_node = (struct Node *) y;   
	
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

bool isEqual(struct Node *a, struct Node *b) {
	
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

	struct Node *node = list->head;
	
	while (!isEqual(node, y)) {
		node = node->next;
	}
	
	if (node != NULL) {
		node->next = node->next->next;
	}

	return list;
}
	 


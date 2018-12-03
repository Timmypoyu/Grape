// Grape.c
// C Library for Grape

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/*
=====================================================
                   DATA TYPES
=====================================================
*/


struct ListNode {
	void *data;
	struct ListNode *next;
};

struct List {
	struct ListNode *head;
};

struct Node { // graph node
	void *data;
	struct List *edges;
};

struct Edge {
	void *data;
	struct Node *to;
	struct Node *from;
};
	
struct Graph {
	struct List *nodes;
	// struct List *edges;
	// struct List *paths;
};


/*
=====================================================
                   LIST FUNCTIONS
=====================================================
*/

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

/*
=====================================================
                   GRAPH FUNCTIONS
=====================================================
*/

struct Graph *GraphInit(struct Graph *glist) {
	
	struct Graph *graph = (struct Graph *)malloc(sizeof(struct Graph));
	initList(graph->nodes);
	return graph;
}

struct Node *GraphCreateNode(void *inputData, void *weight, struct Node *inputTo, struct Node *inputFrom) {
	
	struct Node *node = (struct Node *)malloc(sizeof(struct Node));
	struct List *elist = (struct List *)malloc(sizeof(struct List));
	struct Edge *edge = (struct Edge *)malloc(sizeof(struct Edge));
	node->data = inputData;
	initList(elist);
	edge->data = weight;
	edge->to = inputTo;
	edge->from = inputFrom;
	addFront(elist, edge);

	return node;
}

// when the node already exists
void GraphAddEdge(struct Graph *graph, void *weight, struct Node *inputTo, struct Node *inputFrom, void *value) {
	
	struct ListNode *node = (graph->nodes)->head;
    while (node) {
		if (node->data == value) { // find the same node and add that edge
			struct Edge *edge = (struct Edge *)malloc(sizeof(struct Edge));
			struct Node *graphNode = node->data;
			edge->data = weight;
			edge->to = inputTo;
			edge->from = inputFrom;
			addFront(graphNode->edges, edge);
			break;
		} else {
			continue;
		}
		node = node->next;
	}
}
	
struct Graph *GraphAddNode(struct Graph *graph, struct Node *node) {
	
	addFront(graph->nodes, node);
	return graph;
}

int GraphSize(struct Graph *graph) {
	
	int graphSize = size(graph->nodes);	
	return graphSize;

}

/* decided to get rid of graph root
struct GraphNode *GraphRoot(struct Graph *graph) {
	
	return NULL;
}
*/

struct List *GraphLeaves(struct Graph *graph) {
	
	struct List *leavesList = (struct List *)malloc(sizeof(struct List));
	struct ListNode *target = (graph->nodes)->head;
	while (target) {
		struct Node *targetGraph = (struct Node *)(target->data);
		struct List *edges = targetGraph->edges;
		struct ListNode *tedge = edges->head;
		while (tedge) {
			struct Node *anode = ((struct Edge *)(tedge->data))->to;
			if (anode == NULL) {
				addFront(leavesList, anode);
				tedge = tedge->next;	
			}
		target = target->next;

		}
	}	
	return leavesList;
}

struct List *GraphAdjacent(struct Graph *graph, struct Node *node) {
	
	struct List *adjacentList = (struct List *)malloc(sizeof(struct List));
	struct ListNode *target = (graph->nodes)->head;
	while (target) {
		if (node->data == ((struct Node *)(target->data))->data) {
			struct Node *targetGraph = (struct Node *)(target->data);
			struct List *edges = targetGraph->edges;
			struct ListNode *tedge = edges->head;
			while (tedge) {
				struct Node *anode = ((struct Edge *)(tedge->data))->to;
				addFront(adjacentList, anode);
				tedge = tedge->next;	
			}
		}
		target = target->next;

	}
	return adjacentList;
}

bool GraphFind(struct Graph *graph, void *value) {

    struct ListNode *node = (graph->nodes)->head;
	
    while (node) {
		if (((struct Node *)(node->data))->data == value) {
			return true;
		}
		node = node->next;
	}

	return false;
}

bool GraphIsEmpty(struct Graph *graph) {
	
	return ((graph->nodes)->head == 0);
}

void GraphSwitch(struct Graph *graph, struct Node *node1, struct Node *node2) {

}


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "list.h"

struct Node *init_node(void *input) {
	struct Node *node = (struct Node *)malloc(sizeof(struct Node));
	node->data = input;
	node->edges = init_list();	
	return node;
}

struct Graph *init_graph() {
	struct Graph *graph = (struct Graph *)malloc(sizeof(struct Graph));
	graph->nodes = init_list();
	graph->edges = init_list();
	return graph;
}

struct Edge *init_edge(void *data) {
	struct Edge *edge = (struct Edge *)malloc(sizeof(struct Edge));
	edge->data = data;
	edge->to = NULL;
	edge->from = NULL;
	return edge;
}

void link_edge_from(struct Edge *e, struct Node *from) {
	e->from = from;
	push_list(from->edges, e);
}

void link_edge_to(struct Edge *e, struct Node *to) {
	e->to = to;
	push_list(to->edges, e);
}

void add_node(struct Graph *graph, struct Node *node) {
	push_list(graph->nodes, node);
}

void add_edge(struct Graph *graph, struct Edge *edge) {
	push_list(graph->edges, edge);
}

void *node_get(struct Node *node) {
    return node->data;
}

void *edge_get(struct Edge *edge) {
    return edge->data;
}
	
struct Node *get_to(struct Edge *edge) {
    return edge->to;
}

struct Node *get_from(struct Edge *edge) {
    return edge->from;
}

struct List *get_outgoing(struct Node *node) {
    struct List *adj = node->edges;
    struct List *outgo = init_list();
    struct ListNode *lnode = adj->head;
    struct Node *tnode;
    struct Edge *tedge;
    while( lnode ) {
        tedge = (struct Edge *)lnode->data;
        tnode = (struct Node *)tedge->to;
        if( tnode != node )
            push_list(outgo, tedge);
        lnode = lnode->next;
    }
    return outgo;
}

int GraphSize(struct Graph *graph) {
	return size(graph->node);	
}

bool GraphIsEmpty(struct Graph *graph) {
	
	return ((graph->nodes)->head == 0);
}

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
				push_list(leavesList, anode);
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
				push_list(adjacentList, anode);
				tedge = tedge->next;	
			}
		}
		target = target->next;

	}
	return adjacentList;
}

/* void removeGraph(struct Graph *graph) { */
/* 	if (GraphIsEmpty(graph)) { */
/* 		free(graph); */
/* 	} else { */

/* } */


/*
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

 decided to get rid of graph root
struct GraphNode *GraphRoot(struct Graph *graph) {
	
	return NULL;
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


void GraphSwitch(struct Graph *graph, struct Node *node1, struct Node *node2) {

}
*/

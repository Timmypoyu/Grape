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

void link_edge_from(struct Node *from, struct Edge *e) {
	e->from = from;
	push_list(e, from->edges);
}

void link_edge_to(struct Node *to, struct Edge *e) {
	e->to = to;
	push_list(e, to->edges);
}

void add_node(struct Node *node, struct Graph *graph) {
	push_list(node, graph->nodes);
}

void add_edge(struct Edge *edge, struct Graph *graph) {
	push_list(edge, graph->edges);
}

void *get_val(struct Node *node) {
    return node->data;
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
        tnode = (struct Node *)tedge->from;
        if( tnode == node )
            push_list(tedge, outgo);
        lnode = lnode->next;
    }
    return outgo;
}

//---------
struct List *get_outgoing2(struct Graph *graph, struct Node *node) {
    struct List *adj = graph->edges;
    struct List *outgo = init_list();
    struct ListNode *lnode = adj->head;
    struct Node *tnode;
    struct Edge *tedge;
    while( lnode ) {
        tedge = (struct Edge *)lnode->data;
        tnode = (struct Node *)tedge->from;
        if( tnode == node )
            push_list(outgo, tedge);
        lnode = lnode->next;
    }
    return outgo;
}

int GraphSize(struct Graph *graph) {
	return size(graph->nodes);	
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
			push_list(graphNode->edges, edge);
			break;
		} else {
			continue;
		}
		node = node->next;
	}
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

struct List *graph_to_list(struct Graph *graph) {
	return graph->nodes;
}

struct List *neighbor(struct Node *node){
	struct List *list = (struct List *)malloc(sizeof(struct List));
	struct List *edgelist = node->edges;
	struct ListNode *target = edgelist->head;
	while (target) {
		push_list(list, ((struct Edge *)(target->data))->to);
		target = target->next;
	}
	return list;
}
bool node_same(struct Node *a, struct Node *b){
	if (a == b) {
		return true;
	} else {
		return false;
	}

}


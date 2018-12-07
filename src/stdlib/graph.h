#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// when the node already exists

struct Node *init_node(void *input);

struct Graph *init_graph();

struct Edge *init_edge(void *data);

void link_edge(struct Edge *e, struct Node *from, struct Node *to);

void add_node(struct Graph *graph, struct Node *node);

void add_edge(struct Graph *graph, struct Edge *edge);

int GraphSize(struct Graph *graph);

bool GraphIsEmpty(struct Graph *graph);

struct List *GraphLeaves(struct Graph *graph);

bool GraphFind(struct Graph *graph, void *value);

struct Graph *GraphAddNode(struct Graph *graph, struct Node *node);

struct List *GraphAdjacent(struct Graph *graph, struct Node *node);

void GraphSwitch(struct Graph *graph, struct Node *node1, struct Node *node2);

struct Node *GraphCreateNode(void *inputData, void *weight, struct Node *inputTo, struct Node *inputFrom);

void GraphAddEdge(struct Graph *graph, void *weight, struct Node *inputTo, struct Node *inputFrom, void *value);


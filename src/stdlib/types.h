struct ListNode {
	void *data;
	struct ListNode *next;
};

struct List {
	struct ListNode *head;
};

struct Node { 
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
	struct List *edges;
};

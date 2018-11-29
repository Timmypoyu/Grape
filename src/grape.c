#include <stdio.h>
#include <stdlib.h>
#include "mylist.h"

struct nlist { /* table entry: */
    struct nlist *next; /* next entry in chain */
    char *name; /* defined name */
    char *defn; /* replacement text */
};

#define HASHSIZE 101
static struct nlist *hashtab[HASHSIZE]; /* pointer table */

/* hash: form hash value for string s */
unsigned hash(char *s)
{
    unsigned hashval;
    for (hashval = 0; *s != '\0'; s++)
      hashval = *s + 31 * hashval;
    return hashval % HASHSIZE;
}

/* lookup: look for s in hashtab */
struct nlist *lookup(char *s)
{
    struct nlist *np;
    for (np = hashtab[hash(s)]; np != NULL; np = np->next)
        if (strcmp(s, np->name) == 0)
          return np; /* found */
    return NULL; /* not found */
}

char *strdup(char *);
/* install: put (name, defn) in hashtab */
struct nlist *install(char *name, char *defn)
{
    struct nlist *np;
    unsigned hashval;
    if ((np = lookup(name)) == NULL) { /* not found */
        np = (struct nlist *) malloc(sizeof(*np));
        if (np == NULL || (np->name = strdup(name)) == NULL)
          return NULL;
        hashval = hash(name);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    } else /* already there */
        free((void *) np->defn); /*free previous defn */
    if ((np->defn = strdup(defn)) == NULL)
       return NULL;
    return np;
}

char *strdup(char *s) /* make a duplicate of s */
{
    char *p;
    p = (char *) malloc(strlen(s)+1); /* +1 for ’\0’ */
    if (p != NULL)
       strcpy(p, s);
    return p;
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

struct Node *addBack(struct List *list, void *data)
{
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

    while (end->next != NULL) {
		end = end->next;
	}
    
	end->next = node;
    
	return node;
}

/*

functions that still need to be written

struct List copy() { }

int size() { }

void insert(int x, void *y) { }

void remove(void *x) { }

*/

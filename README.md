# The Grape Programming Language

### Development Environment

# Setting up environment
This could take a while, be patient!
```
$ ./run init
$ ./run dev
```

# Compilation (in VM)
```
$ ./run test
$ ./run build
```


### INFRA
- [X] Makefile
- [X] Dockerfile
- [ ] Test cases

### Scanner 
- [X] List brackets
- [ ] Floats 
- [X] Dictionary 
- [X] modulo operator
- [X] exponent
- [X] ampersand "&" : graph union
- [X] for
- [X] in
- [X] Colon (for dictionary)
- [X] fun

### Parser 
- [X] Implements List 
- [ ] template searching (working for nodes not for edges, because edge notation conflicts with `MINUS expr`)
- [X] function declaration
- [X] dictionary declaration
- [ ] Edge declaration (shift/reduce with GT)


### Semant.mll
node, graph, type
- [ ] Don't assign types to empty lists, make new type: (List<>)
- [ ] Add types for graph 
- [ ] Semant check if the last one has a no expr, if the node is not the end, there should be no expr. 

### Codegen
- [ ] Link C library for lists
- [ ] Link C library for Node
- [ ] Link C library for Graph

## C Library

### Node
- [x] A list of edges 
- [x] void pointer (data) 

### #dge
- [x] void(int) pointer (data)
- [x] node pointer (from) 
- [x] ndoe pointer (to)

### Graph
- [x] a list of node 
- [x] function prototypes
- [ ] finish writing functions
- [ ] edge_init
- [ ] node_init
- [ ] graph_init:
1. Create a new graph
2. Add all nodes to graph
3. add all edges pointing to the node, and from the previous node
Then, for each ID:
3. Merge all graphs into one graph, resolving Nodes with that ID by 
removing all existing edges to that ID and pointing them to a new 
Node instance with that ID
5: return new graph

Suggestion:

### LRM Changes: 
- [ ] We can't have empty block stmt
- [ ] Dictionaries now used STR_LIT as keys 

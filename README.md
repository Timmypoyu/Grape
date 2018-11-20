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

### Codegen
- [ ] Link C library for lists
- [ ] Link C library for Node
- [ ] Link C library for Graph

### LRM Changes: 
- [ ] We can't have empty block stmt
- [ ] Dictionaries now used STR_LIT as keys 

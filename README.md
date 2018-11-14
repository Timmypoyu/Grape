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


### LRM Changes: 
1. We can't have empty block stmt
3. Dictionaries now used STR_LIT as keys 

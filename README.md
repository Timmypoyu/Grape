# PLTProject

### Development Environment

How to compile:
```
$ ./scripts/env-boot.sh
$ ./scripts/bootstrap.sh
$ cd src
$ make grape
```


### INFRA
- [ ] Makefile
- [X] Dockerfile
- [ ] Test cases

### AST 
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

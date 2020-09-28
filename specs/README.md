## Mlang specification files

This directory contains Mlang specification files, which are used to tune the 
output of the compiler. The syntax of the `.m_spec` files is the following:

```
saisie: VAR1, VAR2, <...>, VARn;

const: VAR1 = 42; VAR2 = 56; <...>; VARn = 9000;

condition: VAR1 = 4, VAR2 > 0, <...>, VARn + (VARn-1) < 4;

sortie: VAR1, VAR2, <...>, VARn;
```

Each section accepts a list of variable names. The variable name can also 
correspond to an "alias": for instance, `1AJ` is the alias of `TSHAVLLOV`.

### `saisie`

Variables in this section are assumed to be inputs of the generated programs.
All variables not listed in this section are `undefined` unless defined 
otherwise.

### `const`

Assign constant values to some of the variables. This helps the optimizations
to pre-compute part of the program if you know in advance the values of certain 
variables.

### `condition`

Declare assertions that will be checked during the execution of the program.
Useful to report an error if some inputs are ill-formed.

### `sortie`

Declares the output of the program. Only variables declared in this section 
will be computed and output. The fewer outputs you declare, the more 
optimizations it will enable. 



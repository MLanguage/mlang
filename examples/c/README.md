## Using Mlang with C

Mlang is able to compile the official source code of the French income tax
computation to C. This means that you can embark the computation as a
regular C source code file in your application if you need it. This file
is a guide about how to do it.

### Generating the Python file

You have to invoke Python for that, with something like:

```
mlang --display_time --debug \ # Prints debug information
    --mpp_file=path/to/mlang/<year>.mpp \  # <year> is the year when the income was received
    --mpp_function=compute_double_liquidation_pvro \ # this function is the one computing the taxes
    --optimize \ # optimize the output, otherwise the generated file is very large
	--backend c --output <name>.py \ # outputs a Python file
	--function_spec <spec_file>.m_spec \ # configuration (see above)
	<M source files from ir-calcul>
```

### Using the generated C file

The main function provided by the generated C file provides one function,
`m_extracted`. This functions takes a struct as its argument, and returns another
struct.

he input struct has its own type, `m_input`, whose fields are the different
input values of the income tax computation. Each value has type `m_value`, which
is a `double` wrapped with a boolean flag to indicate if the value is undefined.
If the flag `undefined` is set to true, then the `double` value should be `0.`.
`m_value` is defined in the files `m_value.c` and `m_value.h`, which have to be
linked against your main file when building.

Because the input struct can have a large number of fields, helper functions are
provided to build a struct from an array of values. The methodology is the
following: first, you create an array of `m_value` whose size is given by
`m_num_inputs`. Then, you use `m_get_input_index` to fill the right index of the
array given the name of the input variable you want to set. Finally, you can
build the struct with `m_input_from_array`.

The output struct also has its own type, `m_output`, the fields of which are
the output values requested in the `.m_spec`, as well as an additional boolean
flag indicating whether the computation yielded an error. If there was an error,
the corresponding error message has been printed on stdout during the computation.
In the case of error, all other fields of the output are set to undefined. Helper
functions turning the output struct into an array, similarly as to the input
array to struct conversion, are also provided.

See the files named `run_*.c` for concrete examples.

**Tip:** if you have a segmentation fault when running the binary built from
the generated C file, it is likely that your authorized stack size is too low.
Indeed, `m_extracted` allocates a huge number of intermediate variables on the
stack that overflows the regular limit. Use something like `ulimit -s 65535`
to increase the max stack size before running the binary and the segmentation
fault will disappear.

### Using the Makefile in this folder

The Makefile in this folder contains rules for generating Python files from
`.m_specs` in the `m_specs/` folder of this repository, using the 2019 code from
`ir_calcul` and the canonical `.mpp` file for 2019 income tax. To use it,
simply invoke:

    make ir_<name_of_the_m_spec_file>.c

`make` will re-generate this file at each modification of the `.m_spec` file,
otherwise use `-B` to force re-generation.

If you have an error about unknown variables, then it probably means the
example you're trying to build depends on the 2018 version of the tax code.
To get it working, invoke the command by setting the year to 2018:

    YEAR=2018 make ir_<name_of_the_m_spec_file>.py

### Testing the correctness of the Mlang backend

The `backend_tests` folder contains a small utility that compares the output
of running the Python against the expected output for a test base.

To launch the tests, simply invoke from this folder:

    make backend_tests

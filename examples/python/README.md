## Using Mlang with Python

Mlang is able to compile the official source code of the French income tax 
computation to Python. This means that you can embark the computation as a 
regular Python source code file in your application if you need it. This file 
is a guide about how to do it.

**Warning:** Mlang produces Python code for `python3.7` and above, older 
versions of Python are not supported.

### Generating the Python file

You have to invoke Mlang for that, with something like:

```
mlang --display_time --debug \ # Prints debug information
    --mpp_file=path/to/mlang/<year>.mpp \  # <year> is the year when the income was received
    --mpp_function=compute_double_liquidation_pvro \ # this function is the one computing the taxes
    --optimize \ # optimize the output, otherwise the generated file is very large
	--backend python --output <name>.py \ # outputs a Python file
	--function_spec <spec_file>.m_spec \ # configuration (see above)
	<M source files from ir-calcul>
```

### Using the generated Python file 

The generated Python file provides one function, `extracted`, taking as an 
argument a dictionary whose keys are the input variables declared in the `.m_spec`
file. The function returns a dictionary of the output variables, or just a single 
value if there is only one output variable declared. Caution: the returned value 
can also be an instance of the `Undefined` class, defined inside the generated 
Python file.

See the files named `run_*` for concrete examples.

### Using the Makefile in this folder

The Makefile in this folder contains rules for generating Python files from 
`.m_specs` in the `m_specs/` folder of this repository, using the 2018 code from 
`ir_calcul` and the canonical `.mpp` file for 2018 income tax. To use it, 
simply invoke:

    make ir_<name_of_the_m_spec_file>.py

`make` will re-generate this file at each modification of the `.m_spec` file, 
otherwise use `-B` to force re-generation.
 
### Testing the correctness of the Mlang backend

The `backend_tests` folder contains a small utility that compares the output
of running the Python against the expected output for a test base.

To launch the tests, simply invoke from this folder:

    make backend_tests

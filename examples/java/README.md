## Using Mlang with Java 

Mlang is able to compile the official source code of the French income tax
computation to Java. You can use these files in your Java application to calculate tax.
This file is a guide on this file usage.

**Warning:** Mlang produces Java code for `JDK 7` and above, older
versions of Java are not supported.

### Generating the Java file

You have to invoke Mlang for that, with something like:

```
mlang --display_time --debug \ # Prints debug information
    --mpp_file=path/to/mlang/<year>.mpp \  # <year> is the year when the income was received
    --mpp_function=compute_double_liquidation_pvro \ # this function is the one computing the taxes
    --optimize \ # optimize the output, otherwise the generated file is very large
	--backend java --output <name>.java \ # outputs a Java file
	--function_spec <spec_file>.m_spec \ # configuration (see above)
	<M source files from ir-calcul>
```

### Using the generated Java file 

The generated Java file provides one method, `calculateTax`, taking as an
argument a `Map<String, MValue>` whose keys are the input variables declared in the `.m_spec`
file. The method is overloaded with a version that also takes an `int` representing the max number
of anomalies before the program exits. By default, the program will exit on the first anomaly.

The function returns a `Map<String, MValue>` of the output variables. 
Caution: The elements of this `Map` may be undefined, in which their propery `undefined` is set to true.

### Helper Java classes

- `MValue`: Represents a variable used during calculation, either as input, output or an intermediate value. 
Contains a `value` and a boolean `undefined`.

- `MCalculation`: Internal class that carries the state of calculation during it's lifetime.

- `MError`: Represents an anomaly of any level that may occur during tax calculation. 

- `MException`: Exception class, thrown if the max number of top level anomalies has been reached.
This class includes a list of all the anomalies that occurred up until that point.

- `MOutput`: Final return value of a calculation if `MException` was not thrown, contains an `outputValues`
field of `Map<String, MValue>` with the key being the name of the variable. It also contains
a `calculationVariables` field of `List<MError>` containing the anomalies that occurred during calculation.

- `MValue`: Base calculation variable type that has two properties : `value` and `undefined`

Please see Javadoc in classes for more information

### Using the Makefile in this folder

The Makefile in this folder contains rules for generating Java files from
`.m_specs` in the `m_specs/` folder of this repository, using the 2020 code from
`ir_calcul` and the canonical `.mpp` file for 2020 income tax. To use it,
simply invoke:

    make src/com/mlang/Ir_<name_of_the_m_spec_file>.java

`make` will re-generate this file at each modification of the `.m_spec` file,
otherwise use `-B` to force re-generation.

If you have an error about unknown variables, then it probably means the
example you're trying to build depends on a different year of the tax code.
To get it working, invoke the command by setting the year to another year:

    YEAR=<year> make ir_<name_of_the_m_spec_file>.py

### Testing the correctness of the Mlang backend

The `backend_tests` folder contains a small utility that compares the output
of running the Java against the expected output for a test base.

To launch the tests, simply invoke from this folder:

    make run_tests 

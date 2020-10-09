## Using Mlang with Python

Mlang is able to compile the official source code of the French income tax 
computation to Python. This means that you can embark the computation as a 
regular Python source code file in your application if you need it. This file 
is a guide about how to do it.

### Configuring the generated file

The first thing to do is to figure out which parts of the income tax computation 
you really need. This implies determining what would be your inputs and output
variables. Indeed, the income tax computation is structured around a set of 
variables, which can either be :
* inputs of the earning statements (like the salary of a person);
* computed quantities (like the amount of taxes you owe).

Your application might not need to compute the income tax in a completely 
general case; often you want to compute it in a simplified setting where not 
all inputs can be filled by the user. The descriptions of the variables can be 
found in the [tvgI.m](../../ir-calcul/sources2018m_6_7/tgvI.m). You can also 
figure out the input variables by looking at the 3-letters-and-numbers names 
of the inputs in the 
[official form](https://www3.impots.gouv.fr/simulateur/calcul_impot/2019/simplifie/index.htm),
and here is a list of common output variables:

* `IINET`: "Total de votre imposition"
* `IRNET`: "Total de votre imposition (si positif)"
* `NAPCR`: "Net a payer (CSG + CRDS)"
* `TXMOYIMP`: "Taux moyen d imposition"
* `REVKIRE`: "Revenu de reference"
* `NBPT`: "Nombre de parts"
* `IAN`: "Impot apres imputations non restituables"
* `CIMR`: "Credit impot modernisation du recouvrement"
* `CSG`: "CSG"
* `RDSN`: "CRDS"
* `PSOL`: "Contribution sociale et solidarite"

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

The generated Python file generates one function, `extracted`, taking as an 
argument a dictionary whose keys are the input variables declared in the `.m_spec`
file. The function returns a dictionary of the output variables, or just a single 
value if there is only one output variable declared. Caution: the returned value 
can also be an instance of the `Undefined` class, defined inside the generated 
Python file.

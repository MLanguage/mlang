#include "ir_simulateur_simplifie_inputs_no_outs_2018.h"
#include <string.h>
#include <unistd.h>

int main()
{

    int num_inputs = m_num_inputs();
    int correct_string_size = 2 * num_inputs;

    // First we fill the array with the contents of the fuzzing input
    m_value input_array_for_m[num_inputs];
    char input_string[correct_string_size];
    klee_make_symbolic(&input_string, sizeof(input_string), "input_string");

    for (int i = 0; i < num_inputs; i++)
    {
        // A char holds values from 0 to 65535 so it's a good range for
        // our input values
        char *value = input_string + 2 * i;
        char *undefined = input_string + 2 * i + 1;
        m_value input = (struct m_value){
            .undefined = (((unsigned int)*undefined) > 32767) ? true : false,
            .value = (double)((unsigned int)(*value)),
        };
        input_array_for_m[i] = input;
    }

    // Then we call the program
    m_input input_for_m = m_input_from_array(input_array_for_m);
    m_output output = m_extracted(input_for_m);
    if (output.is_error)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}
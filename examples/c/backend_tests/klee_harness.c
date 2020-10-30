#include "ir_simulateur_simplifie_inputs_no_outs_2018.h"
#include <string.h>
#include <unistd.h>

int main()
{

    int num_inputs = m_num_inputs();

    // First we fill the array with the contents of the fuzzing input
    m_value input_array_for_m[num_inputs];
    bool undefined_inputs[num_inputs];
    double value_inputs[num_inputs];
    klee_make_symbolic(&undefined_inputs, sizeof(undefined_inputs), "undefined_inputs");
    klee_make_symbolic(&value_inputs, sizeof(value_inputs), "value_inputs");

    for (int i = 0; i < num_inputs; i++)
    {
        m_value input = (struct m_value){
            .undefined = undefined_inputs[i],
            .value = value_inputs[i],
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
#include "ir_all_ins_no_outs_2018.h"
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{

    if (argc != 2)
    {
        printf("The program expects no argument!");
    }

    int num_inputs = m_num_inputs();

    // We read the fuzzer input from stdin
    char buffer[9 * num_inputs];
    read(STDIN_FILENO, buffer, num_inputs);

    // First we fill the array with the contents of the fuzzing input
    m_value input_array_for_m[num_inputs];

    for (int i = 0; i < num_inputs; i++)
    {
        double *value = (double *)(buffer + 9 * i);
        char *undefined = buffer + 9 * i + 8;
        m_value input = (struct m_value){
            .undefined = *undefined,
            .value = *value,
        };
        input_array_for_m[i] = input;
    }

    // Then we call the program
    m_input input_for_m = m_input_from_array(input_array_for_m);
    m_output output = m_extracted(input_for_m);
    if (output.is_error)
    {
        return -1;
    }
    else
    {
        return 0;
    }
}
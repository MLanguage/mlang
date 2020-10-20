#include "ir_all_ins_no_outs_2018.h"
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{

    if (argc != 2)
    {
        printf("%d\n", argc);
        return 1;
    }

    int num_inputs = m_num_inputs();
    long correct_string_size = 2 * num_inputs + 1;

    FILE *input_file = fopen(argv[1], "r");
    fseek(input_file, 0, SEEK_END);
    long fsize = ftell(input_file);
    if (fsize != correct_string_size)
    {
        printf("%d != %d\n", (int)correct_string_size, (int)fsize);
        return 2;
    }
    rewind(input_file);
    char input_string[correct_string_size];
    fread(input_string, 1, fsize, input_file);
    fclose(input_file);

    // First we fill the array with the contents of the fuzzing input
    m_value input_array_for_m[num_inputs];

    for (int i = 0; i < num_inputs; i++)
    {
        // A char holds values from 0 to 65535 so it's a good range for
        // our input values
        char *value = input_string + 9 * i;
        char *undefined = input_string + 9 * i + 1;
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
        return 3;
    }
    else
    {
        abort();
    }
}
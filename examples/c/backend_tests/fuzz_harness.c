#include "ir_tests.h"
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
    int size_per_value = 1 + sizeof(unsigned int);
    long correct_string_size = size_per_value * num_inputs + 1;

    FILE *input_file = fopen(argv[1], "r");
    fseek(input_file, 0, SEEK_END);
    long fsize = ftell(input_file);
    if (fsize < correct_string_size)
    {
        printf("Input file size: %ld (correct string size %ld)\n", fsize, correct_string_size);
        return 2;
    }
    rewind(input_file);
    char input_string[correct_string_size];
    fread(input_string, 1, correct_string_size, input_file);
    fclose(input_file);

    // First we fill the array with the contents of the fuzzing input
    m_value input_array_for_m[num_inputs];

    for (int i = 0; i < num_inputs; i++)
    {
        // A char holds values from 0 to 65535 so it's a good range for
        // our input values
        char *undefined = input_string + size_per_value * i;
        char *value = input_string + size_per_value * i + 1;
        m_value input = (struct m_value){
            .undefined = ((unsigned int)*undefined > 32767) ? true : false,
            .value = (double)(*((unsigned int *)value)),
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

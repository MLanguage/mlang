#include "ir_tests.h"
#include <string.h>
#include <unistd.h>
#include <stdint.h>

int main(int argc, char *argv[])
{

    if (argc != 2)
    {
        printf("Expected 1 argument, got %d\n", argc - 1);
        return -1;
    }

    int num_inputs = m_num_inputs();
    int size_per_value = (sizeof(char)) + sizeof(uint16_t);
    long correct_string_size = size_per_value * num_inputs + 1;

    FILE *input_file = fopen(argv[1], "r");
    fseek(input_file, 0, SEEK_END);
    long fsize = ftell(input_file);
    if (fsize < correct_string_size)
    {
        printf("Input file size %ld bytes but expected at least %ld\n", fsize, correct_string_size);
        return -2;
    }
    rewind(input_file);
    char input_string[correct_string_size];
    fread(input_string, 1, correct_string_size, input_file);
    fclose(input_file);

    // First we fill the array with the contents of the fuzzing input
    m_value *input_array_for_m = malloc(num_inputs * sizeof(m_value));

    for (int i = 0; i < num_inputs; i++)
    {
        char *undefined = input_string + size_per_value * i;
        char *value = input_string + size_per_value * i + (sizeof(char));
        bool undefined_v = ((uint16_t)*undefined > 32767) ? true : false;
        // Values are seeded with a uint16_t,
        // corresponding to a value no bigger than 65536
        uint16_t value_v = (*((uint16_t *)value));
        m_value input = (struct m_value){
            .undefined = undefined_v,
            .value = undefined_v ? 0 : (double)value_v,
        };
        input_array_for_m[i] = input;
    }
    // Then we call the program
    m_input *input_for_m = malloc(sizeof(m_input));
    m_input_from_array(input_for_m, input_array_for_m);
    m_output *output = malloc(sizeof(m_output));
    m_extracted(output, input_for_m);
    int num_outputs = m_num_outputs();
    m_value *output_array_for_m = malloc(num_outputs * sizeof(m_value));
    m_output_to_array(output_array_for_m, output);
    // We don't want error cases or household whose revenue is astronomically high
    bool keep_test_case =
        !output->is_error && // the test case should not be an error
        // output_array_for_m[m_get_output_index("REVKIRE")].value < 10000000.) && // the reference income should be low
        true;

    if (!keep_test_case)
    {
        return -3;
    }
    else
    {
        // We print the test case found in the FIP format on stdin
        printf("#NOM\n");
        int checksum = 0;
        for (int i = 0; i < correct_string_size / (sizeof(int)); i++)
        {
            checksum ^= ((int *)input_string)[i];
        }
        printf("RANDOMFUZZERTEST%d\n", checksum);
        printf("#ENTREES-PRIMITIF\n");
        for (int i = 0; i < num_inputs; i++)
        {
            if (!input_array_for_m[i].undefined)
            {
                printf("%s/%f\n", m_get_input_name_from_index(i), input_array_for_m[i].value);
            }
        }
        printf("#CONTROLES-PRIMITIF\n");
        printf("#RESULTATS-PRIMITIF\n");
        // Here should go the output variable
        for (int i = 0; i < num_outputs; i++)
        {
            if (!output_array_for_m[i].undefined)
            {
                printf("%s/%f\n", m_get_output_name_from_index(i), output_array_for_m[i].value);
            }
        }
        printf("#ENTREES-CORRECTIF\n");
        printf("#CONTROLES-CORRECTIF\n");
        printf("#RESULTATS-CORRECTIF\n");
        printf("##\n");

        // Aborting to signal the fuzzer that this is a good one
        abort();
    }
    free(input_array_for_m);
    free(input_for_m);
    free(output);
    free(output_array_for_m);
}

#include "ir_simulateur_simplifie_2018.h"
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
        char *undefined = input_string + size_per_value * i;
        char *value = input_string + size_per_value * i + 1;
        bool undefined_v = ((unsigned int)*undefined > 32767) ? true : false;
        // Values cannot have more than 10 digits
        unsigned int value_v = (*((unsigned int *)value)) % 1000000;
        m_value input = (struct m_value){
            .undefined = undefined_v,
            .value = (double)value_v,
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
        int num_outputs = m_num_outputs();

        m_value output_array_for_m[num_outputs];
        m_output_to_array(output_array_for_m, output);
        // We print the test case found in the FIP format on stdin
        printf("#NOM\n");
        printf("#RANDOMFUZZERTEST#FIP/%llx\n", (unsigned long long)(*input_string));
        printf("#ENTREES-PRIMITIF\n");
        for (int i = 0; i < num_inputs; i++)
        {
            printf("%s/%f\n", m_get_input_name_from_index(i), input_array_for_m[i].value);
        }
        printf("#CONTROLES-PRIMITIF\n");
        printf("#RESULTATS-PRIMITIF\n");
        // Here should go the output variable
        for (int i = 0; i < num_outputs; i++)
        {
            printf("%s/%f\n", m_get_output_name_from_index(i), output_array_for_m[i].value);
        }
        printf("#ENTREES-CORRECTIF\n");
        printf("#CONTROLES-CORRECTIF\n");
        printf("#RESULTATS-CORRECTIF\n");
        printf("##\n");

        // Aborting to signal the fuzzer that this is a good one
        abort();
    }
}

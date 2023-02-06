
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>

#include "calc/ir_tests.h"

#include "calc/irdata.h"
#include "calc/desc.h"

#define MAX_PATH_LENGTH 4096 /* Arbitrary value */

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Expected one command-line argument, the tests directory\n");
        return -1;
    }

    char line_buffer[1000];
    char file_path[MAX_PATH_LENGTH];
    char *separator = "/";
    char *tests_dir = argv[1];
    int state;

    T_irdata *irdata = IRDATA_new_irdata();

    int i;

    //m_error_occurrence *errors;

    char *name;
    char *value_s;
    int value;
    double expected_value;
    double computed_value;

    T_desc_var *desc;
    double *varptr;

    DIR *d;
    struct dirent *dir;
    d = opendir(tests_dir);
    if (d)
    {
        while ((dir = readdir(d)) != NULL)
        {
            char *test_file = dir->d_name;

            if (strcmp(test_file, ".") == 0 || strcmp(test_file, "..") == 0)
            {
                continue;
            }
            if ((strlen(tests_dir) + strlen(test_file) + 2) > MAX_PATH_LENGTH)
            {
                continue;
            }
            sprintf(file_path, "%s/%s", tests_dir, test_file);

            FILE *fp = fopen(file_path, "r");
            if (fp == NULL)
            {
                continue;
            }

            // Resetting the arrays
            IRDATA_reset_irdata(irdata);

            state = 0;
            // 0 - before #ENTREES-PRIMITIF
            // 1 - between #ENTREES-PRIMITIF and #CONTROLES-PRIMITIF
            // 2 - between #CONTROLES-PRIMITIF and #ENTREES-CORRECTIF
            // 3 - after #ENTREES-CORRECTIF
            while (EOF != fscanf(fp, "%[^\n]\n", line_buffer))
            {
                switch (state)
                {
                case 0:
                    if (strcmp(line_buffer, "#ENTREES-PRIMITIF") == 0)
                    {
                        state = 1;
                        break;
                    }
                    // We continue
                    break;

                case 1:
                    if (strcmp(line_buffer, "#CONTROLES-PRIMITIF") == 0)
                    {
                        state = 2;
                        // Here we move to controlling the outputs, so we
                        // have to run the computation!
                        //errors = malloc(sizeof(error_occurrences));
                        //output_for_m->errors = errors;
                        dgfip_calculation(irdata);
                        //free(output_for_m->errors);
                        break;
                    }
                    // We parse the inputs
                    name = strtok(line_buffer, separator);
                    value_s = strtok(NULL, separator);
                    value = atoi(value_s);
                    desc = IRDATA_cherche_desc_var(name);
                    IRDATA_range(irdata, desc, (double)value);
                    break;

                case 2:
                    if (strcmp(line_buffer, "#ENTREES-CORRECTIF") == 0)
                    {
                        state = 3;
                        break;
                    }
                    if (strcmp(line_buffer, "#RESULTATS-PRIMITIF") == 0)
                    {
                        break;
                    }
                    // We parse the outputs
                    name = strtok(line_buffer, separator);
                    value_s = strtok(NULL, separator);
                    expected_value = atof(value_s);
                    desc = IRDATA_cherche_desc_var(name);
                    varptr = IRDATA_extrait_special(irdata, desc);
                    // Undefined values returned are interpreted as 0
                    computed_value = (varptr == NULL) ? 0.0 : *varptr;
                    if (computed_value != expected_value)
                    {
                        printf("Testing file: %s\n", test_file);
                        printf("Expected value for %s : %.4f, computed %.4f!\n", name, expected_value, computed_value);
                        exit(-1);
                    }
                    break;

                default:
                    break;
                }
            }
            fclose(fp);
        }
        closedir(d);
    }

    IRDATA_delete_irdata(irdata);
    return 0;
}

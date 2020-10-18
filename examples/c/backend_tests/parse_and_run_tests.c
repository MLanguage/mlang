#include "ir_tests.h"
#include <dirent.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Expected one command-line argument, the tests directory\n");
        return -1;
    }

    char line_buffer[1000];
    char *separator = "/";
    char *tests_dir = argv[1];
    m_input input_for_m = m_empty_input();
    int num_inputs = m_num_inputs();
    m_value input_array_for_m[num_inputs];

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
            printf("Testing file:%s\n", test_file);
            char file_path[256];
            snprintf(file_path, sizeof file_path, "%s/%s", tests_dir, test_file);
            FILE *fp = fopen(file_path, "r");
            if (fp == NULL)
            {
                continue;
            }
            int state = 0;
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
                        break;
                    }
                    // We parse the inputs
                    char *name = strtok(line_buffer, separator);
                    char *value_s = strtok(NULL, separator);
                    int value = atoi(value_s);
                    int name_index = m_get_input_index(name);
                    input_array_for_m[name_index] = m_literal(value);
                    printf("Name: %s (%d), value: %d\n", name, name_index, value);
                    break;

                case 2:
                    if (strcmp(line_buffer, "#ENTREES-CORRECTIF") == 0)
                    {
                        state = 3;
                        break;
                    }
                    // We parse the outputs
                    break;

                default:
                    break;
                }
            }
            printf("State %d\n", state);
            printf("Data from the file:\n%s\n", line_buffer);
            fclose(fp);
        }
        closedir(d);
    }
    return 0;
}
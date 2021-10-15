#!/usr/bin/env python3
# usage ./test_file all_ins_filename TESTS_DIR

import sys, os
import tests # the generated file

from common import parse_test

if __name__ == "__main__":
    all_ins_filename = sys.argv[1]
    with open(all_ins_filename, 'r') as f:
        total_entrees = f.read().split(", ")
    tests_dir = sys.argv[2]
    for f in os.listdir(tests_dir):
        print(f"Testing {f}")
        entrees, sorties = parse_test(f"{tests_dir}/{f}")
        entrees_completees = {x:(entrees[x] if x in entrees else tests.Undefined()) for x in total_entrees}
        resultats_test = tests.extracted(entrees_completees)
        for (x, r) in resultats_test.items():
            initial_x = '_'.join(x.split('_')[:-2]).upper() if not x.startswith("var_") else '_'.join(x.split('_')[1:]).upper()
            if initial_x in sorties and not(isinstance(r, tests.Undefined)) and r != sorties[initial_x]:
                print(f"Error in {f}, on variable {x} computed output = {r}, expected {sorties[initial_x]}!")
                sys.exit(-1)


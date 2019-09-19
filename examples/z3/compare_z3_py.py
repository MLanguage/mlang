# Copyright (C) 2019 Inria, contributor: Raphaël Monat <raphael.monat@lip6.fr>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import json, sys
try:
    import processing.ir_2018 as ir_2018
except ModuleNotFoundError:
    print("ERROR: please launch this script from the root of the project")
    sys.exit(1)

def py_to_z3_name(npy):
    if npy == "var_0cf": return "V_0CF"
    elif npy == "var_1bj": return "TSHALLOC"
    elif npy == "var_1aj": return "TSHALLOV"

    if npy[:len("var_")] == "var_":
        npy = "t" + npy[len("var_"):]
    fst = npy.rfind("_")
    npy = npy[:fst]
    snd = npy.rfind("_")
    npy = npy[:snd]
    return npy.upper()

with open("results.json", 'r') as f:
    z3_json = json.load(f)
    z3_res = {k: float(v['value']) if v['type'] != "boolean" else v['value'] == 'true' for (k, v) in z3_json.items()}


ir_2018.main(z3_res["V_0CF"], z3_res["TSHALLOC"], z3_res["TSHALLOV"])
py_res = ir_2018.l

for x in py_res:
    if py_to_z3_name(x) not in z3_res:
        print("Variable {} which in py is {} not found in z3".format(x, py_res[x]))
    else:
        diff = abs(py_res[x] - z3_res[py_to_z3_name(x)])
        if diff > 10**-6:
            print("Difference on {}: py={}, z3={}".format(py_to_z3_name(x), py_res[x], z3_res[py_to_z3_name(x)]))
        elif diff > 0:
            print("FP error on {}: py={}, z3={}".format(py_to_z3_name(x), py_res[x], z3_res[py_to_z3_name(x)]))

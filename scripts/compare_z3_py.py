import json, sys
try:
    import ir_2018
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
        print("Variablen {} which in py is {} not found in z3".format(x, py_res[x]))
    else:
        diff = abs(py_res[x] - z3_res[py_to_z3_name(x)])
        if diff > 10**-6:
            print("Difference on {}: py={}, z3={}".format(py_to_z3_name(x), py_res[x], z3_res[py_to_z3_name(x)]))
        elif diff > 0:
            print("FP error on {}: py={}, z3={}".format(py_to_z3_name(x), py_res[x], z3_res[py_to_z3_name(x)]))

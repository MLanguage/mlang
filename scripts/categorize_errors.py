from collections import defaultdict

with open("test_results.txt", "r") as f:
    content = f.readlines()

pieces = []

i = 0
while i < len(content):
    j = i + 1
    while j < len(content) and content[j][0:len("[ERROR]")] != "[ERROR]" :
        j += 1
    pieces.append("".join(content[i:j]))
    i = j

class VCondError:
    def __init__(self, filename, condition, variable):
        self.filename = filename
        self.condition = condition
        self.variable = variable


errors = []
for i in pieces:
    if "Verification condition failed" in i:
        l_f = i.find("file ")+len("file ")
        r_f = i[l_f:].find(", ")
        filename = i[l_f:l_f+r_f][len("tests/"):]
        l_c = i.find("Violated condition:\nnon (")+len("Violated condition:\nnon (")
        r_c = i[l_c:].find(")\n")
        cond = i[l_c:l_c+r_c]
        variable = cond.split("=")[0][:-1]
        errors.append(VCondError(filename, cond, variable))
    else:
        print("Unhandled:" + i)

d = defaultdict(list)
for e in errors:
    d[e.variable].append(e.filename)

for k, v in sorted(d.items(), key=lambda kv: -len(kv[1])):
    print("{} -> {} error{} ({})".format(k, len(v), "s" if len(v) > 1 else "", ", ".join(v)))

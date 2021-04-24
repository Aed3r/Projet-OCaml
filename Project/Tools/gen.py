import re

f = open("input", "r")
temp="\\texttt{\n\\begin{tabbing}\n"
for line in f.readlines():
    parts = line.split("(*")
    temp += "\#" + parts[0][0:2].strip() + "\\=" + parts[0][2:].strip() + " \\\\\n"
    result = parts[1].strip()[:-3].strip()
    firstLine = True
    while len(result)>=45:
        lastItemIndex = result[0:45].rfind(';')+1
        if firstLine:
            temp += "\\> int list = [\\=" + result[1:lastItemIndex].strip() + " \\\\\n"
            firstLine = False
        else:
            temp += "\\> \\> " + result[0:lastItemIndex].strip() + "\\\\\n"
        result = result[lastItemIndex:]
    if firstLine:
        temp += "\\> int list = " + result.strip() + " \\\\\n"
    else:
        temp += "\\> \\>" + result.strip() + " \\\\\n"
    temp += "\\\\\n"
f.close()

temp += "\end{tabbing}\n}"
temp = temp.replace("_", "\\_")
open("output", "w+").write(temp)
import re

# 2017.bin can be found: ftp://nlmpubs.nlm.nih.gov/online/mesh/2017/asciimesh/d2017.bin
# code (lines 8-28) from: https://code.tutsplus.com/tutorials/working-with-mesh-files-in-python-linking-terms-and-numbers--cms-28587
# returns dictionary
def tree():

    terms = {}
    numbers = {}

    meshFile = 'd2022.bin'
    with open(meshFile, mode='rb') as file:
        mesh = file.readlines()

    outputFile = open('mesh.txt', 'w')

    for line in mesh[::-1]:
        meshTerm = re.search(b'UI = (.+)$', line) # 
        if meshTerm:
            term = meshTerm.group(1)
        meshNumber = re.search(b'MN = (.+)$', line)
        if meshNumber:
            number = meshNumber.group(1)
            numbers[number.decode('utf-8')] = term.decode('utf-8')
            if term in terms:
                terms[term] = terms[term] + ' ' + number.decode('utf-8')
            else:
                terms[term] = number.decode('utf-8')

    return terms







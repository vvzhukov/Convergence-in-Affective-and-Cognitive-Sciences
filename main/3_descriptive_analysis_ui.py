import pandas as pd
import re
from meshtree_ui import tree
from ast import literal_eval


in_csv = "E:/Research/Data_pubmed/affective_data23/affective_cit_bal_full_data.csv"
out_csv = "E:/Research/Data_pubmed/affective_data23/affective_cit_bal_full_data_subjectareas.csv"


### IMPORT DATA ###

df0 = pd.read_csv(in_csv)

all_data = df0

# sort by year
#all_data = all_data.sort_values(by=['year'])

### Fixing publication years

columns = empty,A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,Z = [],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]

## Adding subject areas of major topic terms
mt_column = all_data["mesh_major_UI"].to_list()

sa_codes = {'A': 'Anatomy',
            'B': 'Organisms',
            'C': 'Diseases',
            'D': 'Drugs and Chemicals',
            'E': 'Analytical, Diagnostic and Therapeutic Techniques and Equipment',
            'F': 'Psychiatry and Psychology',
            'G': 'Phenomena and Psychology',
            'H': 'Disciplines and Occupations',
            'I': 'Anthropology, Education, Sociology, and Social Phenomena',
            'J': 'Technology, Industry, Agriculture',
            'K': 'Humanities',
            'L': 'Information Science',
            'M': 'Named Groups',
            'N': 'Health Care',
            'V': 'Publication Characteristics',
            'Z': 'Geographicals'}
tree = tree() # mesh tree dictionary
sa_column = []
for row in mt_column:
    # deal with empty lists
    row = row.replace('] [', ',')
    row = row.replace('[,', '[')
    row = row.replace(',]', ']')
    print(row)
    row = literal_eval(row)
    subject_areas = []

    if row:
        for term in row:
            # look in dictionary for ids
            term = bytes(term.capitalize(), 'utf-8')
            ids = tree.get(term)
            if not ids:
                continue
            keys = []
            keys = re.findall('[A-Z]',ids, re.IGNORECASE) # list of all letter abbreviations for subject areas for a term

            keys = list(set(keys)) # remove duplicates
            # convert to actual subject headings
            sa = ""
            if keys: # check if there are any subject areas
                for key in keys:
                    sa = sa_codes.get(key)
                    subject_areas.append(sa)
            else:
                subject_areas.append(sa)
    # remove duplicates from subject_areas and append to column
    subject_areas = list(set(subject_areas))
    sa_column.append(subject_areas)
    # adding binary flags

    # if subject areas match, change binary flag to 1
    if 'Anatomy' in subject_areas:
        A.append(1)
    else:
        A.append(0)
    if 'Organisms' in subject_areas:
        B.append(1)
    else:
        B.append(0)
    if 'Diseases' in subject_areas:
        C.append(1)
    else:
        C.append(0)
    if 'Drugs and Chemicals' in subject_areas:
        D.append(1)
    else:
        D.append(0)
    if 'Analytical, Diagnostic and Therapeutic Techniques and Equipment' in subject_areas:
        E.append(1)
    else:
        E.append(0)
    if 'Psychiatry and Psychology' in subject_areas:
        F.append(1)
    else:
        F.append(0)
    if 'Phenomena and Psychology' in subject_areas:
        G.append(1)
    else:
        G.append(0)
    if 'Disciplines and Occupations' in subject_areas:
        H.append(1)
    else:
        H.append(0)
    if 'Anthropology, Education, Sociology, and Social Phenomena' in subject_areas:
        I.append(1)
    else:
        I.append(0)
    if 'Technology, Industry, Agriculture' in subject_areas:
        J.append(1)
    else:
        J.append(0)
    if 'Humanities' in subject_areas:
        K.append(1)
    else:
        K.append(0)
    if 'Information Science' in subject_areas:
        L.append(1)
    else:
        L.append(0)
    if 'Named Groups' in subject_areas:
        M.append(1)
    else:
        M.append(0)
    if 'Health Care' in subject_areas:
        N.append(1)
    else:
        N.append(0)
    if 'Publication Characteristics' in subject_areas:
        V.append(1)
    else:
        V.append(0)
    if 'Geographicals' in subject_areas:
        Z.append(1)
    else:
        Z.append(0)


all_data['subject_areas'] = sa_column
all_data['A'] = A
all_data['B'] = B
all_data['C'] = C
all_data['D'] = D
all_data['E'] = E
all_data['F'] = F
all_data['G'] = G
all_data['H'] = H
all_data['I'] = I
all_data['J'] = J
all_data['K'] = K
all_data['L'] = L
all_data['M'] = M
all_data['N'] = N
all_data['V'] = V
all_data['Z'] = Z


# for node network
all_data[['pmid','A','B','C','D','E','F','G','H','I','J','K','L','M','N','V','Z']].to_csv(out_csv, sep=',', index=False, encoding="utf-8")


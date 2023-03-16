'''
Script for generation of vector data from annotated TM files
As described in AAAL 2023 presentation,
Davidson & Minnillo "'In a word' Transition in L2 English high-stakes writing"

Author: Sam Davidson
Copyright 2023
Distributed under terms of GNU General Public License v3
'''

import pandas as pd
import copy

#load TM annotation data
df = pd.read_csv('/Users/samdavidson/Documents/projects/transition_markers/merged_tm_data.csv')
#Extract list of TM names, lowercase, and remove duplicates
tms = list(df['TM target'].dropna())
tms_lower = [item.lower() for item in tms]
tms_set = set(tms_lower)

#create dict to track TM counts
temp_dict = {item:0 for item in tms_set}
#create dicts to track ALL uses of TMx and ACCURATE uses of TMx
document_dict_all = {}
document_dict_accurate = {}

#iterate through dataframe and track counts for each TM (ALL and ACCURATE)
for ind in df.index:
    #If temp vector exists for this essay, simply increment at the appropriate TM index
    filename = df['Filename'][ind]
    try:
        marker = df['TM target'][ind].lower()
    except:
        marker = ''
    accuracy = df['Form Accuracy'][ind]
    if filename in document_dict_all:
        document_dict_all[filename][marker] += 1
        print(f'Current count of {marker}: {document_dict_all[filename][marker]}')
        if accuracy:
            document_dict_accurate[filename][marker] += 1
    #Otherwise, create a new empty vector for the essay and indicate score level
    else:
        document_dict_all[filename] = copy.deepcopy(temp_dict)
        document_dict_accurate[filename] = copy.deepcopy(temp_dict)
        score = df['Score Level'][ind]
        if score == 'low':
            score_numeric = 0
        elif score == 'medium':
            score_numeric = 1
        else:
            score_numeric = 2
        document_dict_all[filename]['score'] = score_numeric
        document_dict_accurate[filename]['score'] = score_numeric
        if marker != '':
            document_dict_all[filename][marker] += 1
            if accuracy:
                document_dict_accurate[filename][marker] += 1

#Create output dataframes and write to file
out_df_all = pd.DataFrame.from_dict(document_dict_all, orient='index')
out_df_acc = pd.DataFrame.from_dict(document_dict_accurate, orient='index')

print(out_df_all)

out_df_all.to_csv('tm_vector_data_all.csv')
out_df_acc.to_csv('tm_vector_data_acc.csv')
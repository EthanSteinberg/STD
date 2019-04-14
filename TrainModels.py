#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy
import csv
import numpy as np
from collections import defaultdict
import pickle


# In[2]:


with open('data/disease_map.pkl', 'rb') as f:
    disease_map = pickle.load(f)
    
with open('data/values_per_column.pkl', 'rb') as f:
    values_per_column = pickle.load(f)


# In[3]:


with open('data/stanford_blueprint_datathon_2019_data.csv') as f:
    reader = csv.reader(f)
    header = next(reader)
    
    header_map = {name: i for i, name in enumerate(header)}
    
diseases = ['chlamydia', 'gential_warts', 'gonorrhea', 'herpes', 'hpv',
            'other_std', 'parasitic', 'std_screen', 'syphilis', 'trich']

column_to_name = {}
index = 0
for i, name in enumerate(header):
    if i in values_per_column:
        column_to_name[index] = name + "|" + ''
        index += 1
        
        for val in values_per_column[i]: 
            column_to_name[index] = name + "|" + val
            index += 1
    else:
        column_to_name[index] = name
        index += 1

name_to_column = {column: index for index, column in column_to_name.items()}


# In[4]:


policy_dictionary = {}

def add_to_dict(name):
    if name not in policy_dictionary:
        next_index = len(policy_dictionary)
        policy_dictionary[name] = next_index

with open('data/education_policy.csv') as f:
    reader = csv.reader(f, delimiter='\t')
    header = next(reader)

    header_map = {name: i for i, name in enumerate(header)}
    
    rows = list(reader)
    
    for row in rows:
        for h, column in zip(header[1:], row[1:]):
            if column == 'both' or column == 'std':
                add_to_dict(h + "|std")
            if column == 'both' or column == 'sex':
                add_to_dict(h + "|sex")
            if column == '1' or column == '2':
                add_to_dict(h + "|has")
            if column == '2':
                add_to_dict(h + "|focus")
    
    policy_to_name = {i: name for name, i in policy_dictionary.items()}
    policy_map = {}
    for row in rows:
        values = [0 for _ in range(len(policy_dictionary))]
        for h, column in zip(header[1:], row[1:]):
            if column == 'both' or column == 'std':
                values[policy_dictionary[h + "|std"]] = 1
            if column == 'both' or column == 'sex':
                values[policy_dictionary[h + "|sex"]] = 1
            if column == '1' or column == '2':
                values[policy_dictionary[h + "|has"]] = 1
            if column == '2':
                values[policy_dictionary[h + "|focus"]] = 1
            
            assert column in ('0', '1', '2', 'both', 'std', 'sex', '')
            
        policy_map[row[header_map['State']]] = values


# In[5]:


# print('\n'.join(a for a in name_to_column.keys() if a.startswith('state')))

dc_column = name_to_column['state|Washington, DC']


# In[6]:


disease_to_index = {disease: i for i, disease in enumerate(diseases)}

for target_disease in diseases:

    train_year = '2017-04-01'

    def get_x_y(year, target_disease):
        labels = np.load('data/labels/' + year + '.npy')[:, disease_to_index[target_disease]]
        data = np.load('data/rows/' + year + '.npy')
        non_nan_labels = ~np.isnan(labels) & (data[:, dc_column] == 0)
        return data[non_nan_labels, :], labels[non_nan_labels]
        
    train_x, train_y = get_x_y(train_year, target_disease)



    policy_names = []

    def get_policy_x(data):
        result = np.zeros((data.shape[0], len(policy_dictionary)))
        
        for row_index in range(data.shape[0]):
            for state, state_policies in policy_map.items():
                state_index = name_to_column['state|' + state]
                if data[row_index, state_index] == 1:
                    result[row_index, :] = state_policies
                    break
            else:
                print('Could not find state?', state)
                print(name_to_column)
                print(1/0)
        
        final_columns = []
        for column, name in policy_to_name.items():
            if name in ('Contraception STD education|has', 'Statewide policy to teach|std'):
                final_columns.append(result[:, column])
                policy_names.append(name)
        
        return np.column_stack(final_columns)

    final_names = []

    def clean_for_policy_x(data):
        temp = data.copy()
        final_columns = []
        for name, column in name_to_column.items():
            if (
                name.startswith('age') or 
                name.startswith('gender') or 
                name.startswith('income') or 
                name.startswith('education')
            ) and name not in ('age|', 'age|35-44 years old', 
                               'gender|', 'gender|Male', 'income|', 'income|$45,000 - $49,999', 'education|', 'education|High School'):
                final_columns.append(temp[:, column])
                final_names.append(name)
                
        return np.column_stack(final_columns)
            
    policy_x = get_policy_x(train_x)
    cleaned_train_x = clean_for_policy_x(train_x)


    # In[40]:


    import statsmodels.api as sm

    p_train_x = np.hstack((cleaned_train_x, policy_x))

    X = sm.add_constant(p_train_x)
    model = sm.OLS(train_y, X)

    def get_name(i):
        if i < len(final_names):
            return final_names[i]
        else:
            return 'Policy|' + policy_names[i - len(final_names)]
        
    for i in range(1, len(model.exog_names)):
        model.exog_names[i] = get_name(i - 1)

    results = model.fit()

    # print(results.summary())

    # In[10]:

    print('disease', target_disease)

    print(len(train_y))
    for i, name in enumerate(policy_names):
        index = i + len(final_names) + 1
        print(f'{results.params[index]:.2} ± {results.bse[index]:.2} {results.pvalues[index]:.2}')

print(policy_names)
'''
Script for essay classification using multiple classifiers
and feature weight extraction from SVM classifier.
As described in AAAL 2023 presentation,
Davidson & Minnillo "'In a word' Transition in L2 English high-stakes writing"

Author: Sam Davidson
Copyright 2023
Distributed under terms of GNU General Public License v3
'''


import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn import svm
from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.inspection import permutation_importance

#Load vector data created by running create_vector_data.py
df = pd.read_csv('tm_vector_data_all.csv')

#extract TM names and create dict to align indices with TM names.
tm_names = df.columns[1:]
tm_indices = {tm_names[i]:i for i in range(len(tm_names))}

#Accumulation lists to track multiple runs
LR_list = []
SVM_list = []
RF_list = []
MLP_list = []

#Dictionary to track perm importance of TMs across iterations.
SVM_perm_importance_list = {}

#Run 10 iterations of each classifier with random train/test split each time.
for i in range(10):
    #randomly split data - 30% test, 70% train.
    train, test = train_test_split(df, test_size=0.3)

    scaler = StandardScaler()

    #extract data from dataframe
    y_train = train.iloc[:,-1]
    X_train = train.iloc[:,1:-1]
    train_ids = train.iloc[:,0]

    y_test = test.iloc[:,-1]
    X_test = test.iloc[:,1:-1]
    test_ids = test.iloc[:,0]

    # scale training data to improve classification accuracy
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.fit_transform(X_test)

    #Logistic regression classifier
    LR = LogisticRegression(random_state=0, solver='lbfgs', multi_class='multinomial').fit(X_train, y_train)
    LR.predict(X_test)
    LR_list.append(LR.score(X_test,y_test))

    #SVM classifier
    SVM = svm.SVC(decision_function_shape="ovo", random_state=0).fit(X_train, y_train)
    SVM.predict(X_test)
    SVM_list.append(SVM.score(X_test, y_test))

    #Calculate feature importance for each feature
    #Add feature importance for each feature to the SVM_perm_importance dictionary (TM index as key)
    perm_importance = permutation_importance(SVM, X_test, y_test, n_repeats=10, random_state=0)
    sorted_idx = perm_importance.importances_mean.argsort()
    result_dict = {}
    for idx in np.flip(sorted_idx):
        result_dict[tm_names[idx]] = perm_importance.importances_mean[idx]
    for key in result_dict:
        if key not in SVM_perm_importance_list:
            SVM_perm_importance_list[key] = []
        SVM_perm_importance_list[key].append(result_dict[key])

    #Random forest classifier
    RF = RandomForestClassifier(n_estimators=1000, max_depth=10, random_state=0).fit(X_train, y_train)
    RF.predict(X_test)
    RF_list.append(RF.score(X_test, y_test))

    #Multi-layer perceptron classifier
    NN = MLPClassifier(solver='lbfgs', alpha=1e-5, hidden_layer_sizes=(150, 10), random_state=1).fit(X_train, y_train)
    NN.predict(X_test)
    MLP_list.append(NN.score(X_test, y_test))

#Calculate the mean feature weight for each feature (from weights accumulated over iterations)
mean_dict = {}
for key in SVM_perm_importance_list:
    mean_dict[key] = np.average(SVM_perm_importance_list[key])

#Sort dictionary from highest to lowest and print
sorted_dict = dict(sorted(mean_dict.items(), key=lambda item: item[1], reverse=True))
for key in sorted_dict:
    print(f'{key}: {sorted_dict[key]:.5f}')

#Print overall classification accuracy for each classifier
print(f'LR: {np.mean(LR_list)}')
print(f'SVM: {np.mean(SVM_list)}')
print(f'RF: {np.mean(RF_list)}')
print(f'MLP: {np.mean(MLP_list)}')
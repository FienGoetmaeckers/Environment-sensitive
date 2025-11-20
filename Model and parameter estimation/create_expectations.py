# -*- coding: utf-8 -*-
"""
Created on Tue Jan 21 2025

@author: fgoetmae
#code to calculate the smooth-like and rough-like predictions per participant, per scenario
"""
from scenarios import hiddenlist, ob_b_list, highlighted_list
from solving_models import  GP
import numpy as np
import pandas as pd
import random
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, WhiteKernel
import math
from scipy.optimize import minimize
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
import os

L = W = 15
epsilon = 0.0001

#step 1: read in all the participant's strategies
os.chdir("C:/Users/fgoetmae/OneDrive - UGent/Documents/Projects/Context/data/Exp 3")
df1 = pd.read_csv("M1_EstExp3.csv")
nr_p = len(df1)
#and read in all the scenarios (done in the import section)
nr_sc = len(highlighted_list)

#per participant, per scenario, check what the prediction is for lr and ls
#we save this is a new dataframe
data = {'Participant' : [value for value in list(df1.Participant) for i in range(nr_sc)],
        'Grid': [value for i in range(nr_p) for value in range(nr_sc)],
        'ls': [value for value in list(df1.l_fit_s) for i in range(nr_sc)],
        'lr': [value for value in list(df1.l_fit_r) for i in range(nr_sc)]}
data = pd.DataFrame(data)


e_s = []
e_r = []
for i in range(len(data)):
#☺for i in range(len(data)): #we loop over our newly created dataframe
    #read in all relevant info
    #first about the scenario
    grid = data.Grid[i]
    hidden = np.array(hiddenlist[grid])
    observed_bandit = ob_b_list[grid]
    h = highlighted_list[grid]
    #then about the participant
    ls = data.ls[i]
    lr = data.lr[i]
    '''
    generate expectations
    '''
    #for smooth-like generalization
    m, s = GP(observed_bandit, W, L, ls, hidden)
    m = [x*90 + 40 for x in m] #rescale
    e_s.append(m[h])
    #for rough-like generalization
    m, s = GP(observed_bandit, W, L, lr, hidden)
    m = [x*90 + 40 for x in m] #rescale
    e_r.append(m[h])

data["smooth expectations"] = e_s
data["rough expectations"] = e_r

#data.to_csv("expectations.csv", index = False)

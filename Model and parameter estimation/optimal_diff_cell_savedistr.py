# -*- coding: utf-8 -*-
"""
Created on Tue Sep 10 14:22:07 2024

@author: fgoetmae
#code to identify a setting with the best diagnostic to estimate whether the low l or high l is used
"""

from create_grids import bivariate
from solving_models import  GP
import numpy as np
import random
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, WhiteKernel
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
import pandas as pd
from csv import DictWriter


'''
generalization from data: exp 1 values for full model
'''
list_ls = [4.1608738, 10.3343442, 0.7071660,  0.6794611,  0.6584868,  5.7018193,  0.6149489, 13.9859408,     24.0988100,  4.6330422,  0.7150563, 23.2631984,  2.5948218,  0.7136113,  0.7066248,  5.7576629, 0.6227602,  0.6632027,  0.7054156,  0.7002995,  6.1189279, 19.7087356,  0.5416021, 16.2058469,  0.7782463, 14.3380575,  9.6776455,  9.7649441,  0.6687106,  6.3757237,  4.7712351,  0.6313652,  0.9108906,  0.7304288, 16.3619166, 17.7473688, 18.6467307,  0.7132887, 19.7960578,  5.8875905,  0.6185591,  0.5788048,  0.6244170,  0.5566496,  0.6113044,  0.5836064, 18.8624007,  0.6536863, 12.5406448,  0.6508723,  0.6069719, 15.0893895,  0.7945095,  0.6008702, 18.9917115,  0.6579296,  0.6730909, 22.4329920, 16.9321356,  9.0711101,  0.7414859, 15.6871469,  0.5539195,  0.7068033,  0.8521118,  0.6149213,  7.1567004, 12.4672124, 14.1205619, 21.2833861, 34.2062894,  0.7757130,  0.7581152, 15.2110133,  0.7548698,  0.5803057,  0.4808764,  4.0896220,  0.7976874,  0.6509268,  0.6481600,  0.5458774, 13.2756488, 15.0358966, 17.4228757,  6.9967272, 17.5455604,  1.5019089, 10.2626428,  0.6532609]
list_lr = [0.8396710,  0.8205731,  0.7861133,  0.7286774,  0.7032445,  0.2750044,  0.7585127,  0.4879993,  0.6589008,  0.6712607,  0.7471450,  0.7628496,  0.7229000,  0.7211728,  0.6715970,  0.5009663,  0.5213750,  0.6060338,  0.8213059,  0.6876033,  0.8344849,  0.6330700,  3.7146931,  0.5793105, 25.9459119,  0.6349442,  7.5038146,  0.8339261,  0.8782309,  1.0683338,  0.8379048,  0.5755516,  1.5517908,  0.7164552,  1.2301480,  0.8417861, 0.8376779,  0.8438539,  0.5015566,  0.7999786,  0.6307790,  0.5234703,  0.5794395,  0.5902489,  0.8198601,  0.5488083,  0.5321804,  0.6245191, 0.7475055,  0.7557132,  0.5760962,  0.6701107,  0.8120384,  0.7273776,  0.4952424,  0.6134287,  1.0668468,  0.7467530,  0.4845266,  0.5439764, 0.7795013,  0.6255544,  0.6521429,  0.7157889,  0.7445492,  0.6033741,  0.6784156,  0.7891768,  0.6806263,  0.5151227,  0.6529924,  0.6856096, 0.6410714,  0.7813482,  0.6167053,  0.4632340,  0.6271566,  4.1070529,  0.7336817,  0.7779265,  0.6267887,  0.6018431,  0.6033024,  0.8132231, 0.8367287,  0.6162833, 18.7460874, 22.9460389,  0.6824470,  0.6804047]

#filter smooth and rough generalization parameters so that they do not overlap:
list_ls = [i for i in list_ls if i > np.median(list_lr)]
list_lr = [i for i in list_lr if i < np.median(list_ls)]
    
#we are aiming to create a 15x15 grid, with only two cells open
#we want to offer one forced choice, for which we ask what the participants expect
W = L = 15
l =  np.sqrt(8) #smooth grid
epsilon = 0.0001
ls = np.sqrt(8)
lr = 1
'''
so first, we create the visible grid
'''

scenarios = pd.DataFrame()

#step one; generate grid
bandit = bivariate(W, L, l, np.random.randint(65, 86), 5) #won't be used
hidden = np.array([True]*W*L) 
observed_bandit = [[] for value in bandit]
opened_cells = []
observations = []

#open one random:
tile_number = tile1
reward = r1    
bandit[tile_number] = reward
#save that this cell has been opened before and save the history in observed_bandit
opened_cells.append(tile_number)
observations.append(reward)
hidden[tile_number] = False #save that this cell has been opened
observed_bandit[tile_number].append(reward)

#open a second one
tile_number = tile2
reward = r2
bandit[tile_number] = reward
#save that this cell has been opened before and save the history in observed_bandit
opened_cells.append(tile_number)
observations.append(reward)
hidden[tile_number] = False #save that this cell has been opened
observed_bandit[tile_number].append(reward)

'''
important check: is this environment equally likely rough as smooth?
'''

hidden2 = hidden.reshape(L, W)
xlist = [] #list with the coordinates of the data points (list of doubles)
rewardlist = []
for i in range(0, len(hidden2)):
    for j in range(0, len(hidden2[0])):
        if hidden2[i][j] == False: #then we have an observation for this cell
            for observation in observed_bandit[i*W + j]:
                xlist.append([i, j])
                rewardlist.append(observation)
                
cells =[[i,j] for i in range(0, W) for j in range(0, L)]    
#how likely for smooth?
kernel = RBF(length_scale= ls) + WhiteKernel(epsilon, "fixed") 
gp = GaussianProcessRegressor(kernel=kernel)
gpc = gp.fit(xlist, [(reward-40)/90 for reward in rewardlist])
print(f"Fit: {gpc.score(xlist, [(reward-40)/90 for reward in rewardlist])}")
#how likely for rough?
kernel = RBF(length_scale= lr) + WhiteKernel(epsilon, "fixed") 
gp = GaussianProcessRegressor(kernel=kernel)
gpc = gp.fit(xlist, [(reward-40)/90 for reward in rewardlist])
print(f"Fit: {gpc.score(xlist, [(reward-40)/90 for reward in rewardlist])}")
'''
#now we need to find the most differentiable cell
'''

cell = opent
    
smooth_exp = []
smooth_unc = []
smooth_max = []
rough_exp = []
rough_unc = []
rough_max = []
#they are diagnostic if the distance (cohen's d) between answers for smooth and rough answers are maximal
#we try 100 different smooth and 100 different rough behaviours
for lr in list_lr: 
    #according to this generalization, what would they respond?
    m, s = GP(observed_bandit, W, L, lr, hidden)
    m = [x*90 + 40 for x in m] #rescale
    s = [x*90 for x in s] #rescale
    rough_exp.append(m[cell]) #fill in!
    rough_unc.append(s[cell]) #fill in!
    rough_max.append(m[cell] + s[cell])
       
for ls in list_ls:
    m, s = GP(observed_bandit, W, L, ls, hidden)
    m = [x*90 + 40 for x in m] #rescale
    s = [x*90 for x in s] #rescale
    smooth_exp.append(m[cell]) #fill in!
    smooth_unc.append(m[cell]) #fill in!
    smooth_max.append(m[cell]+ s[cell])
    
#calculate diagnostic value of cell
d = np.mean(smooth_exp) - np.mean(rough_exp) 
t = stats.ttest_ind(smooth_exp, rough_exp)

best_cell = cell


"""
show distributions for chosen cell
"""
smooth_exp = []
smooth_unc = []
smooth_max = []
rough_exp = []
rough_unc = []
rough_max = []
#they are diagnostic if the distance (cohen's d) between answers for smooth and rough answers are maximal
#we try 100 different smooth and 100 different rough behaviours
for lr in list_lr: 
    #according to this generalization, what would they respond?
    m, s = GP(observed_bandit, W, L, lr, hidden)
    m = [x*90 + 40 for x in m] #rescale
    s = [x*90 for x in s] #rescale
    rough_exp.append(m[cell]) #fill in!
    rough_unc.append(s[cell]) #fill in!
    rough_max.append(m[cell] + s[cell])
       
for ls in list_ls:
    m, s = GP(observed_bandit, W, L, ls, hidden)
    m = [x*90 + 40 for x in m] #rescale
    s = [x*90 for x in s] #rescale
    smooth_exp.append(m[cell]) #fill in!
    smooth_unc.append(m[cell]) #fill in!
    smooth_max.append(m[cell]+ s[cell])
    

d = np.mean(smooth_exp) - np.mean(rough_exp) 
t = stats.ttest_ind(smooth_exp, rough_exp)
plt.rcParams.update({'font.size': 10})
plt.hist(smooth_exp, label = 'smooth generalization', color = "g")
plt.hist(rough_exp, label = "rough generalization", color = "r")
plt.xlabel('expected value for this cell')

plt.show()


#transform the output for a JS file used in the experiment
def cell_coord_to_number(x,y,W):
	return y*W+x

def transform_scenario(highlighted, cell1, value1, cell2, value2, W=15):
    print(cell1)
    (x1, y1) = cell1
    nr1 = cell_coord_to_number(x1,y1,W)
    print(cell2)
    (x2, y2) = cell2
    nr2 = cell_coord_to_number(x2,y2,W)
    (x0, y0) = highlighted
    nr0 = cell_coord_to_number(x0,y0,W)
    opened_cells = [nr1, nr2, nr0]
    opened_rewards = [[] for i in range(W*L)]
    opened_rewards[nr1] = [value1]
    opened_rewards[nr2] = [value2]
    opened_rewards[nr0] = ["?"]
    
    return(opened_cells, opened_rewards)

results = {"scenario": sc, "smooth_exp": smooth_exp, "rough_exp": rough_exp, "mean_s": np.mean(smooth_exp), "mean_r": np.mean(rough_exp)}
with open("scenarios.csv", 'a') as f_object:
    field_names = ["scenario", "smooth_exp", "rough_exp", "mean_s", "mean_r"]
    dictwriter_object = DictWriter(f_object, fieldnames=field_names)
    dictwriter_object.writerow(results)

    f_object.close()

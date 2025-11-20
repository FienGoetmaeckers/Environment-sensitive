# -*- coding: utf-8 -*-
"""
GP-UCB model
"""
import random
import math
import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, WhiteKernel


def GP(observed_bandit, W, L, l_fit, hidden, noise=True, epsilon = 0.0001):
    '''
    This function starts from the observations,
    for which the coordinates are saved in xlist
    and the observed rewards in rewardlist
    since the GP assumes that not observed states have as default a value of 0,
    we rescale the reward to have a mean 0 and vary between max bounds of -0.5, 0.5
    
    Parameters
    ----------
    observed_bandit : the observations
    W : width of the bandit.
    L : lenght of the bandit.
    l_fit: the generalization strength with which the participant smooths out the observed rewards
    hidden : list of which cells are hidden. True if hidden, False if the reward is known.
    noise: True if the participant assumes noise in their observations
    epsilon: the assumed noise, 
            measured as the variance of the reward around the mean
            std is 1 up 100, 
            var is 0.01**2 = 0.0001
    
    Returns
    -------
    Returns the mean function m(x) and the uncertainty function s(x) per tile.
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
    
    kernel = RBF(l_fit, "fixed")
    if noise:
        kernel += WhiteKernel(epsilon, "fixed")
        
    gp = GaussianProcessRegressor(kernel=kernel)
    gp.fit(xlist, [(reward-40)/90 for reward in rewardlist])
    mlist, sigmalist = gp.predict(cells, return_std=True)
             
    return mlist, sigmalist


def softmax(UCB, W, L, tau):
    """
    translates the UCB (the inflated expectation) to probabilities to sample tiles

    Parameters
    ----------
    UCB : the inflated expectations per tile.
    W : width.
    L : length.
    tau : softmax temperature, the level of undirected (random) exploration.

    Returns
    -------
    the probability to sample a tile.

    """
    
    UCB = [ucb - max(UCB) for ucb in UCB] #rescaling to avoid overflows
    exp_list = np.array([np.exp(ucb/tau) for ucb in UCB])
    Z = sum(exp_list)
    returnlijst = [value/Z for value in exp_list]
    return returnlijst

def localizer(UCB, prior_choice, W, L):
    cells =[[i,j] for i in range(0, W) for j in range(0, L)]  
    IMD =  [1/(abs(x-prior_choice[0]) + abs(y-prior_choice[1])) if [x,y]!=prior_choice else 1 for [x,y] in cells]
    UCBloc = [IMD[i]*UCB[i] for i in range(W*L)]
    

    return UCBloc

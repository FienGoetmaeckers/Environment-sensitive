# -*- coding: utf-8 -*-
"""
script to estimate the model parameters of one (behavioural/computational) participant
"""
import math
import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, WhiteKernel
import scipy
from scipy.optimize import minimize


"""
Prior with which we want to estimate the model parameters:
"""
#prior values of model parameters, used to start the search and to bias the optimization
x0 = [10, 0.5, 0.01]
#std of the priors on the model parameters, used to bias the optimization
xstd = [8, 0.2, 0.2] #actually is this the variance, just wrong name but correct values and use
#covariance matrix of the priors on the model parameters
xcov = np.diag(xstd)

bounds = np.exp(np.array([(-5, 6), (-5, 3), (-5, 3)]))

def estimate_1env(W, L, nr_trials, nr_blocks, data):
    """
    estimates the model parameters of the data
    for an experiment with one environment
    using leave-one-out method where one round is left out
    for cross validation
    
    1 estimation is made, using the entire dataset
    one value of l, beta and tau
            
    then use the median of the estimations as the final result
    and the quartile deviation as a measurement of variance
    
    Parameters
    ----------
    W : int
        width of the grid.
    L : int
        length of the grid.
    nr_trials : int
        nr of trials per round.
    nr_blocks : int
        nr of blocks per experiment.
    data : dataframe
        all the participant's data.

    Returns
    -------
    the estimated parameters and the NLL belonging to that estimation.

    """
    
    #make usefull lists of the data
    datalast = data.query('trial_nr == {}'.format(nr_trials-1))  #this is a dataframe that only contains the data of the last trial of every grid
    #lists with info per grid
    initial_opened = [value for value in datalast.initial_opened]
    
    l_fit_est_list = [0]*nr_blocks
    beta_est_list = [0]*nr_blocks
    tau_est_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        data_partial = data.query('block_nr != {}'.format(out_index))
        args = (W, L, nr_trials, initial_opened[:out_index] + initial_opened[out_index+1:],
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward])
        
        res = minimize(fun=wrapper, x0 = np.log(x0), args=args, method='SLSQP', bounds=np.log(np.array(bounds)))
        (l_fit_est, beta_est, tau_est) = np.exp(res.x)
        
        l_fit_est_list[out_index] = l_fit_est
        beta_est_list[out_index] = beta_est
        tau_est_list[out_index] = tau_est
        
        #cross validation
        data_partial = data.query('block_nr == {}'.format(out_index))
        cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est, tau_est, 
                        [initial_opened[out_index]], 
                        [value for value in data_partial.selected_choice],
                        [value for value in data_partial.reward], 
                        [value for value in data_partial.average_reward])
        
        NLL_list[out_index] = cross_val
        
    print("for the entire experiment we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta = %.3f +- %.3f, tau = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2, 
                  np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resx = (np.median(l_fit_est_list), np.median(beta_est_list), np.median(tau_est_list))
    resfun = np.sum(NLL_list)
    
        
    return (resx, resfun)




def estimate_2env(W, L, nr_trials, nr_blocks, data):
    """
    estimates the model parameters of the data
    for an experiment with two environments
    using leave-one-out method where one round is left out
    for cross validation
    
    8 different estimations will be made
        * environment specific strategy: 3 par for smooth, 3 par for rough
        * non-environment specific strategy: 3 par for total exp
        * 3 models with 1 environment-sensitive parameter
            - generalization
            - uncertainty guided exploration
            - random exploration
        * 3 models with 2 environment-sensitive paramters
            - non-environment specific generalization
            - non-environment specific uncertainty guided expl
            - non-environment specific random expl

            
    then use the median of the estimations as the final result
    and the quartile deviation as a measurement of variance
    
    Parameters
    ----------
    W : int
        width of the grid.
    L : int
        length of the grid.
    nr_trials : int
        nr of trials per round.
    nr_blocks : int
        nr of blocks per experiment.
    data : dataframe
        all the participant's data.

    Returns
    -------
    the estimated parameters and the NLL belonging to that estimation.

    """
    
    #read in the condition and assign smoothness to the blocks
    condition = int(data["assigned_condition"].values[0][1:-1])
    if (condition <2):
        smoothness_order = ["smooth", "rough"]
       
    else:
        smoothness_order = ["rough", "smooth"]
    
    #make usefull lists of the data
    datalast = data.query('trial_nr == {}'.format(nr_trials-1)) 
    #lists with info per grid
    initial_opened = [value for value in datalast.initial_opened]
    
    #we will save the estimated parameters and NLL in results
    results = []
    
    """
    1st estimations: 
        environment-specific strategy:
        for all three parameters different per environment
    """
    """
    nr_blocks_per_con = int(nr_blocks/2)
    
    #for the first block:
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    l_fit_est_list = [0]*nr_blocks_per_con
    beta_est_list = [0]*nr_blocks_per_con
    tau_est_list = [0]*nr_blocks_per_con
    NLL_list = [0]*nr_blocks_per_con
    
    print(smoothness_order[0])
    for out_index in range(nr_blocks_per_con):
        data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))
        args = (W, L, nr_trials, 
                initial_opened[:out_index] + initial_opened[out_index+1:nr_blocks_per_con],
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward])
        
        res1 = minimize(fun=wrapper, x0 = np.log(x0), args=args, method='SLSQP', bounds=np.log(np.array(bounds)))
        (l_fit_est, beta_est, tau_est) = np.exp(res1.x)
        l_fit_est_list[out_index] = l_fit_est
        beta_est_list[out_index] = beta_est
        tau_est_list[out_index] = tau_est
        
        #cross validation:
        data_partial = data_firsthalf.query('block_nr == {}'.format(out_index))
        cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward])

        NLL_list[out_index] = cross_val
        
    print("for the first block we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta = %.3f +- %.3f, tau = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2, 
                  np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    res1x = (np.median(l_fit_est_list), np.median(beta_est_list), np.median(tau_est_list))
    res1fun = np.sum(NLL_list)
    
    #for the second block:
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    l_fit_est_list = [0]*nr_blocks_per_con
    beta_est_list = [0]*nr_blocks_per_con
    tau_est_list = [0]*nr_blocks_per_con
    NLL_list = [0]*nr_blocks_per_con
    
    print(smoothness_order[1])
    for out_index in range(nr_blocks_per_con):
        data_partial = data_secondhalf.query('block_nr != {}'.format(nr_blocks_per_con + out_index))
        args = (W, L, nr_trials,
                initial_opened[nr_blocks_per_con: nr_blocks_per_con + out_index] + initial_opened[nr_blocks_per_con + out_index+1:],
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward])
        
        res2 = minimize(fun=wrapper, x0 = np.log(x0), args=args, method='SLSQP', bounds=np.log(np.array(bounds)))
        (l_fit_est, beta_est, tau_est) = np.exp(res2.x)
        l_fit_est_list[out_index] = l_fit_est
        beta_est_list[out_index] = beta_est
        tau_est_list[out_index] = tau_est
        
        #cross validation:
        data_partial = data_secondhalf.query('block_nr == {}'.format(nr_blocks_per_con + out_index))
        cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward])

        NLL_list[out_index] = cross_val
    
    print("for the second block we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta = %.3f +- %.3f, tau = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2, 
                  np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    res2x = (np.median(l_fit_est_list), np.median(beta_est_list), np.median(tau_est_list))
    res2fun = np.sum(NLL_list)
    
    results.append(res1x)
    results.append(res1fun)
    results.append(res2x)
    results.append(res2fun)
    """
    """
    2nd estimation: 
        non-environment-sensitive strategy
        one set of model parameters for the entire model,
    this code is identical to estimate_1env
    """
    """
    (restx, restfun) = estimate_1env(W, L, nr_trials, nr_blocks, data)
    results.append(restx)
    results.append(restfun)
    """
    
    """
    3r estimations:
        one environment-sensitive parameter
    """
    '''
    3A
    environment-sensitive generalization
    one beta and tau for both environments,
    a different l per environment
    '''    

    l_fit_est_1_list = [0]*nr_blocks
    l_fit_est_2_list = [0]*nr_blocks
    beta_est_list = [0]*nr_blocks
    tau_est_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        args = (W, L, nr_trials, nr_blocks, data, 
                initial_opened,
                [value for value in data.selected_choice],
                [value for value in data.reward],
                [value for value in data.average_reward], out_index)
        
        resf = minimize(fun=wrapper3A, x0 = np.log([x0[0], x0[0], 0.5, 0.01]), args=args, method='SLSQP', bounds=[(-5, 6), (-5, 6), (-5, 3), (-5, 3)])
        (l_fit_est_1, l_fit_est_2, beta_est, tau_est) = np.exp(resf.x)
        
        l_fit_est_1_list[out_index] = l_fit_est_1
        l_fit_est_2_list[out_index] = l_fit_est_2
        beta_est_list[out_index] = beta_est
        tau_est_list[out_index] = tau_est
        
        #cross validation:
        data_partial = data.query('block_nr == {}'.format(out_index))
        if out_index < nr_blocks_per_con:
            cross_val = NLL(W, L, nr_trials, l_fit_est_1, beta_est, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the first blocks
        else:
            cross_val = NLL(W, L, nr_trials, l_fit_est_2, beta_est, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the second blocks

        NLL_list[out_index] = cross_val
    
    print("for the shared beta and tau we estimated a median of:")    
    print(r'l_fit_1 = %.3f +- %.3f, l_fit_2 = %.3f +- %.3f, beta = %.3f +- %.3f, tau = %.3f +- %.3f'
          % (np.median(l_fit_est_1_list), (np.percentile(l_fit_est_1_list, 75) - np.percentile(l_fit_est_1_list, 25))/2, 
             np.median(l_fit_est_2_list), (np.percentile(l_fit_est_2_list, 75) - np.percentile(l_fit_est_2_list, 25))/2,
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2, 
                  np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resfx = (np.median(l_fit_est_1_list), np.median(l_fit_est_2_list), np.median(beta_est_list), np.median(tau_est_list))
    resffun = np.sum(NLL_list)
    
    results.append(resfx)
    results.append(resffun)
    
    '''
    3B
    environment-sensitive uncertainty guided exploration
    one l and tau for both environments,
    a different beta per environment
    '''    

    l_fit_est_list = [0]*nr_blocks
    beta_est_1_list = [0]*nr_blocks
    beta_est_2_list = [0]*nr_blocks
    tau_est_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        args = (W, L, nr_trials, nr_blocks, data, 
                initial_opened,
                [value for value in data.selected_choice],
                [value for value in data.reward],
                [value for value in data.average_reward], out_index)
        
        resf = minimize(fun=wrapper3B, x0 = np.log([10, 0.5, 0.5, 0.01]), args=args, method='SLSQP', bounds=[(-5, 6), (-5, 3), (-5, 3), (-5, 3)])
        (l_fit_est, beta_est_1, beta_est_2, tau_est) = np.exp(resf.x)
        
        l_fit_est_list[out_index] = l_fit_est
        beta_est_1_list[out_index] = beta_est_1
        beta_est_2_list[out_index] = beta_est_2
        tau_est_list[out_index] = tau_est
        
        #cross validation:
        data_partial = data.query('block_nr == {}'.format(out_index))
        if out_index < nr_blocks_per_con:
            cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est_1, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the first blocks
        else:
            cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est_2, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the second blocks

        NLL_list[out_index] = cross_val
    
    print("for the shared l_fit and tau we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta_1 = %.3f +- %.3f, beta_2 = %.3f +- %.3f, tau = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_1_list), (np.percentile(beta_est_1_list, 75) - np.percentile(beta_est_1_list, 25))/2, 
             np.median(beta_est_2_list), (np.percentile(beta_est_2_list, 75) - np.percentile(beta_est_2_list, 25))/2,
                  np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resfx = (np.median(l_fit_est_list), np.median(beta_est_1_list), np.median(beta_est_2_list), np.median(tau_est_list))
    resffun = np.sum(NLL_list)
    
    results.append(resfx)
    results.append(resffun)
    
    '''
    3C
    environment-sensitive random exploration
    one l and beta for both environments,
    a different tau per environment
    '''    

    l_fit_est_list = [0]*nr_blocks
    beta_est_list = [0]*nr_blocks
    tau_est_1_list = [0]*nr_blocks
    tau_est_2_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        args = (W, L, nr_trials, nr_blocks, data, 
                initial_opened,
                [value for value in data.selected_choice],
                [value for value in data.reward],
                [value for value in data.average_reward], out_index)
        
        resf = minimize(fun=wrapper3C, x0 = np.log([10, 0.5, 0.01, 0.01]), args=args, method='SLSQP', bounds=[(-5, 6), (-5, 3), (-5, 3), (-5, 3)])
        (l_fit_est, beta_est, tau_est_1, tau_est_2) = np.exp(resf.x)
        
        l_fit_est_list[out_index] = l_fit_est
        beta_est_list[out_index] = beta_est
        tau_est_1_list[out_index] = tau_est_1
        tau_est_2_list[out_index] = tau_est_2
        
        #cross validation:
        data_partial = data.query('block_nr == {}'.format(out_index))
        if out_index < nr_blocks_per_con:
            cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est, tau_est_1, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the first blocks
        else:
            cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est, tau_est_2, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the second blocks

        NLL_list[out_index] = cross_val
    
    print("for the shared l_fit and beta we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta = %.3f +- %.3f, tau_1 = %.3f +- %.3f, tau_2 = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2,
             np.median(tau_est_1_list), (np.percentile(tau_est_1_list, 75) - np.percentile(tau_est_1_list, 25))/2,
             np.median(tau_est_2_list), (np.percentile(tau_est_2_list, 75) - np.percentile(tau_est_2_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resfx = (np.median(l_fit_est_list), np.median(beta_est_list), np.median(tau_est_1_list), np.median(tau_est_2_list))
    resffun = np.sum(NLL_list)
    
    results.append(resfx)
    results.append(resffun)
    
    """
    4r estimations:
        two environment-sensitive parameters
    """
    '''
    4A
    non-environment-sensitive generalization
    one l for both environments,
    a different beta and tau per environment
    '''    
    l_fit_est_list = [0]*nr_blocks
    beta_est_1_list = [0]*nr_blocks
    beta_est_2_list = [0]*nr_blocks
    tau_est_1_list = [0]*nr_blocks
    tau_est_2_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        args = (W, L, nr_trials, nr_blocks, data, 
                initial_opened,
                [value for value in data.selected_choice],
                [value for value in data.reward],
                [value for value in data.average_reward], out_index)
        
        resf = minimize(fun=wrapper4A, x0 = np.log([10, 0.5, 0.5, 0.01, 0.01]), args=args, method='SLSQP', bounds=[(-5, 6), (-5, 3), (-5, 3), (-5, 3), (-5, 3)])
        (l_fit_est, beta_est_1, beta_est_2, tau_est_1, tau_est_2) = np.exp(resf.x)
        
        l_fit_est_list[out_index] = l_fit_est
        beta_est_1_list[out_index] = beta_est_1
        beta_est_2_list[out_index] = beta_est_2
        tau_est_1_list[out_index] = tau_est_1
        tau_est_2_list[out_index] = tau_est_2
        
        #cross validation:
        data_partial = data.query('block_nr == {}'.format(out_index))
        if out_index < nr_blocks_per_con:
            cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est_1, tau_est_1, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the first blocks
        else:
            cross_val = NLL(W, L, nr_trials, l_fit_est, beta_est_2, tau_est_2, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the second blocks

        NLL_list[out_index] = cross_val
    
    print("for the shared l_fit we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta_1 = %.3f +- %.3f, beta_2 = %.3f +- %.3f, tau_1 = %.3f +- %.3f, tau_2 = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_1_list), (np.percentile(beta_est_1_list, 75) - np.percentile(beta_est_1_list, 25))/2,
             np.median(beta_est_2_list), (np.percentile(beta_est_2_list, 75) - np.percentile(beta_est_2_list, 25))/2,
             np.median(tau_est_1_list), (np.percentile(tau_est_1_list, 75) - np.percentile(tau_est_1_list, 25))/2,
             np.median(tau_est_2_list), (np.percentile(tau_est_2_list, 75) - np.percentile(tau_est_2_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resfx = (np.median(l_fit_est_list), np.median(beta_est_1_list), np.median(beta_est_2_list), np.median(tau_est_1_list), np.median(tau_est_2_list))
    resffun = np.sum(NLL_list)
    
    results.append(resfx)
    results.append(resffun)
    
    '''
    4B
    non-environment-sensitive uncertainty guided expl
    one beta for both environments,
    a different l and tau per environment
    '''    
    l_fit_est_1_list = [0]*nr_blocks
    l_fit_est_2_list = [0]*nr_blocks
    beta_est_list = [0]*nr_blocks
    tau_est_1_list = [0]*nr_blocks
    tau_est_2_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        args = (W, L, nr_trials, nr_blocks, data, 
                initial_opened,
                [value for value in data.selected_choice],
                [value for value in data.reward],
                [value for value in data.average_reward], out_index)
        
        resf = minimize(fun=wrapper4B, x0 = np.log([10, 10, 0.5, 0.01, 0.01]), args=args, method='SLSQP', bounds=[(-5, 6), (-5, 6), (-5, 3), (-5, 3), (-5, 3)])
        (l_fit_est_1, l_fit_est_2, beta_est, tau_est_1, tau_est_2) = np.exp(resf.x)
        
        l_fit_est_1_list[out_index] = l_fit_est_1
        l_fit_est_2_list[out_index] = l_fit_est_2
        beta_est_list[out_index] = beta_est
        tau_est_1_list[out_index] = tau_est_1
        tau_est_2_list[out_index] = tau_est_2
        
        #cross validation:
        data_partial = data.query('block_nr == {}'.format(out_index))
        if out_index < nr_blocks_per_con:
            cross_val = NLL(W, L, nr_trials, l_fit_est_1, beta_est, tau_est_1, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the first blocks
        else:
            cross_val = NLL(W, L, nr_trials, l_fit_est_2, beta_est, tau_est_2, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the second blocks

        NLL_list[out_index] = cross_val
    
    print("for the shared beta we estimated a median of:")    
    print(r'l_fit_1 = %.3f +- %.3f, l_fit_2 = %.3f +- %.3f, beta = %.3f +- %.3f, tau_1 = %.3f +- %.3f, tau_2 = %.3f +- %.3f'
          % (np.median(l_fit_est_1_list), (np.percentile(l_fit_est_1_list, 75) - np.percentile(l_fit_est_1_list, 25))/2, 
             np.median(l_fit_est_2_list), (np.percentile(l_fit_est_2_list, 75) - np.percentile(l_fit_est_2_list, 25))/2,
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2, 
             np.median(tau_est_1_list), (np.percentile(tau_est_1_list, 75) - np.percentile(tau_est_1_list, 25))/2,
             np.median(tau_est_2_list), (np.percentile(tau_est_2_list, 75) - np.percentile(tau_est_2_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resfx = (np.median(l_fit_est_1_list), np.median(l_fit_est_2_list), np.median(beta_est_list), np.median(tau_est_1_list), np.median(tau_est_2_list))
    resffun = np.sum(NLL_list)
    
    results.append(resfx)
    results.append(resffun)
    
    '''
    4C
    non-environment-sensitive random expl
    one tau for both environments,
    a different l and beta per environment
    '''    
    l_fit_est_1_list = [0]*nr_blocks
    l_fit_est_2_list = [0]*nr_blocks
    beta_est_1_list = [0]*nr_blocks
    beta_est_2_list = [0]*nr_blocks
    tau_est_list = [0]*nr_blocks
    NLL_list = [0]*nr_blocks
    
    for out_index in range(nr_blocks):
        args = (W, L, nr_trials, nr_blocks, data, 
                initial_opened,
                [value for value in data.selected_choice],
                [value for value in data.reward],
                [value for value in data.average_reward], out_index)
        
        resf = minimize(fun=wrapper4C, x0 = np.log([10, 10, 0.5, 0.5, 0.01]), args=args, method='SLSQP', bounds=[(-5, 6), (-5, 6), (-5, 3), (-5, 3), (-5, 3)])
        (l_fit_est_1, l_fit_est_2, beta_est_1, beta_est_2, tau_est) = np.exp(resf.x)
        
        l_fit_est_1_list[out_index] = l_fit_est_1
        l_fit_est_2_list[out_index] = l_fit_est_2
        beta_est_1_list[out_index] = beta_est_1
        beta_est_2_list[out_index] = beta_est_2
        tau_est_list[out_index] = tau_est
        
        #cross validation:
        data_partial = data.query('block_nr == {}'.format(out_index))
        if out_index < nr_blocks_per_con:
            cross_val = NLL(W, L, nr_trials, l_fit_est_1, beta_est_1, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the first blocks
        else:
            cross_val = NLL(W, L, nr_trials, l_fit_est_2, beta_est_2, tau_est, 
                  [initial_opened[out_index]], 
                  [value for value in data_partial.selected_choice],
                  [value for value in data_partial.reward], 
                  [value for value in data_partial.average_reward]) #for the second blocks

        NLL_list[out_index] = cross_val
    
    print("for the shared tau we estimated a median of:")    
    print(r'l_fit_1 = %.3f +- %.3f, l_fit_2 = %.3f +- %.3f, beta = %.3f +- %.3f, tau_1 = %.3f +- %.3f, tau_2 = %.3f +- %.3f'
          % (np.median(l_fit_est_1_list), (np.percentile(l_fit_est_1_list, 75) - np.percentile(l_fit_est_1_list, 25))/2, 
             np.median(l_fit_est_2_list), (np.percentile(l_fit_est_2_list, 75) - np.percentile(l_fit_est_2_list, 25))/2,
             np.median(beta_est_1_list), (np.percentile(beta_est_1_list, 75) - np.percentile(beta_est_1_list, 25))/2,
             np.median(beta_est_2_list), (np.percentile(beta_est_2_list, 75) - np.percentile(beta_est_2_list, 25))/2,
             np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resfx = (np.median(l_fit_est_1_list), np.median(l_fit_est_2_list), np.median(beta_est_1_list), np.median(beta_est_2_list), np.median(tau_est_list))
    resffun = np.sum(NLL_list)
    
    results.append(resfx)
    results.append(resffun)
    
    
    
    return results

    
def wrapper(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 1 l, 1 beta and 1 tau per data set
    
    Parameters
    ----------
    par : array of floats
        the array of the model parameters that need to be optimized
    *args : tuple of ...
        all the other arguments needed for NLL

    Returns
    -------
    returns the function NLL
    """
    l_fit = np.exp(par[0])
    if (l_fit == 0):
        l_fit = 10e-8
    
    beta = np.exp(par[1])
    if (beta == 0):
        beta = 10e-8
    
    tau = np.exp(par[-1])
    if (tau == 0):
        tau = 10e-8    

    (W, L, nr_trials, initial_opened, selected_choice, reward, average_reward) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L * bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    term1 = NLL(W, L, nr_trials, l_fit, beta, tau, initial_opened, selected_choice, reward, average_reward)
    term2 = NLbias((l_fit, beta, tau), x0, xcov)
    NLprob = term1 + term2
    return NLprob

    

def wrapper3A(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 2 l, 1 beta and 1 tau per data set
    extra code to split up the dataset
    
    Parameters
    ----------
    par : array of floats
        the array of the model parameters that need to be optimized
    *args : tuple of ...
        all the other arguments needed for NLL

    Returns
    -------
    returns the function NLL

    """
    
    l_fit_1 = np.exp(par[0])
    if (l_fit_1 == 0):
        l_fit_1 = 10e-8
    l_fit_2 = np.exp(par[1])
    if (l_fit_2 == 0):
        l_fit_2 = 10e-8

    beta = np.exp(par[2])
    if (beta == 0):
        beta = 10e-8
    
    tau = np.exp(par[3])
    if (tau == 0):
        tau = 10e-8
    print("par: ", l_fit_1, l_fit_2, beta, tau)
    (W, L, nr_trials, nr_blocks, data, initial_opened, selected_choice, reward, average_reward, out_index) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L * bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    nr_blocks_per_con = int(nr_blocks/2)
    if out_index < nr_blocks_per_con:
        initial_opened1 = initial_opened[:out_index] + initial_opened[out_index + 1: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con:]
    else: 
        initial_opened1 = initial_opened[: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con: out_index] + initial_opened[out_index + 1:]
    
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))  
    term1 = NLL(W, L, nr_trials, l_fit_1, beta, tau, 
                initial_opened1, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the first blocks
    
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    data_partial = data_secondhalf.query('block_nr != {}'.format(out_index))
    term1 += NLL(W, L, nr_trials, l_fit_2, beta, tau, 
                initial_opened2, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the second blocks
    
    print("prior: ", x0)
    term2 = NLbias((l_fit_1, l_fit_2, beta, tau), x0 = (x0[0], x0[0], x0[1], x0[2]), xcov = np.diag([xstd[0], xstd[0], xstd[1], xstd[2]]))
    NLprob = term1 + term2
    return NLprob


def wrapper3B(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 1 l, 2 beta and 1 tau per data set
    extra code to split up the dataset

    """
    l_fit = np.exp(par[0])
    if (l_fit == 0):
        l_fit = 10e-8
  
    beta_1 = np.exp(par[1])
    if (beta_1 == 0):
        beta_1 = 10e-8
    beta_2 = np.exp(par[2])
    if (beta_2 == 0):
        beta_2 = 10e-8
    
    tau = np.exp(par[3])
    if (tau == 0):
        tau = 10e-8
    
    (W, L, nr_trials, nr_blocks, data, initial_opened, selected_choice, reward, average_reward, out_index) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L + bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    nr_blocks_per_con = int(nr_blocks/2)
    if out_index < nr_blocks_per_con:
        initial_opened1 = initial_opened[:out_index] + initial_opened[out_index + 1: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con:]
    else: 
        initial_opened1 = initial_opened[: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con: out_index] + initial_opened[out_index + 1:]
    
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))  
    term1 = NLL(W, L, nr_trials, l_fit, beta_1, tau, 
                initial_opened1, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the first blocks
    
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    data_partial = data_secondhalf.query('block_nr != {}'.format(out_index))
    term1 += NLL(W, L, nr_trials, l_fit, beta_2, tau, 
                initial_opened2, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the second blocks

    term2 = NLbias((l_fit, beta_1, beta_2, tau), x0 = (x0[0], x0[1], x0[1], x0[2]), xcov = np.diag([xstd[0], xstd[1], xstd[1], xstd[2]]))

    NLprob = term1 + term2
    return NLprob


def wrapper3C(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 1 l, 1 beta and 2 tau per data set
    extra code to split up the dataset

    """
    l_fit = np.exp(par[0])
    if (l_fit == 0):
        l_fit = 10e-8
  
    beta = np.exp(par[1])
    if (beta == 0):
        beta = 10e-8
    
    tau_1 = np.exp(par[2])
    if (tau_1 == 0):
        tau_1 = 10e-8
    tau_2 = np.exp(par[3])
    if (tau_2 == 0):
        tau_2 = 10e-8
    
    (W, L, nr_trials, nr_blocks, data, initial_opened, selected_choice, reward, average_reward, out_index) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L + bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    nr_blocks_per_con = int(nr_blocks/2)
    if out_index < nr_blocks_per_con:
        initial_opened1 = initial_opened[:out_index] + initial_opened[out_index + 1: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con:]
    else: 
        initial_opened1 = initial_opened[: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con: out_index] + initial_opened[out_index + 1:]
    
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))  
    term1 = NLL(W, L, nr_trials, l_fit, beta, tau_1, 
                initial_opened1, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the first blocks
    
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    data_partial = data_secondhalf.query('block_nr != {}'.format(out_index))
    term1 += NLL(W, L, nr_trials, l_fit, beta, tau_2, 
                initial_opened2, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the second blocks

    term2 = NLbias((l_fit, beta, tau_1, tau_2), x0 = (x0[0], x0[1], x0[2], x0[2]), xcov = np.diag([xstd[0], xstd[1], xstd[2], xstd[2]]))

    NLprob = term1 + term2
    return NLprob


def wrapper4A(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 1 l, 2 beta and 2 tau per data set
    extra code to split up the dataset

    """
    l_fit = np.exp(par[0])
    if (l_fit == 0):
        l_fit = 10e-8
  
    beta_1 = np.exp(par[1])
    if (beta_1 == 0):
        beta_1 = 10e-8
    beta_2 = np.exp(par[2])
    if (beta_2 == 0):
        beta_2 = 10e-8
    
    tau_1 = np.exp(par[3])
    if (tau_1 == 0):
        tau_1 = 10e-8
    tau_2 = np.exp(par[4])
    if (tau_2 == 0):
        tau_2 = 10e-8
    
    (W, L, nr_trials, nr_blocks, data, initial_opened, selected_choice, reward, average_reward, out_index) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L + bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    nr_blocks_per_con = int(nr_blocks/2)
    if out_index < nr_blocks_per_con:
        initial_opened1 = initial_opened[:out_index] + initial_opened[out_index + 1: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con:]
    else: 
        initial_opened1 = initial_opened[: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con: out_index] + initial_opened[out_index + 1:]
    
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))  
    term1 = NLL(W, L, nr_trials, l_fit, beta_1, tau_1, 
                initial_opened1, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the first blocks
    
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    data_partial = data_secondhalf.query('block_nr != {}'.format(out_index))
    term1 += NLL(W, L, nr_trials, l_fit, beta_2, tau_2, 
                initial_opened2, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the second blocks

    
    term2 = NLbias((l_fit, beta_1, beta_2, tau_1, tau_2), x0 = (x0[0], x0[1], x0[1], x0[2], x0[2]), xcov = np.diag([xstd[0], xstd[1], xstd[1], xstd[2], xstd[2]]))

    NLprob = term1 + term2
    return NLprob

def wrapper4B(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 2 l, 1 beta and 2 tau per data set
    extra code to split up the dataset

    """
    l_fit_1 = np.exp(par[0])
    if (l_fit_1 == 0):
        l_fit_1 = 10e-8
    l_fit_2 = np.exp(par[1])
    if (l_fit_2 == 0):
        l_fit_2 = 10e-8

    beta = np.exp(par[2])
    if (beta == 0):
        beta = 10e-8
    
    tau_1 = np.exp(par[3])
    if (tau_1 == 0):
        tau_1 = 10e-8
    tau_2 = np.exp(par[4])
    if (tau_2 == 0):
        tau_2 = 10e-8
    
    (W, L, nr_trials, nr_blocks, data, initial_opened, selected_choice, reward, average_reward, out_index) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L + bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    nr_blocks_per_con = int(nr_blocks/2)
    if out_index < nr_blocks_per_con:
        initial_opened1 = initial_opened[:out_index] + initial_opened[out_index + 1: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con:]
    else: 
        initial_opened1 = initial_opened[: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con: out_index] + initial_opened[out_index + 1:]
    
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))  
    term1 = NLL(W, L, nr_trials, l_fit_1, beta, tau_1, 
                initial_opened1, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the first blocks
    
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    data_partial = data_secondhalf.query('block_nr != {}'.format(out_index))
    term1 += NLL(W, L, nr_trials, l_fit_2, beta, tau_2, 
                initial_opened2, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the second blocks

    term2 = NLbias((l_fit_1, l_fit_2, beta, tau_1, tau_2), x0 = (x0[0], x0[0], x0[1], x0[2], x0[2]), xcov = np.diag([xstd[0], xstd[0], xstd[1], xstd[2], xstd[2]]))

    NLprob = term1 + term2
    return NLprob

def wrapper4C(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 2 l, 2 beta and 1 tau per data set
    extra code to split up the dataset

    """
    l_fit_1 = np.exp(par[0])
    if (l_fit_1 == 0):
        l_fit_1 = 10e-8
    l_fit_2 = np.exp(par[1])
    if (l_fit_2 == 0):
        l_fit_2 = 10e-8

    beta_1 = np.exp(par[2])
    if (beta_1 == 0):
        beta_1 = 10e-8
    beta_2 = np.exp(par[3])
    if (beta_2 == 0):
        beta_2 = 10e-8
    
    tau = np.exp(par[4])
    if (tau == 0):
        tau = 10e-8

    
    (W, L, nr_trials, nr_blocks, data, initial_opened, selected_choice, reward, average_reward, out_index) = args
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L + bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    nr_blocks_per_con = int(nr_blocks/2)
    if out_index < nr_blocks_per_con:
        initial_opened1 = initial_opened[:out_index] + initial_opened[out_index + 1: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con:]
    else: 
        initial_opened1 = initial_opened[: nr_blocks_per_con]
        initial_opened2 = initial_opened[nr_blocks_per_con: out_index] + initial_opened[out_index + 1:]
    
    data_firsthalf = data.query('block_nr < {}'.format(nr_blocks_per_con))
    data_partial = data_firsthalf.query('block_nr != {}'.format(out_index))  
    term1 = NLL(W, L, nr_trials, l_fit_1, beta_1, tau, 
                initial_opened1, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the first blocks
    
    data_secondhalf = data.query('block_nr >= {}'.format(nr_blocks_per_con))
    data_partial = data_secondhalf.query('block_nr != {}'.format(out_index))
    term1 += NLL(W, L, nr_trials, l_fit_2, beta_2, tau, 
                initial_opened2, 
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward]) #for the second blocks
    
    term2 = NLbias((l_fit_1, l_fit_2, beta_1, beta_2, tau), x0 = (x0[0], x0[0], x0[1], x0[1], x0[2]), xcov = np.diag([xstd[0], xstd[0], xstd[1], xstd[1], xstd[2]]))

    NLprob = term1 + term2
    return NLprob


'''
functions calculating the probability of each choice given a parameter set
'''
def NLL(W, L, nr_trials, l_fit, beta, tau, initial_opened, selected_choice, reward, average_reward):
    """
    NLL (par, observations)
    This function calculates the negative log likelihood of a set of observations (all the choices) given a parameter set

    Parameters
    ----------
    l_fit : float
        model parameter defining the generalization strenght
    beta : float
        model parameter defining the exploration bonus
    tau : float
        model parameter defining the softmax temperature
    
    initial_opened : list of ints
        per grid, the first opened cell
    selected_choice : list of ints
        all chosen cells
    reward : list of floats
        the observed rewards per chosen cell
    average_reward : list of floats
        the average reward per choice, needed to calculate the reward of the initially opened cell

    Returns
    -------
    returns a float, this will be given to the optimizer

    """
    LL = 0
    
    for round_nr in range(0, len(initial_opened)):
        opened_cells = [initial_opened[round_nr]]
        #we need to change the format to a list of coordinates
        opened_cells2D = [[math.floor(value/W), value%W] for value in opened_cells]
        
        first_observation = 2*average_reward[round_nr*nr_trials] - reward[round_nr*nr_trials]
        observations = [first_observation]
        
        for trial_nr in range(0, nr_trials):
            choice = selected_choice[round_nr*nr_trials + trial_nr]
            LL += log_probability(W, L, choice, opened_cells2D, observations, l_fit, beta, tau)
            
            #update the observations before moving on to the next 
            opened_cells.append(choice)
            opened_cells2D = [[math.floor(value/W), value%W] for value in opened_cells]
            observations.append(reward[round_nr*nr_trials + trial_nr])
       
    return -LL


'''
functions needed to calculate the NLL
'''
#more stable if we use the log_softmax function of scipy instead of calculing P our selves and then logging it
def log_probability(W, L, choice, opened_cells2D, observations, l_fit, beta, tau):
    m, s = GP(observations, W, L, l_fit, opened_cells2D)
    UCB = [m[i] + beta * s[i] for i in range(0, W*L)]
    UCBtau = [value/tau for value in UCB]
    log_P = scipy.special.log_softmax(UCBtau)
    
    return log_P[int(choice)]
    



def GP(observations, W, L, l_fit, opened_cells2D, noise=True, epsilon = 0.0001):   
    '''
    This function starts from the observations
    since the GP assumes that not observed states have as default a value of 0,
    we rescale the reward to have a mean 0 and vary between max bounds of -0.5, 0.5
    
    Parameters
    ----------
    observations : the observations
    W : width of the bandit.
    L : lenght of the bandit.
    l_fit: the generalization strength with which the participant smooths out the observed rewards
    opened_cells2D : list of which cells are opened.
    noise: True if the participant assumes noise in their observations
    epsilon: the assumed noise, 
            measured as the variance of the reward around the mean
            std is 1 up 100, 
            var is 0.01**2 = 0.0001
             
    
    Returns
    -------
    Returns the mean function m(x) and the uncertainty function s(x) per tile.
    '''
    
    cells =[[i,j] for i in range(0, W) for j in range(0, L)]    
    
    kernel = RBF(l_fit, "fixed")
    if noise:
        kernel += WhiteKernel(epsilon, "fixed")
        
    gp = GaussianProcessRegressor(kernel=kernel)
    gp.fit(opened_cells2D, [(reward-40)/90 for reward in observations]) 
    
    mlist, sigmalist = gp.predict(cells, return_std=True)
    
    return mlist, sigmalist



def NLbias(par, x0, xcov):
    """
    function to calculate the negative log of the bias term
    this term comes from a multivariate normal distribution
    (the prior of the model parameters)

    Parameters
    ----------
    par: (l_fit, beta, tau)
    x0 : mean of the prior
    xcov : covariance matrix of the prior

    Returns
    -------
    return the NL of the bias term

    """
    NLbias = 0
    for i in range(len(par)):
        NLbias += (par[i]-x0[i])**2/xcov[i][i]
   
    return NLbias/2
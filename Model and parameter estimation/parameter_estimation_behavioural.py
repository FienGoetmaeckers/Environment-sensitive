"""
script to estimate the model parameters of one participant
"""
import sys
from csv import DictWriter
import pandas as pd
from parameter_estimation import estimate_2env, estimate_1env


#specific variables for this data file
nr_blocks = 10
nr_blocks_per_con = int(nr_blocks/2)
nr_trials = 25
nr_participants = 90
W = L = 11
#name of the file to read the data from
data_name = "data_f"
date =  "2803"
two_env = False
"""
read in the data
"""
path = "C:/Users/fgoetmae\OneDrive - UGent\Documents\Projects\Correlation\data/"
data = pd.read_csv(path + data_name + '.csv', delimiter=',')
#select one participant to estimate in this script
'''
val = sys.argv[1:]
assert len(val) == 1
p_index = int(val[0])
'''
p_index = 0
#participant = data.prolificID.unique()[p_index]
participant = data.subjectID.unique()[p_index]
#participant = data.id.unique()[p_index]
print("For participant {}".format(participant))
#data_p = data.query('prolificID == "{}"'.format(str(participant)))
#data_p = data.query('id == {}'.format(participant))
data_p = data.query('subjectID == "{}"'.format(str(participant)))

"""
estimate the model parameters of this participant
"""
if two_env:
    est = estimate_2env(W, L, nr_trials, nr_blocks, data_p)
else:
    est = estimate_1env(W, L, nr_trials, nr_blocks, data_p)

"""
save the output
"""
'''
if two_env:
    #1 output file per model
    M1  =    [est[0], est[1], est[2], est[3]]
    M2  =    [est[4], est[5]]
    M3A =   [est[6], est[7]]
    M3B =   [est[8], est[9]]
    M3C =   [est[10], est[11]]
    M4A =   [est[12], est[13]]
    M4B =   [est[14], est[15]]
    M4C =   [est[16], est[17]]


    condition = int(data_p["assigned_condition"].values[0][1:-1])
    if (condition<2): #then S-R
        resultsM1 = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M1[0][0], "l_fit_r": M1[2][0], "beta_s": M1[0][1], 
                 "beta_r": M1[2][1],"tau_s": M1[0][2], "tau_r": M1[2][2], "condition": condition, 
                 "NLL_s": M1[1], "NLL_r": M1[3], "AIC": 2*8 + 2 * (M1[1] + M1[3])/2}
    
        resultsM3A = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M3A[0][0], "l_fit_r": M3A[0][1], "beta": M3A[0][2], 
                  "tau": M3A[0][3], "condition": condition, "NLL": M3A[1], "AIC": 2*6 + 2*M3A[1]}
        resultsM3B = {"Participant": data_p["prolificID"].values[0], "l_fit": M3B[0][0], "beta_s": M3B[0][1], "beta_r": M3B[0][2], 
                  "tau": M3B[0][3], "condition": condition, "NLL": M3B[1], "AIC": 2*6 + 2*M3B[1]}
        resultsM3C = {"Participant": data_p["prolificID"].values[0], "l_fit": M3C[0][0], "beta": M3C[0][1], "tau_s": M3C[0][2], 
                  "tau_r": M3C[0][3], "condition": condition, "NLL": M3C[1], "AIC": 2*6 + 2*M3C[1]}
    
        resultsM4A = {"Participant": data_p["prolificID"].values[0], "l_fit": M4A[0][0], "beta_s": M4A[0][1], "beta_r": M4A[0][2], 
                  "tau_s": M4A[0][3], "tau_r": M4A[0][4], "condition": condition, "NLL": M4A[1], "AIC": 2*7 + 2*M4A[1]}
        resultsM4B = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M4B[0][0], "l_fit_r": M4B[0][1], "beta": M4B[0][2], 
                  "tau_s": M4B[0][3], "tau_r": M4B[0][4], "condition": condition, "NLL": M4B[1], "AIC": 2*7 + 2*M4B[1]}
        resultsM4C = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M4C[0][0], "l_fit_r": M4C[0][1], "beta_s": M4C[0][2], 
                  "beta_r": M4C[0][3], "tau": M4C[0][4], "condition": condition, "NLL": M4C[1], "AIC": 2*7 + 2*M4C[1]}

    else: #R-S
        resultsM1 = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M1[2][0], "l_fit_r": M1[0][0], "beta_s": M1[2][1], "beta_r": M1[0][1], 
	     	     "tau_s": est[2][2], "tau_r": est[0][2], "condition": condition, "NLL_s": M1[3], "NLL_r": M1[1], "AIC": 2*8 + 2 * (M1[1] + M1[3])/2}
    
        resultsM3A = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M3A[0][1], "l_fit_r": M3A[0][0], "beta": M3A[0][2], 
                  "tau": M3A[0][3], "condition": condition, "NLL": M3A[1], "AIC": 2*6 + 2*M3A[1]}
        resultsM3B = {"Participant": data_p["prolificID"].values[0], "l_fit": M3B[0][0], "beta_s": M3B[0][2], "beta_r": M3B[0][1], 
                  "tau": M3B[0][3], "condition": condition, "NLL": M3B[1], "AIC": 2*6 + 2*M3B[1]}
        resultsM3C = {"Participant": data_p["prolificID"].values[0], "l_fit": M3C[0][0], "beta": M3C[0][1], "tau_s": M3C[0][3], 
                  "tau_r": M3C[0][2], "condition": condition, "NLL": M3C[1], "AIC": 2*6 + 2*M3C[1]}
    
        resultsM4A = {"Participant": data_p["prolificID"].values[0], "l_fit": M4A[0][0], "beta_s": M4A[0][2], "beta_r": M4A[0][1], 
                  "tau_s": M4A[0][4], "tau_r": M4A[0][3], "condition": condition, "NLL": M4A[1], "AIC": 2*7 + 2*M4A[1]}
        resultsM4B = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M4B[0][1], "l_fit_r": M4B[0][0], "beta": M4B[0][2], 
                  "tau_s": M4B[0][4], "tau_r": M4B[0][3], "condition": condition, "NLL": M4B[1], "AIC": 2*7 + 2*M4B[1]}
        resultsM4C = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M4C[0][1], "l_fit_r": M4C[0][0], "beta_s": M4C[0][3], 
                  "beta_r": M4C[0][2], "tau": M4C[0][4], "condition": condition, "NLL": M4C[1], "AIC": 2*7 + 2*M4C[1]}

    resultsM2      = {"Participant": data_p["prolificID"].values[0], "l_fit": M2[0][0], "beta": M2[0][1], 
	     	     "tau": M2[0][2], "condition": condition, "NLL": M2[1], "AIC": 2*5 + 2 * M2[1]}

    #open CSV file in append mode
    with open("M1_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit_s", "l_fit_r", "beta_s", "beta_r", "tau_s", "tau_r", "condition", "NLL_s", "NLL_r", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM1)
        f_object.close()
    
    with open("M2_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit", "beta", "tau", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM2)
        f_object.close()
    
    with open("M3A_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit_s", "l_fit_r", "beta", "tau", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM3A)
        f_object.close()
    with open("M3B_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit", "beta_s", "beta_r", "tau", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM3B)
        f_object.close()
    with open("M3C_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit", "beta", "tau_s", "tau_r", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM3C)
        f_object.close()              

    with open("M4A_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit", "beta_s", "beta_r", "tau_s", "tau_r", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM4A)
        f_object.close()
    with open("M4B_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit_s", "l_fit_r", "beta", "tau_s", "tau_r", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM4B)
        f_object.close()
    with open("M4C_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit_s", "l_fit_r", "beta_s", "beta_r", "tau", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM4C)
        f_object.close()
else:
    resultsM2      = {"Participant": data_p["prolificID"].values[0], "l_fit": est[0][0], "beta": est[0][1], 
	     	     "tau": est[0][2], "condition": condition, "NLL": est[1], "AIC": 2*5 + 2 * est[1]}
    with open("1env_" + date + ".csv", 'a') as f_object:
        field_names = ["Participant", "l_fit", "beta", "tau", "condition", "NLL", "AIC"]
        dictwriter_object = DictWriter(f_object, fieldnames=field_names)
        dictwriter_object.writerow(resultsM2)
        f_object.close()


del data_p
'''
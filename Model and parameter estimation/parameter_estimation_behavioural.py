"""
script to estimate the model parameters of one participant
"""
import sys
from csv import DictWriter
import pandas as pd
from parameter_estimation import estimate_2env


#specific variables for this data file
nr_blocks = 32
nr_blocks_per_con = int(nr_blocks/2)
nr_trials = 30
W = L = 15
#name of the file to read the data from
exp = "Exp 3"
data_name = "dataF"
"""
read in the data
"""
path = "../Data/" + exp + "/"
data = pd.read_csv(path + data_name + '.csv', delimiter=',')
#select one participant to estimate in this script
'''
val = sys.argv[1:]
assert len(val) == 1
p_index = int(val[0])
'''
p_index = 0
participant = data.subjectID.unique()[p_index]
print("For participant {}".format(participant))
data_p = data.query('subjectID == "{}"'.format(str(participant)))

"""
estimate the model parameters of this participant
"""
est = estimate_2env(W, L, nr_trials, nr_blocks, data_p)

"""
save the output
"""


#1 output file per model
M1  =    [est[0], est[1], est[2], est[3]]
condition = int(data_p["assigned_condition"].values[0])

if (condition<2): #then S-R
	resultsM1 = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M1[0][0], "l_fit_r": M1[2][0], "beta_s": M1[0][1], 
			 "beta_r": M1[2][1],"tau_s": M1[0][2], "tau_r": M1[2][2], "condition": condition, 
			 "NLL_s": M1[1], "NLL_r": M1[3], "AIC": 2*8 + 2 * (M1[1] + M1[3])/2}

else: #R-S
	resultsM1 = {"Participant": data_p["prolificID"].values[0], "l_fit_s": M1[2][0], "l_fit_r": M1[0][0], "beta_s": M1[2][1], "beta_r": M1[0][1], 
			 "tau_s": est[2][2], "tau_r": est[0][2], "condition": condition, "NLL_s": M1[3], "NLL_r": M1[1], "AIC": 2*8 + 2 * (M1[1] + M1[3])/2}


#open CSV file in append mode
with open("M1_" + date + ".csv", 'a') as f_object:
	field_names = ["Participant", "l_fit_s", "l_fit_r", "beta_s", "beta_r", "tau_s", "tau_r", "condition", "NLL_s", "NLL_r", "AIC"]
	dictwriter_object = DictWriter(f_object, fieldnames=field_names)
	dictwriter_object.writerow(resultsM1)
	f_object.close()

del data_p

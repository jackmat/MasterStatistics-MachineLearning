# -*- coding: utf-8 -*-
"""
Created on Fri Nov 10 15:04:30 2017

@author: Carles
"""

import os.path
import pickle
import sys
sys.path.append("C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining")
from ProcessData import ProcessData 

save_path = 'C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/'
name_of_file = "description"
completeName = os.path.join(save_path, name_of_file+".pkl")  

name_of_file2 = "title"
completeName2 = os.path.join(save_path, name_of_file2+".pkl")

##############################
##########Reading pickles
with open(completeName, 'rb') as f: 
    CleanDescript= pickle.load(f)

with open(completeName2, 'rb') as f: 
    appnames= pickle.load(f)


###We have just two variables now: 
##  (1) CleanDescript with the Descriptions in a list
##  (2) appnames with the names  

Mydict=dict(zip(appnames, CleanDescript))


myquery= "dating"
numberRecommendations= 10

QueryProcess(myquery, numberRecommendations, Mydict)
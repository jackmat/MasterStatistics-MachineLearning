# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 10:53:26 2017

@author: Carles
"""

###########Importing libraries
import ClassObjecttrial as myscript ##myfile
import numpy

from os import listdir
from os.path import isfile, join, abspath
import sys 
sys.path.append(abspath("C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project"))


###Extra imports
import datetime
from collections import Counter
import matplotlib
import matplotlib.pyplot as plt

########################################################################
##################     Reading the CVs (.docx)
########################################################################

mypath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/CV"
##Getting all the file docs from my folder and listing them
mycvs = [f for f in listdir(mypath) if isfile(join(mypath, f))]


CVexplanation=[] ## Variable list with all the descriptions of my data


for cv in mycvs:
    totalpath = mypath +"/"+ cv
    CVexplanation.append(myscript.GettingText(totalpath))
    
cvDict=  dict(zip(mycvs, CVexplanation)) ##Variable with all key = name of the person, value = description


########################################################################
##################     Reading the e-mails
########################################################################

emailpath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/Emails"
##Getting all the file docs from my folder and listing them
myemailslist = [f for f in listdir(emailpath) if isfile(join(emailpath, f))]


########################################################################
##################     Executing algorithm
########################################################################

data = myscript.cvSearcher(myemailslist)


for email_id in myemailslist:
    totalpath = emailpath +"/"+ email_id
    data, recommendations = myscript.QueryProcess(totalpath, 3, cvDict)


########################################################################
##################     Storing variables
########################################################################
##exporting to R
        #bestRec
        #cosSim
        #myemailslist
########################################################################
    
storePath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/DataResults/"
numpymycvs = numpy.asarray(mycvs, numpy.unicode_)
EmailsList= numpy.asarray(myemailslist, numpy.unicode_)


numpy.savetxt(storePath+'cvNames.txt', numpymycvs, delimiter=',', fmt="%s")
numpy.savetxt(storePath+'EmailList.txt', EmailsList, delimiter=',', fmt="%s")
numpy.savetxt(storePath+'BestReccommendations.txt', bestRec, delimiter=',',fmt = '%i')
numpy.savetxt(storePath+'cosSim.txt', cosSim, delimiter=',', fmt = "%10.5f")



########################################################################
##################     Example of plotting
########################################################################
        #Plotting number of e-mails by time
        #Plotting number of e-mails by day of the week    
########################################################################


mydates =[(datetime.datetime(date.year,date.month,date.day)) for date in dateEmail]
labels, values = zip(*Counter(mydates).items())


dates = matplotlib.dates.date2num(labels)
plt.plot_date(dates, values)





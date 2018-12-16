# -*- coding: utf-8 -*-
"""
Created on Mon Nov  6 11:27:48 2017

@author: Carles
"""

####################PART 1
url = "https://play.google.com/store/apps/top"
shortUrl =  "https://play.google.com"

from urllib.request import urlopen
import urllib
import re , numpy

contents = urllib.request.urlopen(url).read().decode('utf8') 
mylist = numpy.unique([re.findall(pattern = "href=\"/store/apps/details.*?\"",
                                  string =contents)]) #Getting the link reference
wholeUrl = [str(shortUrl + link[6:-1] + "&hl=en")  for link in mylist]
##Doing the same for each link to get more links
contents2 = [urllib.request.urlopen(link).read().decode('utf8') for link in wholeUrl]

newlist2 =[]
for link in contents2:
    newlist2= newlist2 + re.findall(pattern = "href=\"/store/apps/details.*?\"",
                                    string =link)   #Getting the link reference

newlist2 = numpy.unique(newlist2)
wholeUrl2 = [str(shortUrl + link[6:-1] + "&hl=en")  for link in newlist2 
             if not "reviewId" in link]
wholeUrl2 = numpy.unique(wholeUrl2)

# Putting together the url + link reference
appname =[]
descriptionRes=[]
for link in wholeUrl2[0:1500]:
    with urlopen(link) as url:
        descriptionRes = descriptionRes + re.findall(pattern = "itemprop=\"description.*?\">.*?<div jsname=\".*?\">.*?</div>", string = url.read().decode('utf8'))
    with urlopen(link) as url:    
        appname= appname + re.findall("<title id=\"main-title\">.*? - Android", string = url.read().decode('utf8'))

cleanDes= [link[58:-6] for link in descriptionRes]
appname = [link[23:-10] for link in appname]

####################PART 2
###preprocess
""" Remove non alpha-numeric characters 
 Tokenize 
 Lowercase words 
 Remove stopwords 
 Stem 
 Check language Extract terms 
 Compute tf and idf (numpy)"""

#Remove non alpha numeric charachters, lowercase and tokenize
from nltk.corpus import stopwords, words
from nltk.tokenize import wordpunct_tokenize

clean = [re.sub(r'[^a-zA-Z0-9\[\]]',' ' ,link).lower() for link in cleanDes] #Clean and tokenize 

##############################
####Saving it in a file text

import os.path
import pickle

save_path = 'C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/'
name_of_file = "description"
completeName = os.path.join(save_path, name_of_file+".pkl")  

name_of_file2 = "title"
completeName2 = os.path.join(save_path, name_of_file2+".pkl")  

with open(str(completeName), 'wb') as f:
    pickle.dump(clean, f)

with open(str(completeName2), 'wb') as f:
    pickle.dump(appname, f)


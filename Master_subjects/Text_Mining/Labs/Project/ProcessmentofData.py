# -*- coding: utf-8 -*-
"""
Created on Thu Nov  9 18:14:56 2017

@author: Carles
"""
import numpy

##Geting content from the e-mail
class cvSearcher(object):
    def __init__(self, Listofemails):
            #Creating a list of ids, emails sent by, habilities and date
        self.count = 0    
        self.id_unic=[]
        self.text = []
        self.dateEmail = []
        self.Emailsend=[]
        self.Emailpaths = []
        self.bestRec = numpy.zeros(shape=(len(Listofemails),3), dtype=numpy.int)
        self.cosSim =  numpy.zeros(shape=(len(Listofemails),3))
        
    
def getText(filename):
    import docx 
    doc = docx.Document(filename)
    fullText = []
    for para in doc.paragraphs:
        fullText.append(para.text)
    return '\n'.join(fullText)

## Reinitializing variables   

###Getting date in proper format
def datetreat(date):
    import email.utils
    import time
    import datetime
    timetransf = email.utils.parsedate(date)    
    timetransf = time.mktime(timetransf)    
    timetransf = datetime.datetime.fromtimestamp(timetransf)
    return(timetransf)
###Part Process data
from nltk.stem import SnowballStemmer
from nltk.corpus import stopwords, words
from nltk.tokenize import TweetTokenizer
import re

def remove_nonalphanum(words):
    return [re.sub(r'[^a-zA-Z0-9\[\]]',' ' , word) for word in words]

def tokenize(words):
    tknzr = TweetTokenizer()
    return [tknzr.tokenize(word) for word in words]

def lowercase(words):
    return [[word.lower() for word in lines] for lines in words]

def stemming(words):
    st = SnowballStemmer("swedish")
    return [[st.stem(word) for word in lines] for lines in words]

def stoppingwords(wordes):
    stop= set(stopwords.words('swedish'))
    stop.update(['.', ',', '"', "'", '?', '!', ':', ';', '(', ')', '[', ']', '{', '}', 'br']) # remove it if you need punctuation 
    return [[word for word in lines if word not in stop] for lines in wordes]

def checkLanguage(data):
    checkWords = set(words.words())
    return [[word for word in lines if word in checkWords] for lines in data]
    
def joiner(words):
    return [" ".join(word) for word in words] 

def processdata(data):
    #functionList = [remove_nonalphanum, tokenize, lowercase, stoppingwords, checkLanguage ,stemming , joiner]
    
    functionList = [tokenize, lowercase, stoppingwords ,stemming , joiner]
    for function in functionList:
        data = function(data)
    return data




########################################################################3
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

##variable for CV = cvDict

###############################################################   
from email import policy
from email.parser import BytesParser

def QueryProcess(data, Emailpath, NtopResults, myCVdict):
    
    Description = list(myCVdict.values())
    IndivNames = list(myCVdict.keys())
       
    k = NtopResults # Number of results to show
    #####################   Matrix numerical transformation 
    Result = processdata(Description) ##Processing data calling function from part2
    transvector = TfidfVectorizer()
    tf_matrix = transvector.fit_transform(Result)
    
    #Query numerical transformation
    with open(Emailpath , 'rb') as fp:
        msg = BytesParser(policy=policy.default).parse(fp)   
        date =msg["Date"]
        EmailDescr = msg.get_body(preferencelist=('plain')).get_content()

        if Emailpath in data.Emailpaths:
            
            print("It has already been evaluated")
            k_rec = data.bestRec[data.Emailpaths.index(Emailpath),:]
            k_recomendations = [IndivNames[i] for i in k_rec ]
            return data, k_recomendations 
        else: 
            
            data.id_unic.append(data.count)
            data.Emailsend.append(msg['From'])
            data.dateEmail.append(datetreat(date)) ## Passing the function datetreat to get the data in a proper string  
            data.text.append(EmailDescr)
            data.Emailpaths.append(Emailpath)
            
            Email = processdata([EmailDescr])###Processing query calling function from part2
            Email = transvector.transform(Email)
            
            ###Similarity comparison between matrix and query
            search_result = cosine_similarity(tf_matrix, Email)
            SearchNiceArray= search_result.reshape(1,search_result .size)[0]
            kTopIndex = SearchNiceArray.argsort(axis = 0 )[::-1][:k].flatten()
            data.bestRec[data.count,:]= kTopIndex
            k_recomendations = [IndivNames[i] for i in kTopIndex ]
            data.cosSim[data.count,:]= ([SearchNiceArray[i] for i in kTopIndex])
            data.count = data.count+1 ### upgrading the counter
            print([SearchNiceArray[i] for i in kTopIndex])
            return data, k_recomendations 






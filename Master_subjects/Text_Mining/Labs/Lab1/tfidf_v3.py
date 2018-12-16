# -*- coding: utf-8 -*-
"""
Created on Wed Jan 17 14:37:50 2018

@author: Carles
"""

import numpy as np

def tf(documents):
    WordList =[]
    
    ##Creating the list of words of all documents being WordList
    for document in documents:
        splitWords = document.split(" ")
        for Word in splitWords:
            if Word not in WordList:
                WordList.append(Word)
    
    ##Matrix with colnames(Documents), rownames = Words            
    tfperdoc = np.zeros([len(WordList), len(documents)])
    for i in range(len(documents)): #### CHange from here
        splitWords = documents[i].split(" ")
        for Word in splitWords:
            tfperdoc[WordList.index(Word), i] +=1
    ##Dividing by the Columnsum and then divide by the larger term in the column
    for column in range(len(documents)):
        tfperdoc [:,column] = tfperdoc [:,column]/np.sum(tfperdoc [:,column])
        tfperdoc [:,column] = tfperdoc [:,column]/np.amax(tfperdoc [:,column])

    return [tfperdoc, WordList]




def idf(tfres):
    #Number of documents in U that contain t
    nwords = tfres.shape[0]
    ndocs = tfres.shape[1]
    wordDoc = np.zeros([nwords, ndocs])
    WordCount = np.zeros([nwords])
    for i in range(nwords):
        for j in range(ndocs):
            if(tfres[i,j]==0):
                wordDoc[i,j]=0
            else:    
                wordDoc[i,j]=1
    for row in range(nwords):
        WordCount[row] = np.sum(wordDoc[row,:])

    for number in range(nwords):
        if WordCount[number] == 0:
            WordCount[number] = 0
        else:
            WordCount[number] = ndocs/WordCount[number]
            WordCount[number] = np.log(WordCount[number])
    return WordCount


def tfidf(tf, idf):
    ndocs = tf.shape[1]
    for column in range(ndocs):
        tf[:,column] = tf[:,column]*idf
        
    return(tf)



def tfidfquery(query, Matwordlist, tf):
    
    queryWordList =[]
    queryWordCount =[]
    ##Creating the list of words with the query being WordList
    
    splitWords = query.split(" ")
    for Word in splitWords:
        if Word not in queryWordList:
            queryWordList.append(Word)
            queryWordCount.append(1)
        else: 
            queryWordCount[queryWordList.index(Word)] +=1
    
    queryWordList =np.asarray(queryWordList)
    queryWordCount =np.asarray(queryWordCount)
    
    ## Matrix of length of vectorwords
    Matrix_tf = np.zeros([len(queryWordList),tf.shape[1]])
    print(Matrix_tf)
    for i in range(len(queryWordList)):
        if queryWordList[i] in Matwordlist:
            Matrix_tf[i,] = tf[Matwordlist.index(queryWordList[i]),] 
    ##Dividing by the Columnsum and then divide by the larger term in the column
    if np.array_equal(Matrix_tf ,np.zeros([len(queryWordList),tf.shape[1]])):
        return "No match found"
    else:
        queryWordCount  = queryWordCount/np.sum(queryWordCount)
        queryWordCount = queryWordCount/np.amax(queryWordCount)  #query word = tf now
    ##Total tf
    Alltf = np.c_[Matrix_tf, queryWordCount] # insert values before column 3
    print(Alltf)
    ###Doing idf    
    Allidf= idf(Alltf)
    ##tfidif =(0.5+0.5tf)*idf
    tfidfres = 0.5*Matrix_tf + 0.5*tfidf(Matrix_tf,Allidf)
    return [tfidfres, queryWordCount.reshape(-1,1)]


documents = ["Tomorrow will rain", "today will rain", "it is not today that we will do this but tomorrow"]
tf1=tf(documents)
idf1= idf(tf1[0])
tfidfRes=tfidf(tf1[0], idf1)


tfidfquery=tfidfquery("Hello will will rain dear", tf1[1], tf1[0])    
from sklearn.metrics.pairwise import cosine_similarity
cosine_similarity(tfidfquery[0].transpose(),tfidfquery[1].transpose()).transpose()

tf2=tf("Hello will will rain dear")

a= [1,2,3]
np.asarray(a)

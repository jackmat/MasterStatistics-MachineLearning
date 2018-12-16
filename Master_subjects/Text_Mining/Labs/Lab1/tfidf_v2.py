# -*- coding: utf-8 -*-
"""
Created on Mon Jan 15 18:44:33 2018

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
    print(WordCount)
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



def tfidfquery(query, wordlist, tf):
    queryWord =np.zeros([len(wordlist)])
    QuerySplit= query.split(" ")
    
    for Word in QuerySplit:
        if Word in wordlist:
            queryWord[wordlist.index(Word)] +=1
    ##Dividing by the Columnsum and then divide by the larger term in the column
    if np.array_equal(queryWord ,np.zeros([len(query)])):
        return "No match found"
    else:
        queryWord  = queryWord/np.sum(queryWord)
        queryWord = queryWord/np.amax(queryWord)  #query word = tf now
    ##Total tf
    Alltf = np.c_[tf, queryWord] # insert values before column 3
    ###Doing idf    
    Allidf= idf(Alltf)
    ##tfidif =(0.5+0.5tf)*idf
    tfidf = (0.5+0.5*queryWord)*Allidf #####IF CHANGED TO queryWord*Allidf it works good
    return tfidf.reshape(-1,1)


documents = ["Tomorrow will rain", "today will rain", "it is not today that we will do this but tomorrow"]
tf1=tf(documents)
idf1= idf(tf1[0])
tfidfRes=tfidf(tf1[0], idf1)


tfidfquery=tfidfquery("Hello will will rain dear", tf1[1], tf1[0])    
from sklearn.metrics.pairwise import cosine_similarity
cosine_similarity( tfidfquery.transpose(),tfidfRes.transpose())

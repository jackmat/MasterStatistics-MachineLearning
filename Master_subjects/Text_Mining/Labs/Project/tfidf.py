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
    ###Giving more importance to words
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
    tfidf = queryWord*Allidf #####IF CHANGED TO queryWord*Allidf it works good
    return tfidf.reshape(-1,1)

def importantwords(totaltf, wordlist):
    ImpWords = set(["deploy", "release","projektledare", "förbättringsarbete", "cognos", "wcf", "integration", "lösning"]) ##Defining Words
    CommonWords = list(ImpWords & set(text.split(" "))) ##Important Words in Common
    

documents = ["Tomorrow will rain", "today will rain", "it is not today that we will do this but tomorrow"]
tf1=tf(documents)
idf1= idf(tf1[0])
tfidfRes=tfidf(tf1[0], idf1)


tfidfquery=tfidfquery("Today is the day", tf1[1], tf1[0])    
from sklearn.metrics.pairwise import cosine_similarity
cosine_similarity( tfidfquery.transpose(),tfidfRes.transpose())

WORDLIST=tf1[1]


ImpWords = set(["will","deploy", "release","projektledare", "förbättringsarbete", "cognos", "wcf", "integration", "lösning"]) ##Defining Words

def WeightWordChan(ImpWords, listOfWords, alltf, parameter):
    ###Args:
    #ImpWords:   List of Words that you want to change the weight in case they appear on the intersection with listOfWords
    #listOfWords:List of Words produce by the tf function, being result tf[1]
    #alltf: The matrix created in querytfidf together with the previous matrix (also called Alltf)
    #parameter : Set this to how much do you want to affect the effect of that word to the final outcome
    #            Recommended to be set between 0.2 and 1 (it will sum that much to the place where this word appears on the Alltf matrix)
    
    ###Return:
    # alltf with matrix modified by manual weights
    
    CommonWords = list(set(ImpWords) & set(listOfWords))
    for word in CommonWords:
        #Check the index on wordlist and pass it to Alltf being Alltf + parameters = 0.5 for example
        for column in alltf.shape[1]:
            if alltf[listOfWords.index(word), column]>0:
                alltf[listOfWords.index(word), column] +=parameter
                
    return alltf            
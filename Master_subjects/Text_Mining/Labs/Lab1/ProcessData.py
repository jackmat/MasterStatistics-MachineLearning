# -*- coding: utf-8 -*-
"""
Created on Thu Nov  9 18:14:56 2017

@author: Carles
"""

###Part 2
from nltk.stem.lancaster import LancasterStemmer
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
    st = LancasterStemmer()
    return [[st.stem(word) for word in lines] for lines in words]

def stoppingwords(wordes):
    stop= set(stopwords.words('english'))
    stop.update(['.', ',', '"', "'", '?', '!', ':', ';', '(', ')', '[', ']', '{', '}', 'br']) # remove it if you need punctuation 
    return [[word for word in lines if word not in stop] for lines in wordes]

def checkLanguage(data):
    checkWords = set(words.words())
    return [[word for word in lines if word in checkWords] for lines in data]
    
def joiner(words):
    return [" ".join(word) for word in words] 

def processdata(data):
    functionList = [remove_nonalphanum, tokenize, lowercase, stoppingwords, checkLanguage ,stemming , joiner]
    for function in functionList:
        data = function(data)
    return data




########################################################################3
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

def QueryProcess(query, NtopResults, dictionary):
    Description = list(dictionary.values())
    appnames = list(dictionary.keys())
    
    k = NtopResults # Number of results to show
    #####################   Matrix numerical transformation 
    Result = processdata(Description) ##Processing data calling function from part2
    transvector = TfidfVectorizer()
    tf_matrix = transvector.fit_transform(Result)
    
    #Query numerical transformation
    query = processdata([query])###Processing query calling function from part2
    query = transvector.transform(query)
    
    ###Similarity comparison between matrix and query
    search_result = cosine_similarity(tf_matrix, query) 
    SearchNiceArray= search_result.reshape(1,search_result .size)[0]
    kTopIndex = SearchNiceArray.argsort(axis = 0 )[::-1][:k].flatten()
    k_recomendations = [appnames[i] for i in kTopIndex ]
    return k_recomendations 




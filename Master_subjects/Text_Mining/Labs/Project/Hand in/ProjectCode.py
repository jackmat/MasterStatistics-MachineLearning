# -*- coding: utf-8 -*-
"""
Created on Mon Jan 15 12:33:46 2018

@author: Carles
"""

import numpy as np
from docx import Document

from nltk.stem import SnowballStemmer
from nltk.corpus import stopwords, words
from nltk.tokenize import TweetTokenizer
import re

from sklearn.metrics.pairwise import cosine_similarity

from email import policy
from email.parser import BytesParser
import datetime

######################################################
#############   Functions
#################################################

class cvSearcher(object):
    def __init__(self, Listofemails):
            #Creating a list of ids, emails sent by, habilities and date
        self.count = 0    
        self.id_unic=[]
        self.text = []
        self.dateEmail = []
        self.Emailsend=[]
        self.Emailpaths = []
        self.bestRec = np.zeros(shape=(len(Listofemails),3), dtype=np.int) #numpy.int, numpy.zeros
        self.cosSim =  np.zeros(shape=(len(Listofemails),3))
        self.ImpWords =[]
        self.parameter = 0
        self.manip = False
        self.keyWord = ""
        self.listkeywords =[]
def GetText(filename):
    doc = Document(filename)
    fullText = []
    for para in doc.paragraphs:
        fullText.append(para.text)
    return '\n'.join(fullText)


###Getting date in proper format
def datetreat(date):
    import email.utils
    import time
    timetransf = email.utils.parsedate(date)    
    timetransf = time.mktime(timetransf)    
    timetransf = datetime.datetime.fromtimestamp(timetransf)
    return(timetransf)


###Part Process data

def remove_nonalphanum(words):
    
    words = [re.sub(r'[^a-zA-Z\[\]]',' ' , word) for word in words]
    
    return words

    
def tokenize(words):
    tknzr = TweetTokenizer()
    return [tknzr.tokenize(word) for word in words]

def lowercase(words):
    return [[word.lower() for word in lines] for lines in words]





######################################TO DOOOO
def WordCorrections(words):
# =============================================================================
    ##Important word change manually
    words = [lines.replace("  ", " ") for lines in words]
    words = [lines.replace("  ", " ") for lines in words]
    words = [lines.replace(" erf ", " seni ") for lines in words] ## it is means erfaren (senior)
    #words = [lines.replace(" seni ", " ") for lines in words] ## it is means erfaren (senior)
    words = [lines.replace(" juni ", " ") for lines in words] ## it is means erfaren (junior)
    words = [lines.replace(" cognos ", " cogno ") for lines in words] 
    words = [lines.replace(" java ", " jav ") for lines in words] 
    words = [lines.replace(" jira ", " jir ") for lines in words] 
    words = [lines.replace(" kravspecialist ", " kravanalytik ") for lines in words] 

#   words = [lines.replace(" led ", " projektled ") for lines in words] 
#    words = [lines.replace(" processled ", " projektled ") for lines in words] 
#    words = [lines.replace(" projektadministratör ", " projektled ") for lines in words] 
    words = [lines.replace(" release ", " releas ") for lines in words] 


    words = [lines.replace(" pl ", " projektled ") for lines in words]
    words = [lines.replace( "data warehouse", "dw") for lines in words]
    words = [lines.replace( "teknisk projektled", "tpl") for lines in words]
    words = [lines.replace( "technical project manag", "tpl") for lines in words]
#   words = [lines.replace( "lösningsarkitek", "systemutveckl") for lines in words]
    words = [lines.replace( "project manager", "projektled") for lines in words]
    words = [lines.replace( "project manag", "projektled") for lines in words]
    
    words = [lines.replace( "architect", "arkitek") for lines in words]
    words = [lines.replace( "systemarkitek", "arkitek") for lines in words]
    words = [lines.replace( "arkitekur product plan", "tpl") for lines in words]
    words = [lines.replace( "plan technical arkitek", "tpl") for lines in words]
    words = [lines.replace( "arkitekur product plan", "tpl") for lines in words]
    words = [lines.replace( "systemansvar", "projektled") for lines in words]
#    words = [lines.replace( "system responsibl", "projektled") for lines in words]
#    words = [lines.replace( "projektledning", "projektled") for lines in words]

    return words

########################################TO DOO
def stemming(words):
    st = SnowballStemmer("swedish")
    return [[st.stem(word) for word in lines] for lines in words]

def stoppingwords(wordes):
    stop= set(stopwords.words('swedish'))
    stop.update(['.', ',', '"', "'", '?', '!', ':', ';', '(', ')', '[', ']', '{', '}', 'br', "st"]) # remove it if you need punctuation 
    #stop2= set(stopwords.words('english'))
    wordes = [[word for word in lines if word not in stop] for lines in wordes]
    #wordes = [[word for word in lines if word not in stop2] for lines in wordes]
    return wordes
def checkLanguage(data):
    checkWords = set(words.words())
    return [[word for word in lines if word in checkWords] for lines in data]
    
def joiner(words):
    return [" ".join(word) for word in words] 

def cleaning(words):
    words = [re.sub(r'<.+?>', '', word) for word in words]
    words = [re.sub(r'\b\w{1}\b', '', word) for word in words] ## remove letters alone
    words = [lines.replace("\r","") for lines in words]
    words = [lines.replace("\n","") for lines in words]

    return words

def processdata(functions):
    #functionList = [remove_nonalphanum, tokenize, lowercase, stoppingwords, WordCorrections,checkLanguage ,stemming , joiner]
    
    functionList = [tokenize, lowercase, stoppingwords, stemming , joiner, cleaning, WordCorrections]
    for function in functionList:
        functions = function(functions)
    return functions
#####################################################################
########################################################################3
## Tfidf implementation of the Searcher
###############################################################   

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

def WeightWordChan(data, listOfWords, alltf):
# =============================================================================
#     ### Args:
#     # data.parameter : Set this to how much do you want to affect the effect of that word to the final outcome
#     # data.ImpWords:   List of Words that you want to change the weight in case they appear on the intersection with listOfWords
#     # listOfWords:List of Words produce by the tf function, being result tf[1]
#     # alltf:      A matrix or vector created in numpy format (also called Alltf)
#     #             Recommended to be set between 0.2 and 1 (it will sum that much to the place where this word appears on the Alltf matrix)
#     
#     ###Return:
#     # alltf with matrix modified by manual weights for a matrix
# =============================================================================
    CommonWords = list(set(data.ImpWords) & set(listOfWords))
#    print(CommonWords)
    if len(alltf.shape)== 2:
        if data.keyWord in CommonWords:
#            print(data.keyWord+ "word in the email")
            if alltf[data.keyWord.index(data.keyWord)]>0:
                alltf[listOfWords.index(data.keyWord)] += data.parameter
        for word in CommonWords:
            #Check the index on wordlist and pass it to Alltf being Alltf + parameters = 0.5 for example
            for column in range(alltf.shape[1]):
                if alltf[listOfWords.index(word), column]>0:
                    alltf[listOfWords.index(word), column] += data.parameter
         
        return alltf
    else:
        
        if data.keyWord in CommonWords:
#            print(data.keyWord+ "word in the email")
            if alltf[data.keyWord.index(data.keyWord)]>0:
                alltf[listOfWords.index(data.keyWord)] += 2*data.parameter
        for word in CommonWords:
            if alltf[listOfWords.index(word)]>0:
                alltf[listOfWords.index(word)] += data.parameter
        return alltf


def tfidfquery(data, query, wordlist, mytf):
# =============================================================================
#     ### Args:
#     # query:    Message to be taken into account. Must be a string
#     # wordlist: List of Words produce by the tf function, being result tf[1]
#     # tf:       The matrix created in by tf being result tf[0]
#     # data.manip: if True: the following variables will be passed exclusively to the function WeightWordChan()
#     #     data.ImpWords:   List of Words that you want to change the weight in case they appear on the intersection with listOfWords
#     #     data.parameter : Set this to how much do you want to affect the effect of that word to the final outcome
# 
#     ###Return:
#     # alltf with matrix modified by manual weights
# =============================================================================
    
    queryWord =np.zeros([len(wordlist)])
    QuerySplit= query[0].split(" ")
    
    for Word in QuerySplit:
        if Word in wordlist:
            queryWord[wordlist.index(Word)] +=1
    ##Dividing by the Columnsum and then divide by the larger term in the column
    if np.array_equal(queryWord ,np.zeros([len(query)])):
        return "No match found"
    if  data.manip == True:
        queryWord = WeightWordChan(data, listOfWords= wordlist, alltf= queryWord)
        queryWord  = queryWord/np.sum(queryWord)

    else:
        queryWord  = queryWord/np.sum(queryWord)
        if  data.manip == True:
                queryWord = WeightWordChan(data, listOfWords= wordlist, alltf= queryWord)

        queryWord = queryWord/np.amax(queryWord)  #query word = tf now  
        ##Total tf
    Alltf = np.c_[mytf, queryWord] # insert values before column 3
    
    ###Doing idf    
    Allidf= idf(Alltf)
    ##tfidif =(0.5+0.5tf)*idf
    tfidfRes = queryWord*Allidf #####IF CHANGED TO queryWord*Allidf it works good
    return tfidfRes.reshape(-1,1)


########################################################################3
##End of Search algorithm
###############################################################   


########################################################################3
## Main Query
###############################################################   


def QueryProcess(data, Emailpath, NtopResults, myCVdict):
    
    Description = list(myCVdict.values())
    IndivNames = list(myCVdict.keys())
       
    k = NtopResults # Number of results to show
    #####################   Matrix numerical transformation 
    Result = processdata(Description) ##Processing data calling function from part2
    tf1=tf(Result)
    idf1= idf(tf1[0]) ###tf1[0] outputs the tf, tf1[1] outputs the wordList
    tfidfMatrix=tfidf(tf1[0], idf1) ###
    
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
            key = re.search("(?<=sök )\w+", Email[0])
            
            if(type(key) != type(None)):
                data.keyWord = key.group()
                data.listkeywords.append(key.group())

            tfidfquest=tfidfquery(data,Email, tf1[1], tf1[0]) ##Passing own searcher functions 
    
            ###Similarity comparison between matrix and query
            search_result = cosine_similarity( tfidfquest.transpose(),tfidfMatrix.transpose())            
            SearchNiceArray= search_result.reshape(1,search_result .size)[0]
            kTopIndex = SearchNiceArray.argsort(axis = 0 )[::-1][:k].flatten()
            data.bestRec[data.count,:]= kTopIndex
            k_recomendations = [IndivNames[i] for i in kTopIndex ]
            data.cosSim[data.count,:]= ([SearchNiceArray[i] for i in kTopIndex])
            data.count = data.count+1 ### upgrading the counter
            #print([SearchNiceArray[i] for i in kTopIndex])
            return data, k_recomendations



#############################################################################################
############### Running code
###########################################################################################
       ###########Importing libraries

from os import listdir
from os.path import isfile, join, abspath
import sys 
sys.path.append(abspath("C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/Hand in"))


########################################################################
##################     Reading the CVs (.docx)
########################################################################

mypath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/CV"
##Getting all the file docs from my folder and listing them
mycvs = [f for f in listdir(mypath) if isfile(join(mypath, f))]


CVexplanation=[] ## Variable list with all the descriptions of my data


for cv in mycvs:
    totalpath = mypath +"/"+ cv
    CVexplanation.append(GetText(totalpath))
    
cvDict=  dict(zip(mycvs, CVexplanation)) ##Variable with all key = name of the person, value = description

     
########################################################################
##################     Reading the e-mails
########################################################################


emailpath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/Emails"
##Getting all the file docs from my folder and listing them
########################################################################
##################     Reading excel gold standard(.docx)
########################################################################

GoldFile = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/Hand in/20180314FinalEmailEvaluation.xlsx"
import pandas as pd

xl = pd.read_excel(GoldFile, sheetname ="FinalRec")
GoldFramePandas =xl.iloc[:,1:]
GoldEmailName =xl.iloc[:,0]

GoldEmailName = GoldEmailName.values
GoldEmailName[0] =GoldEmailName[0]+".eml"
myemailslist = GoldEmailName

myemailslist == GoldEmailName
#Convert to numpy
GoldNp = GoldFramePandas.values
GoldNp  = np.where(np.isnan(GoldNp), -1, GoldNp)
GoldNp = np.asarray(GoldNp, dtype = int)



########################################################################
##################     Reading excel gold standard(.docx)
########################################################################

matches =np.zeros(GoldNp.shape[0], dtype = int) ## Total number of possible matches per query
for i in range(GoldNp.shape[0]):
    for j in range(GoldNp.shape[1]):
        if GoldNp[i,j] >= 0:
            matches[i]+=1

def intersection(pred, Gold):
    nrows = pred.shape[0]
    intersectionRes =np.zeros([nrows], dtype = int)
    for i in range(nrows):
        intersectionRes[i] =len(set(pred[i,]) & set(Gold[i,]))
    return intersectionRes



def optRange(numParam):
    # =============================================================================
#     ### Args:
#     # sequence: 
#     # 
#     ###Return:
#     # PercWellClass: percentage between matches/totalgoodpredictions
# =============================================================================
    import time
    start_time = time.time()
    
     
    
    data2 = cvSearcher(myemailslist)
    data2.ImpWords = set(['analytic', 'apex','arkitek', 'configuration', 'datamodellering', 
                          'dax', 'django', 'excel',  'express',  'förvaltningsled',
                          'integration', 'jir', 'lösning', 'management', 'microsoft', 
                          'mongodb', 'mysql', 'net',  'oracl', 'pow', 
                          'prediktiv', 'projektled', 'python', 'qlikview', 'spss', 'sql',
                          'ssas', 'ssis', 'tableau', 'vba',
                          'visual', 'warehous', 'wcf', 'weblogic', "dw", "nosql", "nodej", 
                          "bi", "postgresql", 'almexpert', 'alm', 'cam', 'frontend', 
                          'it-arbetsplat', 'marknadsanalytik', 'sas', 'sap', 'testled',"arkitektur",
                          "pss", "tpl", 'cogno',"jav", 'javintegration', 'informationsarkitek', 
                          'kravanalytik','releas',"systemutveckl","seni"]) ##Defining Words
    data2.manip = True

    data2.parameter = numParam

    for email_id in myemailslist:
        totalpath = emailpath +"/"+ email_id
        QueryProcess(data2, totalpath, 3, cvDict)
    
    PredIntersect =  intersection(data2.bestRec,GoldNp)  

                    
    PercWellClass =np.sum(PredIntersect)/np.sum(matches)## Percentage of good
    print ("My program took "+ str(time.time() - start_time) + " seconds to run")

    return PercWellClass  , data2.bestRec, data2.cosSim, data2.listkeywords

paramRange= np.arange(0, 10, 1)




yRes =[]
for i in range(len(paramRange)):
    yRes.append(optRange(paramRange[i])[0])
yRes 
    
# =============================================================================
# result=optRange(0)
# result[0]
# result[1]
# result[2]
# result[3]
# 
# mylist = set(result[3])
# 
# 
# =============================================================================

########################################################################
##################     Storing variables
########################################################################
##exporting to R
        #bestRec
        #cosSim
        #myemailslist
########################################################################

# =============================================================================
# ImpWords = ["deploy", "release","projektledare", "förbättringsarbete", "cognos", "wcf", 
#             "integration", "lösning", "django","mysql", "microsoft", "oracle","power", "bi","spss", "tableau",
#             "net", "apex", "c#","configuration", "management",  "warehouse", "datamodellering",
#             "dax", "excel", "förvaltningsledare","jira", "mongodb", "mysql", "nodejs", "nosql",
#             "express", "pentaho", "pl/sql", "postgresql", "power", "prediktiv", "python", "qlikview",
#             "sql", "ssas","ssis","vba", "visual", "analytics", "weblogic", 'pow', 'förbättringsarbet',
#             'jir', 'oracl', 'releas',  'analytic',  'cogno',  'förvaltningsled',  'warehous', "data warehouse",
#             "plsql", "dw", "nodej", 'projektled', 'dat', 'rele', ]
# 
# MatchedWOrds = ['ssis', 'wcf', 'förvaltningsled', 'dax', 'prediktiv', 'visual', 'integration', 'warehous', 'oracl', 'projektledare', 
#                 'pow', 'weblogic', 'qlikview', 'django', 'warehouse', 'microsoft', 'analytic', 'vba', 'management', 'spss', 'configuration', 'jir', 'mysql', 'ssas', 'bi', 'tableau', 'lösning', 'excel', 'python', 'apex', 'net', 'mongodb', 'express', 'datamodellering', 'sql']
# 
# =============================================================================
#functionList = [tokenize, lowercase, stoppingwords, stemming , joiner, WordCorrections]
# =============================================================================
# TotalAMount = set(["deploy", "release","projektledare", "förbättringsarbete", "cognos", 
#                           "wcf", "integration", "lösning", "django","mysql", "microsoft", "oracle",
#                           "power", "bi","spss", "tableau", "net", "apex", "c","configuration", 
#                           "management",  "warehouse", "datamodellering","dax", "excel", 
#                           "förvaltningsledare","jira", "mongodb", "mysql", "nodejs", "nosql",
#             "express", "pentaho", "pl/sql", "postgresql", "power", "prediktiv", "python", "qlikview",
#             "sql", "ssas","ssis","vba", "visual", "analytics", "weblogic", 'pow', 'förbättringsarbet',
#             'jir', 'oracl', 'releas',  'analytic',  'cogno',  'förvaltningsled',  'warehous', 
#             "data warehouse", "plsql", "dw", "nodej", 'projektled', 'rele']) ##Defining Words
# 
# =============================================================================



storePath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/DataResults/"
paramOpt = np.asarray(yRes, np.unicode_)
paramRange = np.asarray(paramRange, np.unicode_)

np.savetxt(storePath+'ParamOpt.txt', paramOpt, delimiter=',', fmt="%s")
np.savetxt(storePath+'ParamRange.txt', paramRange, delimiter=',', fmt="%s")
# np.savetxt(storePath+'Cossim.txt', np.around(result[2],3), delimiter=';', fmt="%s")
# np.savetxt(storePath+'BestReccommendations.txt', result[1], delimiter=';', fmt="%s")




# =============================================================================

# #numpymycvs = np.asarray(mycvs, np.unicode_)
# EmailsList= np.asarray(myemailslist, np.unicode_)
# 
# #np.savetxt(storePath+'cvNames.txt', numpymycvs, delimiter=',', fmt="%s")
# np.savetxt(storePath+'EmailList.txt', EmailsList, delimiter=',', fmt="%s")
# np.savetxt(storePath+'BestReccommendations.txt', data2.bestRec, delimiter=',',fmt = '%i')
# np.savetxt(storePath+'cosSim.txt', data2.cosSim, delimiter=',', fmt = "%10.5f") 
# 
# =============================================================================

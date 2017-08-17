# -*- coding: utf-8 -*-
"""
Created on Thu Oct  6 10:44:25 2016

@author: fanti
"""

from __future__ import print_function
import sklearn
from sklearn.decomposition import TruncatedSVD, NMF, LatentDirichletAllocation
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.preprocessing import Normalizer
from sklearn import metrics
from sklearn.cluster import KMeans, MiniBatchKMeans

import matplotlib.pyplot as plt

import pandas
import numpy
import pyodbc

import csv
import sys, getopt
import warnings

import json

def print_top_words(model, feature_names, n_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print("Topic #%d:" % topic_idx)
        print(" ".join([feature_names[i]
            for i in topic.argsort()[:-n_top_words - 1:-1]]))
    print()

def extract_top_words(model, feature_names, n_top_words):
    res = [[feature_names[i] for i in topic.argsort()[:-n_top_words - 1:-1]] for topic_idx, topic in enumerate(model.components_)]
    return(res)

def main(argv):
    with open('../../db_credential.json') as data_file:    
        cred = json.load(data_file)

    dbServer = 'C9W27725.itcs.hpecorp.net'
    dbName = 'cpx'
    dbUser = cred['username']
    dbPwd = cred['password']
    dbColumns = '([Sales Order ID] + \'_\'+[HLI_ID]) as [SO_HLI], [Supplier Complexity Group], [ConfigText]'
    #dbColumns = '([Sales Order ID] + \'_\'+[HLI_ID]) as [SO_HLI], [Supplier Complexity Group] + \'_\' + [Plant Code], [ConfigText]'
    #dbColumns = '([Sales Order ID] + \'_\'+[HLI_ID]) as [SO_HLI], [Global Business Unit Name], [ConfigText]'
    dbTable = 'Region_HLI_CLEAN'
    #dbTable = 'QADTA_Intersect_HLI_NEW_v'
    #dbCondition = '[Profit Center L0 Name] = \'EMEA\' AND [ASAP Flag] = \'Y\''
    dbCondition = '[Profit Center L0 Name] = \'EMEA\' AND [Supplier Complexity Group] = \'Complex CTO\' AND [ASAP Flag] = \'Y\' AND [ITM_COUNT] > 1'
    #dbCondition = '[Profit Center L0 Name] = \'EMEA\' AND [Supplier Complexity Group] = \'CTO\' AND [ASAP Flag] = \'Y\' AND [Plant Code] in (\'G111\', \'G110\', \'8O00\')'
    #dbCondition = '[Profit Center L0 Name] = \'EMEA\' AND [ASAP Flag] = \'Y\' AND [Plant Code] in (\'G111\', \'G110\', \'8O00\')'
    nComponents = 20 

    ssql = ''
    customSSQL = False

    opts, args = getopt.getopt(argv,"hi:o:",["ifile=","ofile="])

    for opt, arg in opts:
        if opt == '-SERVER':
            dbServer = arg
        elif opt == '-DATABASE':
            dbName = arg
        elif opt == '-DBUSER':
            dbUser = arg
        elif opt == '-DBPWD':
            dbPwd = arg
        elif opt == '-COLUMNS':
            dbColumns = arg
        elif opt == '-DBTABLE':
            dbTable = arg
        elif opt == '-CONDITION':
            dbCondition = arg
        elif opt == '-SSQL':
            ssql = arg
            customSSQL = True
        elif opt == '-NCOMPONENTS':
            nComponents = int(opt)
        else:
            print('ARGUMENT ' + opt + ' NOT RECOGNIZED.')
            print('cpxLSA USAGE:')
            print('Options:')
            print('-SERVER \t\t\t\t database server DEFAULT VALUE: ' + dbServer)
            print('-DATABASE \t\t\t\t database name DEFAULT VALUE:'  + dbName)
            print('-DBUSER \t\t\t\t database user')
            print('-DBPWD \t\t\t\t\t database user password')
            print('-COLUMNS \t\t\t\t columns to extract DEFAULT VALUE:'  + dbColumns)
            print('-DBTABLE \t\t\t\t database table to query DEFAULT VALUE: ' + dbTable)
            print('-CONDITION \t\t\t\t WHERE condition to filter data DEFAULT VALUE: ' + dbCondition)
            print('-NCOMPONENTS \t\t\t\t number of components to extract from LSA analysos DEFAULT VALUE: ' + str(nComponents))
            print('-SSQL \t\t\t\t\t custom SQL Statement to extract information')

            sys.exit()

    cnnStr = 'DRIVER={SQL Server};SERVER=' + dbServer +';DATABASE=' + dbName + ';UID=' + dbUser + ';PWD=' + dbPwd
    cnn = pyodbc.connect(cnnStr)

    if (not customSSQL):
        if len(dbCondition) == 0:
            ssql = 'SELECT ' + dbColumns + ' FROM ' + dbTable
        else:
            ssql = 'SELECT ' + dbColumns + ' FROM ' + dbTable + ' WHERE ' + dbCondition

        cmd = cnn.cursor()
        cmd.execute(ssql);

        allDocs = []

    for r in cmd.fetchall():
        allDocs.append([r[0], r[1], r[-1]])

    levels = set([d[1] for d in allDocs])

    componentTitle = []

    for i in range(1, nComponents + 1):
        componentTitle.append('C_' + str(i))

    for lvl in levels:
        documents = [d[-1] for d in allDocs if d[1] == lvl]
        orderIDs = [d[0] for d in allDocs if d[1] == lvl]

        tv = TfidfVectorizer(min_df = 1, tokenizer=lambda x: x.split(' '), stop_words = None, sublinear_tf = True)
        #cv = CountVectorizer(min_df = 1, tokenizer=lambda x: x.split(' '), stop_words = None)
        dtm_tv = tv.fit_transform(documents)

        print(lvl)
        print(dtm_tv.shape)
        print(dtm_tv.count_nonzero())

        #numpy.savetxt('dtm.csv', dtm_tv.toarray(), delimiter=",")

        #dtm_cv = cv.fit_transform(documents)
        print(len(tv.stop_words_))

        # LSA
        lsa = TruncatedSVD(nComponents, algorithm = 'randomized')
        dtm_lsa = lsa.fit_transform(dtm_tv)
        #dtm_lsa = Normalizer(copy = False).fit_transform(dtm_lsa)

        dtm_lsa_df = pandas.DataFrame(dtm_lsa, index = orderIDs, columns = componentTitle)
        dtm_lsa_df.to_csv('LSA_' + str(nComponents) + '_Topics_' + lvl + "_" + 'EMEA' + '.csv')

        tf_feature_names = tv.get_feature_names()
        #print_top_words(lsa, tf_feature_names, 10)

        tpc_lsa_df = pandas.DataFrame(extract_top_words(lsa, tf_feature_names, 10), index = [lvl + str(i) for i in range(0, nComponents)])
        tpc_lsa_df.to_csv('LSA_Topic_Top_Words_' + lvl + '_.csv')

        # LDA
        #lda = LatentDirichletAllocation(n_topics=nComponents, max_iter=20,
        #        learning_method='online',
        #        random_state=0)

        #dtm_lda = lda.fit_transform(dtm_cv)

        #dtm_lda_df = pandas.DataFrame(dtm_lda, index = orderIDs, columns = componentTitle)
        #dtm_lda_df.to_csv('LDA_' + str(nComponents) + '_Topics_' + lvl + "_" + 'EMEA' + '.csv')

        #tf_feature_names = cv.get_feature_names()
        #print_top_words(lda, tf_feature_names, 10)

        #tpc_lda_df = pandas.DataFrame(extract_top_words(lda, tf_feature_names, 10), index = [lvl + str(i) for i in range(0, nComponents)])
        #tpc_lda_df.to_csv('LDA_Topic_Top_Words_' + lvl + '_.csv')

if __name__ == "__main__":
    main(sys.argv[1:])

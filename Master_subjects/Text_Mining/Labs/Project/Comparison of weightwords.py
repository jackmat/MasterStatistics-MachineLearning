# -*- coding: utf-8 -*-
"""
Created on Sat Jan 20 13:04:53 2018

@author: Carles
"""

############WOrds matched 19/01
import numpy as np
##19/01
yesterday=['förvaltningsled', 'python', 'datamodellering', 'warehous', 'excel', 'express', 'apex', 'sql', 'jir', 'analytic', 'configuration', 'pow', 'tableau', 'wcf', 'qlikview', 'integration', 'mongodb', 'bi', 'management', 'ssis', 'mysql', 'vba', 'oracl', 'weblogic', 'dax', 'spss', 'projektledare', 'django', 'ssas', 'microsoft', 'net', 'lösning', 'prediktiv', 'visual']

yesterday.sort()
## 20/01
today = ['förvaltningsled', 'python', 'datamodellering', 'warehous', 'nodej', 'excel', 'express', 'apex', 'sql', 'jir', 'analytic', 'configuration', 'pow', 'tableau', 'nosql', 'dw', 'c', 'wcf', 'qlikview', 'integration', 'mongodb', 'bi', 'management', 'ssis', 'mysql', 'vba', 'oracl', 'weblogic', 'dax', 'spss', 'projektledare', 'django', 'ssas', 'microsoft', 'net', 'projektled', 'lösning', 'prediktiv', 'visual']
today.sort()

list( set(today)-set(yesterday))

['c', 'projektled', 'nosql', 'dw', 'nodej']

newset = ['analytic', 'apex', 'configuration', 'datamodellering', 
                          'dax', 'django', 'excel',  'express',  'förvaltningsled',
                          'integration', 'jir', 'lösning', 'management', 'microsoft', 
                          'mongodb', 'mysql', 'net',  'oracl', 'pow', 
                          'prediktiv', 'projektled', 'python', 'qlikview', 'spss', 'sql',
                          'ssas', 'ssis', 'tableau', 'vba',
                          'visual', 'warehous', 'wcf', 'weblogic', "dw", "nosql", "nodej", "bi"]
newset.sort()




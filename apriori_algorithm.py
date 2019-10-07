# -*- coding: utf-8 -*-
"""
Created on Sun Apr 28 17:55:46 2019

@author: kner7001
"""

############################## ASSOCIATION RULES ##############################
# Apriori algorithm

import numpy as np  
import matplotlib.pyplot as plt  
import pandas as pd  
from apyori import apriori  
import os 

path = 'C:\\Users\\kner7001\\Google Drive\\DSaaS_no_code\\Machine Learning Method Notes\\Apriori Algorithm - Association Rules'

os.chdir(path)

store_data = pd.read_csv('store_data.csv', header=None) 

# use the Apriori algorithm to find out which items are commonly sold together

# This Apriori library we are going to use requires our dataset to be in the 
# form of a list of lists, where the whole dataset is a big list and each 
# transaction in the dataset is an inner list within the outer big list.
records = []  
for i in range(len(store_data)):  
    records.append([str(store_data.values[i,j]) for j in range(0, 20)])
    

# suppose that we want rules for only those items that are purchased at least 5
#  times a day, or 7 x 5 = 35 times in one week, since our dataset is for a
# one-week time period. The support for those items can be calculated as
# 35/7500 = 0.0045. The minimum confidence for the rules is 20% or 0.2. 
# Similarly, we specify the value for lift as 3 and finally min_length is 2
# since we want at least two products in our rules
association_rules = apriori(
                            records,
                            min_support=0.0045,
                            min_confidence=0.2,
                            min_lift=3,
                            min_length=2
                            )  

# convert to list for interpreting
association_results = list(association_rules)  

# see how many total rules it came up with
print(len(association_results)) 

# print the first rule
print(association_results[0]) 
#### Interpret rule results 

# Support = 0.0045. Calculated by dividing the number of transactions 
# containing light cream divided by total number of transactions

# confidence level for the rule is 0.2905 which shows that out of all the 
# transactions that contain light cream, 29.05% of the transactions also 
# contain chicken

# lift of 4.84 tells us that chicken is 4.84 times more likely to be bought by
# the customers who buy light cream compared to the default likelihood of the
# sale of chicken.


# Look at all the association rules
for item in association_results:

    # first index of the inner list
    # Contains base item and add item
    pair = item[0] 
    items = [x for x in pair]
    print("Rule: " + items[0] + " -> " + items[1])

    #second index of the inner list
    print("Support: " + str(item[1]))

    #third index of the list located at 0th
    #of the third index of the inner list

    print("Confidence: " + str(item[2][0][2]))
    print("Lift: " + str(item[2][0][3]))
    print("=====================================")
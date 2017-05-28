#apriori (association rule machine learning) tutorial
groceries <- read.csv("groceries.csv", header=T)
head(groceries)
#we don't want a normal dataframe bc we want market baskets with up to n number of items, so we need to 
#instead form a sparse matrix which is memory efficient bc it only stores the cells that are occupied by an item
#install.packages("arules")
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries) # the 169 columns indicate that there are 169 total items that could be in someone's basket
#the rows is the number of transactions
# 2513/9853 = 0.255 so whole milk shows up in around a quarter of transactions
# a total of 2159 transactions only had 1 item
# looking at the 5 number summary: a quarter of transactions only include 2 items or less and 75% of transactions
# include 6 items or less

inspect(groceries[1:5]) #shows the first 5 transactions

#now look at the proportion of transaction that contain the first three items in the data:
itemFrequency(groceries[,1:3])

#visualise item support (i.e. how frequent the items occur in the dataset)
itemFrequencyPlot(groceries, support=0.1) #we set a threshold here of occuring in 10% of transactions
#alternatively you can limit the plot to the top 20 items of highest support
itemFrequencyPlot(groceries, topN=20)

#plotting the sparse matrix (first 25 transactions) can help identify data issues
#also can be helpful in identifying seasonal effects once sorted by date or other trends if sorted by category
image(groceries[1:25])
#now take a random 100 sample
image(sample(groceries, 100)) # a few columns are heavily populated indicating that there are some items that are very popular

#moving onto training a model on the dataset
#try default support of 0.1 and confidence 0.8
apriori(groceries) #uh oh it gives us 0 rules but it makes sense bc only 8 items occurred in 10% of the transactions

#widening our net for rules
# thinking about when we'd find a pattern interesting...let's start with an item purchased 2x a day (60 times a month)
# so 60 / total transactions of 9835 = 0.006
#also lowering confidence threshold to 0.25 in other words, the rule has to be correct 25% of the time
# also include minlen = 2 which eliminates rules that contain < 2 items bc they are uninteresting and just show items that are frequently purchased
groceryrules <- apriori(groceries, parameter = list(support = 
                                                      0.006, confidence = 0.25, minlen = 2))
groceryrules # contains a set of 463 rules

#Evaluating model performance
summary(groceryrules)
#150 rules have only 2 items, 297 have 3 items and 16 rules have 4 items.
#lift values > 1 indicate that it the items in the rule are found together more than by chance. Therefore, 
#a high lift indicates that that rule is particularly strong / a true connection between the items.

#look at the first 3 rules created:
inspect(groceryrules[1:3,])
#If a customer buys potted plants, they will buy whole milk - covers 0.7% of the transactions and is correct 40%
#of purchases containing potted plants.
#but is this rule useful? 
#next section will sort and select the learned rules to find the most actionable ones

#Improving model performance:
#sort grocery rules from highest lift
inspect(sort(groceryrules, by = "lift") [1:5])
#people who buy berries are almost 4x as likely to buy whipped cream then other people. But what other items are 
#also bought with berries?

#filter rules for all those that contain berries
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)
#see page 282 for additional options to make sorting and filtering as specific or general as you want.

#finally save learned rules as a csv
write(groceryrules, file="groceryrules.csv", 
      sep = ",", quote=TRUE, row.names=FALSE)

#you can also save them as a df with rules as a factor and other numeric
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

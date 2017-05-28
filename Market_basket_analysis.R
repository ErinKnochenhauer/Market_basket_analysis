#ecommerce order analysis
#load in data
aisles <- read.csv("aisles.csv", header=T)
head(aisles)
departments <- read.csv("departments.csv", header=T)
head(departments)
order_prod_prior <- read.csv("order_products__prior.csv", header=T)
order_prod_train <- read.csv("order_products__train.csv", header=T)
head(order_prod_prior)
head(order_prod_train)
orders <- read.csv("orders.csv", header=T)
head(orders)
products <- read.csv("products.csv", header=T)
head(products)

#####load in packages
library(plyr)
library(dplyr)
library(tidyverse)

######merging datasets
#joining by aisle_id
products %>%
  full_join(aisles) -> prod_aisles
head(prod_aisles)
#joining with dept
prod_aisles %>% 
  full_join(departments) -> store
rm(aisles)
rm(departments)
rm(products)
rm(prod_aisles)
orders %>% 
  full_join(order_prod_prior) -> order
head(order)
rm(orders)
rm(order_prod_prior)
#finally merge product attributes / store location with orders
order %>%
  full_join(store) -> instacart
head(instacart)
rm(order)
rm(store)

#####rename/recode variables
instacart <- rename(instacart, order_seq = order_number)
head(instacart)
instacart$order_dow <- factor(instacart$order_dow,
                              levels = c(0, 1, 2, 3, 4, 5, 6),
                              labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
head(instacart)
#write.csv(instacart, file="instacart.csv")

#since my computer is slow with a large sample, read in just the first 1 mill obs
instacart <- read.csv("instacart.csv", header=T, nrow=1000000)

#####check for missing values
library(Amelia)
sum(complete.cases(instacart)) 
#visualising missing values
missmap(instacart, main= "Missing values vs observed") #mostly red which is good
#dropping missing values where product ID is missing
library(tidyr)
instacart %>% drop_na(product_id) -> instacart1
#check missing values again
missmap(instacart1) #looks good! the only missing vals are the prior order ones 
write.csv(instacart1, file="instacart1.csv")

#####descriptive stats to get an idea of the dataset
#number of orders by day of the week
plot(instacart1$order_dow)
instacart1$order_id <- as.factor(instacart1$order_id)
length(unique(instacart1$order_id)) #there are 99791 unique transactions
length(unique(instacart1$user_id)) #there are 6433 unique customers in the dataset

#what is the mean number of items customers buy?
#need to group by order_id 
instacart1 %>% group_by(order_id) %>% 
  summarise(ordernum = length(order_id)) -> ordernum
head(ordernum)
median(ordernum$ordernum) #the median number of items people purchase is 8
count <- table(ordernum$ordernum)
barplot(count, main="Distribution of order quantities across categories")

#then you can look at median number of orders for each dow

#Monday orders
instacart1 %>% group_by(order_id) %>%
  filter(order_dow == "Mon") %>% 
  summarise(monorders=length(order_id)) -> monorders
head(monorders)
mean(monorders$monorders) #mean number of items bought on Mondays is about 11
median(monorders$monorders)
#Tuesday orders
instacart1 %>% group_by(order_id) %>% 
  filter(order_dow == "Tues") %>%
  summarise(tuesorders=length(order_id)) -> tuesorders
mean(tuesorders$tuesorders) #mean number of items bought Tues is about 10
median(tuesorders$tuesorders)
#Wed orders
instacart1 %>% group_by(order_id) %>% 
  filter(order_dow == "Wed") %>%
  summarise(wedorders=length(order_id)) -> wedorders
mean(wedorders$wedorders) #mean number of items bought on weds is 9.4 
median(wedorders$wedorders)
#Thurs
instacart1 %>% group_by(order_id) %>% 
  filter(order_dow == "Thurs") %>%
  summarise(thursorders=length(order_id)) -> thursorders
mean(thursorders$thursorders) #mean number of items bought on thurs is 9.1
median(thursorders$thursorders)
#Fri orders
instacart1 %>% group_by(order_id) %>% 
  filter(order_dow == "Fri") %>%
  summarise(friorders=length(order_id)) -> friorders
mean(friorders$friorders) #mean number of items bought on fri is 9.4
median(friorders$friorders)
#Sat orders
instacart1 %>% group_by(order_id) %>% 
  filter(order_dow == "Sat") %>%
  summarise(satorders=length(order_id)) -> satorders
mean(satorders$satorders) #mean number of items bought on weds is 9.8
median(satorders$satorders)

#Sun orders
instacart1 %>% group_by(order_id) %>% 
  filter(order_dow == "Sun") %>%
  summarise(sunorders=length(order_id)) -> sunorders
mean(sunorders$sunorders) #mean number of items bought on sun is 10.7
median(sunorders$sunorders)

#what products are staples? i.e. which are usually put in the basket first or second?
instacart1 %>% filter(add_to_cart_order==1 | add_to_cart_order==2) -> staples
figure <- table(staples$department, exclude= "missing")
barplot(figure)
count(staples, vars=product_id) -> mostcommmonstaple
arrange(mostcommmonstaple, desc(n))
#top 8 most ordered staples with over 1000 times placed in basket first or second
#1  24852  5561  # banana
#2  13176  4464  # Bag of Organic Bananas
#3  21137  1789  # Organic Strawberries
#4  27845  1751  # Organic Whole Milk
#5  47209  1715  # Organic Hass Avocado
#6  21903  1526  # Organic Baby Spinach
#7  47766  1323  # Organic Avocado
#8  27966  1018  # Organic Raspberries

#what is the difference in product penetration of regular bananas compared to organic?
#finding total number of unique customers who buy bananas category
filter(instacart1, product_id == 24852 | product_id == 13176) -> banana
dim(banana) # 25273 observations
#but need to find the number of unique customers:
banana %>% group_by(user_id) %>%
  summarise(bananabuyers=length(unique(user_id))) -> bananabuyers
head(bananabuyers) # there are 3605 total unique customers who buy banana products

#finding the number of customers who buy just organic bananas
filter(banana, product_id == 13176) %>% group_by(user_id) %>%
  summarise(organicbuyers=length(unique(user_id))) -> organicbuyers # 1965 unique customers buy organic bananas
1965/3605
#Organic bananas have a product penetration of 55% and regular bananas 45%

#number of orders by department
figure1 <- table(instacart1$department, exclude="missing") 
barplot(figure1) #highest dept sales: produce, dairy eggs, snacks, beverages, breakfast, bakery 

#visualise the distribution of time between reorders across product categories
filter(instacart1, reordered == 1) -> reordered
counts <- table(reordered$days_since_prior_order)
barplot(counts, main= "Days between prior order", 
        xlab="Number of days") #variable skewed to the right with lots of products reordered after
#30 days though the majority are reordered on a weekly basis (so reporting the median days)
median(reordered$days_since_prior_order) #7 days

#seeing which aisles are reordered most frequently and least frequently
filter(instacart1, reordered == 1) %>% group_by(aisle) %>%
  summarise(reordertime=median(days_since_prior_order)) -> reorderedtime
head(reorderedtime)
arrange(reorderedtime, desc(reordertime))
#frozen juice, eye/ear care, icecream toppings, facial care, frozen meals/pizza, laundry and trash bags are least frequently purchased
arrange(reorderedtime, reordertime)
#<fctr>       <dbl> #more frequently than average reordered time product categories 
#1              baby accessories           6
#2                 beers coolers           6
#3  bulk dried fruits vegetables           6
#4              cold flu allergy           6
#5              kitchen supplies           6
#6                      mint gum           6
#7                more household           6
#8                     red wines           6
#9          vitamins supplements           6
#10                  white wines           6
median(reorderedtime$reordertime) #median number of days across categories is 7


#apriori algorithm for association rule learning
names(instacart1)
head(instacart1)
library(dplyr)
apriori <- select(instacart1, order_id, product_name)
head(apriori)

apriori %>% group_by(order_id) -> i
with(i, aggregate(product_name, list(order_id), FUN = function(product_name) c(as.character(product_name)))) -> i2
i2[1,]
library(arules)

x <- as.character(unique(apriori$order_id))
y <- as.character(i2$x)
head(y)
i3 <- data.frame(x,y)
head(i3)
write.csv(i3, file="newdata.csv")

#library(splitstackshape)
#i3$y <- cSplit(i3, "y", ",") 

i3 <- read.transactions("newdata.csv", sep = ",")
summary(i3)
#bananas are part of 12.6% of transactions (12525/99792 = 0.1255)
#organic bananas 10.5% of transactions
#organic strawberries 7%
#ogranic baby spinach and organic hass acocado about 6.3%
#25% of transactions had 6 items or less, 75% of transactions had 14 items or less with a mean of about 10 items.
#look at the first 5 transactions
inspect(i3[1:5])
itemFrequency(i3[,1:5]) #look at how often the first 5 items occur in the dataset
itemFrequencyPlot(i3, support=0.05) #barplot of items occuring in at least 5% of transactions

#visualise frequently bought items and look for data issues
library(ggplot2)
image(i3[1:25])
#now take a random 100 sample
image(sample(i3, 100))

#top 20 items that are most frequently purchased
itemFrequencyPlot(i3, topN=20)

#lift values > 1 indicate that it the items in the rule are found together more than by chance. Therefore, 
#a high lift indicates that that rule is particularly strong / a true connection between the items.
rules2 <- apriori(i3, parameter = list(support = 
                                         0.0025, confidence = 0.33, minlen = 2)) 
inspect(sort(rules2, by = "lift")) #support too high

rules <- apriori(i3, parameter = list(support=
                                        0.001, confidence = 0.25, minlen=2))
inspect(sort(rules, by = "lift"))

#save rules as a csv
write(rules, file="finalrules.csv", 
      sep = ",", quote=TRUE, row.names=FALSE)
